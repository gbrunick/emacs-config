;;
;; This file provides a mechanism to manage a pool of remote shell
;; processes.  This can be helpful when working on remote machines if it
;; takes a while for TRAMP to spin up a new process.
;;
;; The main function is `prat-async-shell-command' which is used to
;; implement the `prat-shell-command'.  The later function is a potential
;; replacement for the interactive use of `shell-command'.
;;

(defvar prat-idle-time (* 5 60)
  "The amout of time a server process waits for the next command.")

(defvar prat-available-workers nil
  "An alist of TRAMP prefixes and buffers.

Each buffer contains a Bash or cmd.exe processes running on the
machine associated with the TRAMP prefix.  An empty string
denotes the local machine.")

(defvar prat-all-workers nil
  "A list of buffer that contain worker processes.")

(defvar prat-output-marker "ae6c3bc5-cf99-48c1-840d-6662f718d384"
  "Arbitrary string that is used to mark locations within process output.

This string should be something that is unlikely to show up in
any command output and doesn't require any shell quoting.")

(defvar prat-output-start (format "START:%s" prat-output-marker))
(defvar prat-output-end (format "END:%s" prat-output-marker))

(defvar prat-git-state-change-count 0
  "Incremented each time a command that satisfies the
`prat-command-changes-git-state-p' predicate is run.")

(define-derived-mode prat-server-mode special-mode
  "Prat-Server"
  "\nMode for buffers containg server processes."
  (setq truncate-lines t)
  (setq-local kill-buffer-query-functions nil))

(defun prat-async-shell-command (cmd &optional callback env-vars no-check)
  "Execute CMD and call CALLBACK as output becomes available.

CMD is a string that is passed through to a Bash or Windows cmd process.
This string must be properly quoted by the caller.  CALLBACK is a function
that accepts (BUF START END COMPLETE).  It is called in the buffer which
was active when `prat-async-shell-command' was called as output becomes
available.  If that buffer has been killed, CALLBACK is not called.

CALLBACK is called as output is read from the worker process.
The buffer containing process output is passed to CALLBACK as
BUF.  START and END give the region that contains new output and
COMPLETE is initially nil.  The new output only contains complete
lines (i.e., newline terminated).  After all lines have been
processed, the callback is called a final time with COMPLETE equal
to t.  In this case, START and END delimit all process output.
In particular, callers that don't want to process the output in
real-time, can wait for COMPLETE and then process all output
between START and END.

If ENV-VARS is provided, it should be a list of strings of the
form VARNAME=VALUE and these variables are set in the shell
before CMD is run.

We normally check the return code produced by CMD and show a buffer
containing the process output if CMD failed, but this check is omitted if
NO-CHECK is non-nil."
  (let* ((marker (copy-marker (point)))
         (server-buf (prat-get-server-buf))
         (local-dir (file-local-name default-directory))
         (proc (get-buffer-process server-buf))
         (search-start (marker-position (process-mark proc))))

    (with-current-buffer server-buf
      ;; Pass data to `prat-async-shell-command-1'.
      (setq-local prat-asc-data `( :cmd ,cmd
                                   :marker ,marker
                                   :callback ,callback
                                   :search-start ,search-start
                                   :no-check ,no-check))

      (set-process-filter proc #'prat-async-shell-command-1)

      ;; Insert any extra config settings from `prat-extra-git-config'
      (when (and prat-extra-git-config (string-match "^git " cmd))
        (let* ((replacement (format "git%s "
                                    (mapconcat (lambda (var-val)
                                                 (format " -c %s" var-val))
                                               prat-extra-git-config))))
          (setq cmd (replace-regexp-in-string "^git " replacement cmd))))

      (cond
       ;; Windows
       ((prat-use-cmd-exe-p)
        (process-send-string proc "setlocal\n")
        ;; Set any environment variables
        (dolist (def env-vars)
          (process-send-string proc (format "set %s\n" def)))
        ;; echo includes any trailing space when called from cmd.exe.
        (process-send-string
         proc (concat (format "(echo:& echo %s& %s)" prat-output-start cmd)
                      (format "&& (echo:& echo %s:SUCCESS)" prat-output-end)
                      (format "|| (echo:& echo %s:FAIL)\n" prat-output-end)))
        (process-send-string proc "endlocal\n"))

       ;; Linux
       (t
        ;; We start a new process to isolate commands.
        (process-send-string proc "bash --noediting --noprofile --norc\n")
        (process-send-string proc "stty -echo\n")
        ;; Hide prompts.
        (process-send-string proc "PS1=\n")
        (process-send-string proc "PS2=\n")
        ;; Don't save command history
        (process-send-string proc "unset HISTFILE\n")
        ;; Set any environment variables
        (dolist (def env-vars)
          (process-send-string proc (format "export %s\n" def)))
        (process-send-string
         proc (concat (format "(echo ; echo %s ; %s)" prat-output-start cmd)
                      (format "&& (echo ; echo %s:SUCCESS)" prat-output-end)
                      (format "|| (echo ; echo %s:FAIL)\n" prat-output-end)))
        (process-send-string proc "exit\n")))

      proc)))


(defun prat-async-shell-command-1 (proc string)
  "Process filter for `prat-async-shell-command'."
  (when (buffer-live-p (process-buffer proc))
    (let* ((proc-buf (process-buffer proc))
           (data (with-current-buffer proc-buf prat-asc-data))
           ;; `prat-asc-data' is set in `prat-async-shell-command'
           (cmd (plist-get data :cmd))
           (callback-marker (plist-get data :marker))
           (callback (plist-get data :callback))
           (no-check (plist-get data :no-check))
           (callback-buffer (marker-buffer callback-marker))
           ;; The search for `start-regex' starts at `search-start'.
           (search-start (plist-get data :search-start))

           (start-regex (format "^%s" prat-output-start))
           (end-regex (format "^%s" prat-output-end))
           ;; `start-marker' and `end-marker' will hold the matches to the

           ;; regexes.
           start-marker end-marker
           ;; `output-start' and `output-end' will delimit complete lines.
           output-start output-end
           (inhibit-read-only t))

      (with-current-buffer proc-buf
        (save-excursion
          ;; Insert `string`, advance the process marker, and record the
          ;; beginning and end of the inserted text as `output-start` and
          ;; `output-end`.
          (goto-char (process-mark proc))
          (setq output-start (pos-bol))
          (insert string)
          (set-marker (process-mark proc) (point))
          (comint-carriage-motion output-start (point))
          (setq output-end (save-excursion (forward-line 0) (point)))

          ;; Look for the special output markers.
          (setq start-marker (and
                              (progn
                                (goto-char search-start)
                                (re-search-forward start-regex nil t))
                              (progn
                                (forward-line 1)
                                (setq output-start (max output-start (point)))
                                (point))))

          (when start-marker
            (setq end-marker (and
                              (progn
                                (goto-char output-start)
                                (re-search-forward end-regex nil t))
                              (progn
                                (forward-line 0)
                                ;; We insert a newline before the output to
                                ;; ensure a match at the start of the line,
                                ;; but this shouldn't be included in the
                                ;; process output.  It can matter when
                                ;; generating diffs.
                                (skip-chars-backward "\n" (1- (point)))
                                (setq output-end (point))
                                (point)))))

          (when (and start-marker callback)
            (when (> output-end output-start)
              (if (equal callback t)
                  (let ((text (buffer-substring-no-properties
                               output-start output-end)))
                    (with-current-buffer callback-buffer
                      (save-excursion
                        (goto-char callback-marker)
                        (insert text)
                        (set-marker callback-marker (point))
                        (set-buffer-modified-p nil))))
                (when callback
                  (prat-call-callback callback-buffer callback
                                      proc-buf output-start output-end nil)))))

          (when end-marker
             ;; If there was a callback, call with `complete' set to true.
            (when callback
              (with-current-buffer callback-buffer
                (prat-call-callback callback-buffer callback
                                    proc-buf start-marker end-marker t)))

             ;; Show an error buffer if the command failed unless
             ;; no-check is non-nil.
             (unless no-check
              (save-excursion
                (goto-char end-marker)
                (let ((regex (format "%s:FAIL" prat-output-end))
                      (proc-output (buffer-substring start-marker end-marker)))
                  (when (re-search-forward regex nil t)
                    (with-current-buffer (get-buffer-create "*Command Error*")
                      (erase-buffer)
                      (prat-base-mode)
                      (insert (format "> %s\n\n" cmd))
                      (save-excursion (insert proc-output))
                      (switch-to-buffer (current-buffer)))
                    (message "Error during '%s'" cmd)))))

             ;; If we just completed a command that changes the Git state,
             ;; we increment `prat-git-state-change-count' and schedule a
             ;; refresh for status and log buffers.
             (when (prat-command-changes-git-state-p cmd)
               (cl-incf prat-git-state-change-count)
               (run-with-timer 0 nil #'prat-refresh-all-buffers))

             (prat-return-or-kill-buffer proc-buf)))))))


(defun prat-get-server-buf (&optional dir)
  "Get a buffer containing a live Bash or Windows cmd.exe process.

Attempts to pop a process running on the correct machine from
`prat-available-workers'and sets the current directory to DIR.  Spins
up a new process if nothing is available in the worker pool.  The
caller is responsible for returning the buffer by calling
`prat-return-buffer' when done."
  (let* ((dir (expand-file-name (or dir default-directory)))
         (local-dir (directory-file-name
                     (file-local-name (expand-file-name dir))))
         (tramp-prefix (file-remote-p dir))
         (key-value (assoc tramp-prefix prat-available-workers))
         (inhibit-read-only t)
         buf proc)

    ;; If we have a TRAMP `dir', try to pull a worker from the pool.
    (while (not (null key-value))
      ;; Remove a buffer from prat-available-workers
      (setq buf (cdr key-value))
      (setq prat-available-workers
            (rassq-delete-all buf prat-available-workers))

      (cond
       ;; If the process is still alive, change the working directory,
       ;; prepare buffer for the next command and break the while loop.
       ((prat-reset-buffer buf dir)
        (setq key-value nil))

       ;; Otherwise, kill the buffer and try to pull another buffer from
       ;; the pool.
       (t
        (kill-buffer buf)
        (setq prat-all-workers (delq buf prat-all-workers)
              key-value (assoc tramp-prefix prat-available-workers)
              buf nil))))

    ;; We couldn't reuse an existing server buffer, so we spin up a new
    ;; process.
    (unless buf
      (setq buf (generate-new-buffer
                 (if tramp-prefix
                     (format "*shell on %s*" tramp-prefix)
                   "*local shell*")))

      ;; `prat-all-workers' is used by `prat-kill-all-workers'.
      (push buf prat-all-workers)

      (with-current-buffer buf
        (prat-server-mode)
        ;; `start-file-process' looks at `default-directory'.
        (setq default-directory dir)
        (cond
         ;; Windows
         ((prat-use-cmd-exe-p dir)
          (setq proc (start-file-process (buffer-name buf) buf "cmd")))
         ;; Linux
         (t
          (setq proc (start-file-process (buffer-name buf) buf
                                         "bash" "--noediting"
                                         "--noprofile" "--norc"))
          ;; No prompts; it just makes the output more non-deterministic.
          (process-send-string proc "PS1=\n")
          (process-send-string proc "PS2=\n")
          ;; Don't save command history
          (process-send-string proc "unset HISTFILE\n")))))

    buf))


(defun prat-return-or-kill-buffer (buf)
  "Return BUF to the worker pool or kill it."
  (with-current-buffer buf
    (let ((tramp-prefix (file-remote-p default-directory))
          (proc (get-buffer-process buf))
          (msg "Waiting for the next command... "))
      (cond
       ;; If we have a live remote process, try to reuse it.
       ((and tramp-prefix (process-live-p proc))
        ;; Reset the sentinel and filter before returning to the pool.
        (set-process-sentinel proc nil)
        (set-process-filter proc nil)
        ;; We now arrange for the process to terminate if it doesn't
        ;; receive another request in the next `prat-idle-time' seconds.
        (process-send-string
         proc (format "read -t %s -p \"%s\" || exit 0\n"
                      prat-idle-time msg))
        ;; Add back to the worker pool.
        (push `(,tramp-prefix . ,buf) prat-available-workers))

       ;; Otherwise, kill the buffer.
       (t
        (kill-buffer buf))))))


(defun prat-use-cmd-exe-p (&optional dir)
  "Should we use cmd.exe rather than bash?

We use cmd.exe when we are on the local filesystem of a Windows
machine.  In all other cases (i.e., not on Windows machine or on
a Windows machine but working remotely via TRAMP) we use bash."
  (let ((dir (or dir default-directory)))
    (and (eq window-system 'w32)
         (ignore-errors (not (file-remote-p default-directory))))))


(defun prat-reset-buffer (buf dir)
  "Prepare `buf' for the next command.

Sets working directory to DIR.  Return t on success and nil
otherwise."
  (let* ((proc (get-buffer-process buf)))
    (when (and proc (process-live-p proc))
      (with-current-buffer buf
        ;; After a request, the server process is waiting to read some
        ;; inputs, so we send an initial string to be ignored.
        (process-send-string proc "for read command\n")
        (process-send-string proc (format "cd \"%s\"\n" local-dir))
        (process-send-string proc "echo\n")
        (process-send-string proc (format "echo cwd: $(pwd)\n")))
      t)))


(defun prat-kill-all-workers ()
  "Kill all worker process buffers"
  (interactive)
  (dolist (buf prat-all-workers)
    (when (buffer-live-p buf)
      (kill-buffer buf)
      (message "Killed buffer %s" buf)))
  (setq prat-all-workers nil))


(defun prat-call-callback (buf callback &rest args)
  (unless (with-local-quit
            (when (buffer-live-p buf)
              (with-current-buffer buf
                (apply callback args)))
            t)
    (message "Quit during %S %S" buf `(,callback ,@args))))


(defun prat-command-changes-git-state-p (cmd)
  (let* ((subcommands '("add" "apply" "branch" "cherry-pick" "commit"
                        "merge" "push" "rebase" "reset" "revert")))
    (and
     (catch 'result

       ;; Look for a subcommand
       (dolist (subcmd subcommands)
         (when (string-match (format "^git +%s" subcmd) cmd)
           (throw 'result t)))


       ;; Not all stash variants change the state.
       (or (string-match "^git stash\\( -u\\)?\\( -- .*\\)?$" cmd)
           (string-match "^git stash \\(apply\\|pop\\|drop\\)?$" cmd)))

     ;; Return nil or t.
     t)))


(provide 'prat-async-shell-command)
