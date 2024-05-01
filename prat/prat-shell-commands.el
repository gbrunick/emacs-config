;;
;; This file provides a mechanism to manage a pool of remote shell
;; processes.  This can be helpful when working on remote machines if it
;; takes a while for TRAMP to spin up a new process.
;;
;; The main function is `prat-async-shell-command' which is used to
;; implement the `prat-shell-command'.  The later function is a potential
;; replacement for the interactive use of `shell-command'.
;;

(defvar prat-debug t
  "When non-nil, we write tracing info into `prat-debug-buffer-name'.")

(defvar prat-debug-buffer-name "*Prat Debug*"
  "The name of the buffer used to hold tracing information.")

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

(define-derived-mode prat-server-mode special-mode
  "Prat-Server"
  "\nMode for buffers containg `shpool' server processes."
  (setq truncate-lines t)
  (setq-local kill-buffer-query-functions nil))

(defvar prat-command-output-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "!" 'prat-shell-command)
    (define-key map "q" 'bury-buffer)
    map))

(define-derived-mode prat-command-output-mode special-mode "Git Output")

(defun prat-async-shell-command (cmd &optional dir callback env-vars)
  "Execute CMD in DIR and call CALLBACK as output becomes available.

CMD is a string that is passed through to a Bash or Windows cmd
process.  This string must be properly quoted by the caller.  DIR
is a string giving the working directory in which the command is
executed.  Remote TRAMP directories are supported.  CALLBACK is a
function that accepts (BUF START END COMPLETE).  It is called in
the buffer which was active when `prat-async-shell-command' was
called as output become available.  If that buffer is no longer
alive, CALLBACK is not called.

CALLBACK is called as output is read from the worker process.
The buffer containing process output is passed to CALLBACK as
BUF.  START and END give the region that contains new output and
COMPLETE is initially nil.  The new output only contains complete
lines (i.e., newline terminated).  After all lines have been
process, the callback is called a final time with COMPLETE equal
to t.  In this case, START and END delimit all process output.
In particular, callers that don't want to process the output in
real-time, can wait for COMPLETE and then process all output
between START and END.

If ENV-VARS is provided, it should be is a list of strings of the
form VARNAME=VALUE and these variables are set in the shell
before CMD is run."
  (prat-trace-call)
  (let* ((buf (current-buffer))
         (dir (or dir default-directory))
         (server-buf (prat-get-server-buf dir))
         (local-dir (file-local-name default-directory))
         proc)

    (with-current-buffer server-buf
      ;; Set variables for `prat-async-shell-command--process-filter'.
      (setq proc (get-buffer-process (current-buffer)))
      (setq-local callback-buf buf)
      (setq-local callback-func callback)
      ;; The searches in the process filter start at `proc-start'.
      (setq-local proc-start (marker-position (process-mark proc)))
      (set-process-filter proc #'prat-async-shell-command--process-filter)

      ;; We start a new process to isolate commands.
      (cond
       ;; Windows
       ((prat-use-cmd-exe-p dir)
        (process-send-string proc "cmd\n")
        ;; Set any environment variables
        (dolist (def env-vars)
          (process-send-string proc (format "set %s\n" def)))
        (process-send-string
         proc (format "echo:& echo %s& %s & echo:& echo %s\n"
                      prat-output-start cmd prat-output-end))
        (process-send-string proc "exit\n"))

       ;; Linux
       (t
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
         proc (format "echo ; echo %s ; %s ; echo ; echo %s\n"
                      prat-output-start cmd prat-output-end))
        (process-send-string proc "exit\n")))

      proc)))


(defun prat-async-shell-command--process-filter (proc string)
  "Process filter for `prat-async-shell-command'."
  (prat-trace-call)
  (when (buffer-live-p (process-buffer proc))
    (let ((proc-buf (process-buffer proc))
          ;; echo includes the trailing space when called from cmd.exe.
          (start-regex (format "^%s" prat-output-start))
          (end-regex (format "^%s" prat-output-end))
          ;; `start-marker' and `end-marker' are the matches to the regexs.
          start-marker end-marker
          ;; `output-start' and `output-end' will delimit complete lines.
          output-start output-end
          ;; We set `complete' to t when we see end of output marker.
          complete
          (inhibit-read-only t))

      (with-current-buffer proc-buf
        (save-excursion
          ;; Insert `string`, advance the process marker, and record the
          ;; beginning and end of the inserted text as `output-start` and
          ;; `output-end`.
          (goto-char (process-mark proc))
          (setq output-start (save-excursion (forward-line 0) (point)))
          (insert string)
          (set-marker (process-mark proc) (point))
          (setq output-end (save-excursion (forward-line 0) (point)))

          ;; Look for the special output markers.
          (setq start-marker (and
                              (progn
                                (goto-char proc-start)
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
                                ;; but we this shouldn't be included in the
                                ;; process output.  It can matter when
                                ;; generating diffs.
                                (skip-chars-backward "\n" (1- (point)))
                                (setq output-end (point))
                                (point)))))

          (when (and start-marker callback-func)
            ;; `callback-func' is buffer-local, so we need to save it
            ;; before we switch buffers.
            (let ((f callback-func))
              (with-current-buffer callback-buf
                (when (> output-end output-start)
                  (funcall f proc-buf output-start output-end nil))
                (when end-marker
                  (funcall f proc-buf start-marker end-marker t)
                  (prat-return-or-kill-buffer proc-buf))))))))))


(defun prat-shell-command (cmd &optional bufname env-vars dir)
  "Execute CMD in a new buffer and pop to that buffer.

CMD is a string that is passed through to an interactive bash or
cmd.exe process.  BUFNAME is a string giving the name of the
buffer in which the results are written.  Any current contents
are deleted.  If ENV-VARS is provided, it is a list of strings of
the form VARNAME=VALUE.  In this case, we run CMD in an inferior
shell in which these environment variables have been set."
  (interactive "sShell Command: ")
  (prat-trace-call)
  (let* ((dir (expand-file-name (or dir default-directory)))
         (buf (get-buffer-create (or bufname "*Shell Command Output*")))
         (inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer)
      (fundamental-mode)
      (setq-local default-directory dir)
      (setq-local mode-line-process ":running")
      (insert (format "Working Directory: %s\n\n" default-directory))
      (when env-vars
        (insert "Environment Variables:\n")
        (insert (mapconcat 'identity env-vars "\n"))
        (insert "\n\n"))
      (insert (format "> %s\n\n" cmd))
      (save-excursion (insert "..."))
      (setq-local output-marker (copy-marker (point) t))
      (goto-char (point-min))
      (prat-async-shell-command cmd dir #'prat-shell-command-1 env-vars))
    (pop-to-buffer buf)
    buf))


(defun prat-shell-command-1 (buf start end complete)
  (prat-trace-call)
  (cond
   (complete
    ;; Delete the ellipsis
    (delete-region output-marker (point-max))
    ;; (save-excursion (goto-char output-marker) (insert "Done.\n"))
    (setq mode-line-process ":complete"))

   (t
    (let ((new-output (with-current-buffer buf
                        (string-trim-right
                         (buffer-substring-no-properties start end))))
          (inhibit-read-only t)
          (output-start (marker-position output-marker))
          (move-pt (= (point) output-marker)))

      ;; Insert the new text and fix up the display.
      (unless (string-empty-p new-output)
        (save-excursion
          ;; The insertion moves `output-marker'
          (goto-char output-marker)
          (insert new-output)
          (insert "\n")

          ;; Delete output that was overwritten using carriage returns and
          ;; process color codes.
          (goto-char output-start)
          (while (re-search-forward "^[^]*" nil t)
            (delete-region (match-beginning 0) (match-end 0)))
          (ansi-color-apply-on-region output-start output-marker))

        (when move-pt (goto-char output-marker)))))))


(defun prat-get-server-buf (&optional dir)
  "Get a buffer containing a live Bash or Windows cmd.exe process.

Attempts to pop a process running on the correct machine from
`prat-available-workers'and sets the current directory to DIR.  Spins
up a new process if nothing is available in the worker pool.  The
caller is responsible for returning the buffer by calling
`prat-return-buffer' when done."
  (prat-trace-call)
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
          (setq proc (start-file-process "cmd-server" buf "cmd")))
         ;; Linux
         (t
          (setq proc (start-file-process "bash-server" buf "bash" "--noediting"
                                         "--noprofile" "--norc"))
          ;; No prompts; it just makes the output more non-deterministic.
          (process-send-string proc "PS1=\n")
          (process-send-string proc "PS2=\n")
          ;; Don't save command history
          (process-send-string proc "unset HISTFILE\n")
          (accept-process-output proc nil nil t)))))

    buf))


(defun prat-return-or-kill-buffer (buf)
  "Return BUF to the worker pool or kill it."
  (prat-trace-call)
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

      ;; Otherwise, we kill the buffer unless we are debugging.
      ((not prat-debug) (kill-buffer buf))))))


(defun prat-use-cmd-exe-p (&optional dir)
  "Should we use cmd.exe rather than bash?

We use cmd.exe when we are on the local filesystem of a Windows
machine.  In all other cases (i.e., not on Windows machine or on
a Windows machine but working remotely via TRAMP) we use bash."
  (let ((dir (or dir default-directory)))
    (and (eq window-system 'w32)
         (ignore-errors (not (file-remote-p default-directory))))))


(defun prat-trace-call (&optional func args n)
  "Write tracing info prat-debug-buffer-name'.

The argument N gives the number of additional step to skip."
  (when prat-debug
    (let* ((n (or n 0))
           (buf (current-buffer))
           (bufname prat-debug-buffer-name)
           ;; If the nesting changes, the NFRAMES may change.
           (outer-call (backtrace-frame (+ 5 n)))
           (func (cadr outer-call))
           (args (cddr outer-call)))
      (with-current-buffer (get-buffer-create bufname)
        (setq truncate-lines t)
        (let ((args-string (mapconcat (lambda (y)
                                        (truncate-string-to-width
                                         (prin1-to-string y) 1000 nil nil t))
                                      args "\n  ")))
          (save-excursion
            (goto-char (point-max))
            (unless (bobp) (insert "\n"))
            (insert (format "%S called in buffer %S\n  %s\n"
                            func (buffer-name buf) args-string))))))))


(defun prat-reset-buffer (buf dir)
  "Prepare `buf' for the next command.

Sets working directory to DIR.  Return t on success and nil
otherwise."
  (prat-trace-call)
  (let* ((proc (get-buffer-process buf)))
    (when (and proc (process-live-p proc))
      (accept-process-output nil 0)
      (with-current-buffer buf
        ;; After a request, the server process is waiting to read some
        ;; inputs, so we send an initial string to be ignored.
        (process-send-string proc "for read command\n")
        (process-send-string proc (format "cd \"%s\"\n" local-dir))
        (process-send-string proc "echo\n")
        (process-send-string proc (format "echo cwd: $(pwd)\n"))
        (accept-process-output proc))
      t)))


(defun prat-kill-all-workers ()
  "Kill all worker process buffers"
  (interactive)
  (dolist (buf prat-all-workers)
    (when (buffer-live-p buf)
      (kill-buffer buf)
      (message "Killed buffer %s" buf)))
  (setq prat-all-workers nil))


(provide 'prat-shell-commands)

