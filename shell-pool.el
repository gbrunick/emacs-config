;;
;; Shell Pool
;;
;; This package provides a mechanism to start a pool of shell processes and
;; use them to execute commmands and scripts.  This can be particularly
;; helpful when working on remote machines if it takes a while for TRAMP to
;; spin up a new process.
;;
;; The main function is `shpool-async-shell-command' which is use to
;; implement the `shpool-shell-command' which provides a potential
;; replacement for the interactive use of `shell-command'.
;;

(defvar shpool-show-tracing-info t
  "When non-nil, we write tracing info into `shpool-tracing-buffer-name'.")

(defvar shpool-tracing-buffer-name "*Shell Pool Tracing*"
  "The name of the buffer used to hold tracing information.")

(defvar shpool-worker-pool nil
  "An alist of TRAMP prefixes and buffers.

Each buffer contains a bash or cmd.exe processes running on the
machine associated with the TRAMP prefix.  An empty string
denotes the local machine.")

(defvar shpool-output-marker "ae6c3bc5-cf99-48c1-840d-6662f718d384"
  "Arbitrary string that is used to mark locations within process output.

We want this string to be something that is unlikely to show up
in any command output and doesn't require any shell quoting.")

(defvar shpool-output-start (format "START:%s" shpool-output-marker))
(defvar shpool-output-end (format "END:%s" shpool-output-marker))
(defvar shpool-ping (format "PING:%s" shpool-output-marker))


(defun shpool-async-shell-command (cmd dir &optional callback env-vars)
  "Execute CMD in DIR and call CALLBACK as output becomes available.

CMD is a string that is passed through to a Bash or cmd.exe
process.  This string must be properly quoted by the caller.  DIR
is a string giving the working directory in which the command is
executed.  Remote TRAMP directories are supported.  CALLBACK is a
function that accepts (BUF START END COMPLETE).  It is called in
the buffer which was active when `shpool-async-shell-command'
was called as output become available.  If that buffer is no
longer alive, CALLBACK is not be called.

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

If ENV-VARS is provided, it is a list of strings of the form
VARNAME=VALUE.  In this case, we run CMD in an inferior shell in
which these environment variables have been set."
  (shpool-trace-call)
  (let ((buf (current-buffer))
        (server-buf (shpool-get-server-buf dir))
        (local-dir (file-local-name default-directory))
        proc proc-cmd)

    (when (not (null env-vars))
      (if (shpool-use-cmd-exe-p)
          ;; cmd.exe
          (let ((var-defs (mapconcat (lambda (def)
                                       (format "SET %s=%s" (car def)
                                               (shell-quote-argument (cdr def))))
                                     env-vars " && ")))
            (setq cmd (format "cmd.exe /c \"%s\"" var-defs cmd)))
        ;; Bash
        (let ((var-defs (mapconcat (lambda (def)
                                     (format "%s=%s" (car def)
                                             (shell-quote-argument (cdr def))))
                                   env-vars " ")))
          (setq cmd (format "env %s bash -c '%s'" var-defs cmd)))))

    (with-current-buffer server-buf
      ;; Set variables for `shpool-async-shell-command--process-filter'.
      (setq-local callback-buf buf)
      (setq-local callback-func callback)
      (setq proc (get-buffer-process (current-buffer)))

      (set-process-filter proc #'shpool-async-shell-command--process-filter)
      (setq proc-cmd
            (cond
             ((shpool-use-cmd-exe-p)
              (mapconcat 'identity
                         (list (format "echo %s & " shpool-output-start)
                               cmd
                               (format "& echo %s" shpool-output-end))))
             (t (format "echo \"$output_start\"; %s; echo \"$output_end\";"
                        cmd))))

      (process-send-string proc (format "%s\n" proc-cmd)))))


(defun shpool-async-shell-command--process-filter (proc string)
  "Process filter for `shpool-async-shell-command'."
  (shpool-trace-call)
  (when (buffer-live-p (process-buffer proc))
    (let ((proc-buf (process-buffer proc))
          ;; echo includes the trailing space when called from cmd.exe.
          (start-regex (format "^%s" shpool-output-start))
          (end-regex (format "^%s" shpool-output-end))
          ;; `output-start' and `output-end' will delimit `string' after we
          ;; insert it in the buffer.
          output-start output-end
          ;; `start-marker' and `end-marker' match the output marker
          ;; strings that we echo from the process.
          start-marker end-marker
          ;; We set `complete' to t when we see end of output marker.
          complete
          (inhibit-read-only t))

      (with-current-buffer proc-buf
        ;; Insert `string`, advance the process marker, and record the
        ;; beginning and end of the inserted text as `output-start` and
        ;; `output-end`.
        (save-excursion
          (goto-char (process-mark proc))
          ;; We return complete lines.
          (setq output-start (point))
          (insert string)
          (set-marker (process-mark proc) (point))
          (setq output-end (point)))

        ;; Look for the special output markers.
        (save-excursion
          (goto-char (point-min))
          (setq start-marker (and (re-search-forward start-regex nil t)
                                  (progn (forward-line 1) (point))))
          (let ((match (re-search-forward end-regex nil t)))
            (setq end-marker (or (and match
                                      (save-excursion (forward-line 0) (point)))
                                 (point-max)))
            (when match (setq complete t))))

        (when (and start-marker (> output-end start-marker) callback-func)
          ;; (message "shpool-async-shell-command--process-filter: %s %s %s %S"
          ;;          output-start output-end start-marker end-marker)
          (setq output-start (max (save-excursion (goto-char output-start)
                                                  (forward-line 0)
                                                  (point))
                                  start-marker)
                output-end (min (save-excursion (goto-char output-end)
                                                (forward-line 0)
                                                (point))
                                end-marker))
          (let ((f callback-func))
            (with-current-buffer callback-buf
              (funcall f proc-buf output-start output-end nil)
              (when complete
                (funcall f proc-buf start-marker end-marker t))))

          (when complete (shpool-return-buffer proc-buf)))))))


(defun shpool-shell-command (cmd &optional bufname env-vars)
  "Execute CMD in a new buffer and pop to that buffer.

CMD is a string that is passed through to an interactive bash or
cmd.exe process.  BUFNAME is a string giving the name of the
buffer in which the results are written.  Any current contents
are deleted.  If ENV-VARS is provided, it is a list of strings of
the form VARNAME=VALUE.  In this case, we run CMD in an inferior
shell in which these environment variables have been set."
  (interactive "sShell Command: ")
  (shpool-trace-call)
  (let* ((dir default-directory)
         (buf (get-buffer-create (or bufname "*Shell Command Output*")))
         (inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer)
      (fundamental-mode)
      (setq-local default-directory dir)
      (setq-local mode-line-process ":running")
      (insert (format "Working Directory: %s\n\n" default-directory))
      (when env-vars
        (insert "Environment Varables:\n")
        (insert (mapconcat (lambda (def) (format "  %s=%s" (car def) (cdr def)))
                           env-vars "\n"))
        (insert "\n\n"))
      (insert (format "> %s\n\n" cmd))
      (setq-local output-marker (shpool-insert-spinner))
      (shpool-async-shell-command cmd dir #'shpool-shell-command-1 env-vars))
    (switch-to-buffer buf)
    buf))


(defun shpool-shell-command-1 (buf start end complete)
  (shpool-trace-call)
  (cond
   (complete
    ;; Delete the spinner
    (delete-region output-marker (point-max))
    (setq mode-line-process ":complete"))

   (t
    (let ((new-text (with-current-buffer buf (buffer-substring start end)))
          (tramp-prefix (or (file-remote-p default-directory) ""))
          (inhibit-read-only t)
          this-line move-pt path)

      ;; Insert the new text and fix up the display.
      (save-excursion
        (if (= (point) output-marker)
            (setq move-pt t)
          (goto-char output-marker))

        ;; This insertion moves `output-marker'
        (save-excursion (insert new-text))

        ;; Delete output that was overwritten using carriage returns.
        (save-excursion
          (while (re-search-forward "^[^]*" nil t)
            (delete-region (match-beginning 0) (match-end 0))))

        ;; Color new input.
        (ansi-color-apply-on-region (point) output-marker)

        (when move-pt
          (goto-char output-marker)))))))


(defun shpool-get-server-buf (&optional dir)
  "Get a buffer containing a live Bash or Windows cmd.exe process.

Attempts to pop a process running on the correct machine from
`shpool-worker-pool'and sets the current directory to DIR.  Spins
up a new process if nothing is available in the worker pool.  The
caller is responsible for returning the buffer by calling
`shpool-return-buffer' when done."
  (shpool-trace-call)
  (let* ((dir (or dir default-directory))
         (local-dir (directory-file-name
                     (file-local-name (expand-file-name dir))))
         (tramp-prefix (or (file-remote-p (or dir default-directory)) ""))
         (key-value (assoc tramp-prefix shpool-worker-pool))
         (buf (cdr key-value))
         (proc (get-buffer-process buf))
         (inhibit-read-only t))

    ;; Remove the buffer from shpool-worker-pool
    (when (not (null key-value))
      (setcar key-value nil)
      (setq shpool-worker-pool (assoc-delete-all nil shpool-worker-pool)))


    (when (process-live-p proc) (shpool-check-for-life buf))

    ;; If the process is dead, use a new buffer.  Leave the buffer for
    ;; debugging purposes.
    (unless (process-live-p proc)
      (setq buf (shpool-generate-new-buffer "shell-pool-server"))
      (with-current-buffer buf
        ;; `start-file-process' looks at `default-directory'.
        (setq default-directory dir)
        (setq proc (if (shpool-use-cmd-exe-p)
                       (start-file-process "cmd-server" buf "cmd")
                     (shpool-start-bash-process)))
        ;; Checking for life here ensures that we have accepted all process
        ;; output before calling `erase-buffer' below.
        (shpool-check-for-life buf)))

    (with-current-buffer buf
      (erase-buffer)
      (special-mode)
      (setq-local kill-buffer-query-functions nil)
      ;; Get a fresh prompt
      (process-send-string proc (format "cd \"%s\"\npwd\n" local-dir))
      (setq default-directory dir))

    buf))


(defun shpool-return-buffer (buf)
  "Return BUF and its process to the worker pool."
  (with-current-buffer buf
    (let ((tramp-prefix (or (file-remote-p default-directory) ""))
          (proc (get-buffer-process buf)))
      ;; Reset the sentinel and filter before returning to the pool.
      (when (process-live-p proc)
        (set-process-sentinel proc nil)
        (set-process-filter proc nil))
      (push `(,tramp-prefix . ,buf) shpool-worker-pool))))


(defun shpool-use-cmd-exe-p (&optional dir)
  "Should we use cmd.exe rather than bash?

We use cmd.exe when we are on the local filesystem of a Windows
machine.  In all other cases (i.e., not on Windows machine or on
a Windows machine but working remotely via TRAMP) we use bash."
  (let ((dir (or dir default-directory)))
    (and (eq window-system 'w32)
         (ignore-errors (not (file-remote-p dir))))))

(defun shpool-start-bash-process (&optional buf)
  (shpool-trace-call)
  (let* ((buf (or buf (current-buffer)))
         (proc (start-file-process "bash-server" buf "bash" "--noediting")))

    ;; Echo process input
    (process-send-string proc "stty echo\n")
    ;; No prompts.  It just makes the output more non-deterministic.
    (process-send-string proc "PS1=\n")
    (process-send-string proc "PS2=\n")
    ;; Define boundary values.  We jump through all this quoting to insert
    ;; a newline at the start of the string.  Using these varaibles allows
    ;; us to avoid embedding the marker string directly in the shell
    ;; commands.
    (process-send-string proc (format "output_start=\"\n%s\"\n"
                                      shpool-output-start))
    (process-send-string proc (format "output_end=\"\n%s\"\n"
                                      shpool-output-end))
    (process-send-string proc (format "ping=\"\n%s\"\n" shpool-ping))
    ;; Return the new process.
    proc))


(defun shpool-trace-call (&optional func args n)
  "Write tracing info about calling function to `shpool-tracing-buffer-name'.

The argument N gives the number of additional step to skip."
  (when shpool-show-tracing-info
    (let* ((n (or n 0))
           (buf (current-buffer))
           (bufname shpool-tracing-buffer-name)
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


(defun shpool-generate-new-buffer (name)
  "Get a new buffer with a name based on NAME."
  (or
   ;; See if the name wrapped in asterisks is available.
   (and (null (get-buffer (format "*%s*" name)))
           (get-buffer-create (format "*%s*" name)))
   ;; Otherwise append numbers to get a new name.
   (let ((i 2))
     (while (get-buffer (format "*%s<%d>*" name i)) (incf i))
     (get-buffer-create (format "*%s<%d>*" name i)))))


(defun shpool-insert-spinner ()
  "Insert spinner at current point."
  (let ((m (copy-marker (point))))
    (set-marker-insertion-type m nil)
    (insert (propertize "|" 'spinner t 'sequence '("/" "-" "\\" "|")))
    (set-marker-insertion-type m t)
    (run-at-time 0.5 nil 'shpool-insert-spinner--spin m)
    m))


(defun shpool-insert-spinner--spin (m)
  "Implementation detail of `gpb-git:insert-spinner'"
  (let ((buf (marker-buffer m)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (ignore-errors (get-text-property m 'spinner))
          (let* ((seq (get-text-property m 'sequence))
                 (next-seq (append (cdr seq) (list (car seq))))
                 (inhibit-read-only t)
                 props)
            (save-excursion
              (goto-char m)
              (setq props (text-properties-at m))
              (plist-put props 'sequence next-seq)
              (set-marker-insertion-type m nil)
              (insert (apply 'propertize (car seq) props))
              (set-marker-insertion-type m t)
              (delete-region (+ m 1) (+ m 2))))
          (run-at-time 0.5 nil 'gpb-git:insert-spinner--spin m))))))


(defun shpool-check-for-life (&optional buf)
  (let* ((buf (or buf (current-buffer)))
         (proc (get-buffer-process buf)))
    (with-current-buffer buf
      ;; Confirm that the process is alive.
      (process-send-string proc (format "echo \"$ping\"\n"))
      ;; Wait for the last echo statement to complete.
      (while (not (save-excursion
                    (goto-char (point-min))
                    (re-search-forward (format "^%s" shpool-ping) nil t)))
        (accept-process-output proc)))))



(provide 'shell-queue)

