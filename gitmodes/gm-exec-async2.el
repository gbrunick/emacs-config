(defvar gpb-git:worker-pool nil
  "An alist of working directories and buffers.

Each buffer contains a bash processes with the associated working
directory.  The working directory may be a TRAMP remote filename,
so these processes may be running on different machines.")

(defvar gpb-git:output-start "f691081fx1b3bx5ab4x8233x4f382c9cba21"
  "Arbitrary string that is used to mark the start of process output.

We want something that is unlikely to show up in any Git output
and doesn't require any shell quoting.")

(defvar gpb-git:output-end "cc412e81x7f5dxf624x93f3xab948734143a"
  "Arbitrary string that is used to mark the end of process output.")

(defun gpb-git:get-git-server-buf (&optional repo-root)
  "Get a buffer containing a live bash process.

Attempts to pop a bash process with the correct working directory
from `gpb-git:worker-pool'.  Spins up a new process if nothing is
available in the worker pool.  The caller is responsible for add
the buffer returned back to `gpb-git:worker-pool' when done using
the worker."
  (let* ((repo-root (or repo-root default-directory))
         (key-value (assoc repo-root gpb-git:worker-pool))
         (buf (cdr key-value))
         (proc (get-buffer-process buf)))

    (when (not (null buf))
      ;; Remove the buffer from gpb-git:worker-pool
      (setcar key-value nil)
      (setq gpb-git:worker-pool (assoc-delete-all nil gpb-git:worker-pool)))

    (cond
     ((process-live-p proc) buf)
     (t (gpb-git:make-git-server-buf repo-root)))))

(defun gpb-git:make-git-server-buf (repo-root)
  "Create a buffer containing a live bash process.

The buffer name is based on the buffer name of the current buffer."
  (gpb-git--trace-funcall #'gpb-git:make-git-server-buf `(,repo-root))
  (let* ((buf (gpb-git--get-new-buffer "*git server" "*"))
         (dir default-directory)
         (use-cmd (and (eq window-system 'w32) (not (file-remote-p dir))))
         proc)
    (with-current-buffer buf
      (insert (format "Repo Dir: %s\n\n" repo-root))
      (setq default-directory repo-root
            proc (if use-cmd
                     (start-file-process "cmd-git-server" buf "cmd")
                   (start-file-process "bash-git-server" buf "bash")))
      (setq-local kill-buffer-query-functions nil)
      (setq-local command-start-marker (make-marker))
      (setq-local output-start-marker (make-marker))
      (setq-local output-end-marker (make-marker)))
    buf))

(defun gpb-git:shell-command (cmd)
  "Execute CMD.

CMD is a list of strings that is passed through to a bash
process."
  (interactive "sShell Command: ")
  (let ((repo-root (gpb-git--find-repo-root))
        (buf (gpb-git--get-new-buffer "*shell command" "*"))
        proc output)

    (with-current-buffer buf
      (setq default-directory repo-root)
      (setq-local output-marker (copy-marker (point-min)))
      (gpb-git:async-shell-command-1
       cmd repo-root #'gpb-git:shell-command-1 t))

    (pop-to-buffer buf)))


(defun gpb-git:shell-command-1 (buf start end complete)
  (gpb-git--trace-funcall)
  (let ((new-text (with-current-buffer buf
                    (buffer-substring-no-properties start end))))
    (save-excursion
      (goto-char output-marker)
      (insert new-text)
      (ansi-color-apply-on-region output-marker (point))
      (move-marker output-marker (point)))))


(defun gpb-git:async-shell-command (cmd)
  "Execute CMD.

CMD is a list of strings that is passed through to a bash
process."
  (interactive "sShell Command: ")
  (let ((server-buf (gpb-git:get-git-server-buf))
        (start-regex (format "%s *\n" gpb-git:output-start))
        (end-regex (format "^?\n%s?\n" gpb-git:output-end))
        (buf (gpb-git--get-new-buffer "*shell command" "*"))
        proc output)

    (with-current-buffer buf
      (insert cmd))

    (with-current-buffer server-buf
      ;; Set variables for `gpb-git:exec-async2--process-filter'.
      (setq proc (get-buffer-process (current-buffer)))

      ;; Try to flush any pending output.
      (set-process-filter proc nil)
      (accept-process-output proc 0 nil t)
      (erase-buffer)

      (process-send-string proc (format "echo %s && %s echo %s\n"
                                        gpb-git:output-start cmd
                                        gpb-git:output-end))
      (let ((inhibit-message t))
        (message "Sent command %S to %S" cmd (current-buffer)))

      (while (not (save-excursion
                    (goto-char (point-max))
                    (and (re-search-backward end-regex nil t)
                         (re-search-backward start-regex nil t))))
        (accept-process-output proc nil nil t))

      (setq output (buffer-substring-no-properties
                    (match-end 0) (save-excursion
                                    (re-search-backward start-regex)
                                    (match-beginning 0)))))

    (pop-to-buffer buf)))

(defun gpb-git:exec-async2 (cmd dir callback &rest args)
  "Execute CMD in DIR and call CALLBACK when the command is complete.

CMD is a list of strings that is passed through to a Bash or
Windows cmd.exe process.  CALLBACK is a function that
will (eventually) be called in the buffer which was active when
`gpb-git:exec-async2` was called.  If that buffer is no longer
alive when CMD finishes, the CALLBACK will not be called.  The
CALLBACK function is passed the buffer that contains the process
output followed by any additional arguments in ARGS."
  (gpb-git--trace-funcall #'gpb-git:exec-async2 `(,cmd ,dir ,callback ,@args))
  (let ((buf (current-buffer))
        (server-buf (gpb-git:get-git-server-buf))
        proc)
    (with-current-buffer server-buf
      ;; Set variables for `gpb-git:exec-async2--process-filter'.
      (setq-local callback-func callback)
      (setq-local callback-args args)
      (setq-local callback-buf buf)
      (setq proc (get-buffer-process (current-buffer)))

      ;; Try to flush any pending output.
      (set-process-filter proc nil)
      (accept-process-output proc 0 nil t)
      (erase-buffer)
      (set-process-filter proc #'gpb-git:exec-async2--process-filter)

      (let ((cmd-string (mapconcat #'shell-quote-argument cmd " ")))
        (process-send-string proc (format "&& echo %s && %s\n"
                                          gpb-git:output-start
                                          cmd-string)))
      (let ((inhibit-message t))
        (message "Sent command %S to %S" cmd (current-buffer))))))

(defun gpb-git:exec-async2--process-filter (proc string)
  "Process sentinel used handle asyncronous Git commands."
  (gpb-git--trace-funcall #'gpb-git:exec-async2--process-filter
                          `(,proc ,string))
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (unless (boundp 'command-output-start)
        (setq-local command-output-start nil))
      (unless (boundp 'partial-line)
        (setq-local insert-process-output nil))

      (save-excursion
        ;; Insert the text, advancing the process marker.
        (goto-char (process-mark proc))
        (insert string)
        (set-marker (process-mark proc) (point)))

      (let ((start-regex (format "%s *\n" gpb-git:output-start))
            (end-regex (format "^?\n%s?\n" gpb-git:output-end)))
        (when (save-excursion
                (goto-char (point-max))
                (and (re-search-backward end-regex nil t)
                     (re-search-backward start-regex nil t)))
          (gpb-git:exec-async2--handle-callback))))))

(defun gpb-git:exec-async2--handle-callback ()
  "Implementation detail of `gpb-git:exec-async2--process-filter`.

This function was split out for easier debugging."
  (gpb-git--trace-funcall #'gpb-git:exec-async2--handle-callback nil)

  ;; Delete everything in front of the `gpb-git:output-start` marker.
  (delete-region (point-min) (match-end 0))

  ;; Delete the BASH prompt.
  (re-search-forward end-regex)
  (delete-region (match-beginning 0) (match-end 0))

  (goto-char (point-min))

  ;; We bind the values from the current buffer local variables
  ;; before we switch back to `callback-buf'.
  (let ((func callback-func)
        (args (cons (current-buffer) callback-args))
        (buf callback-buf))
    (with-current-buffer callback-buf
      (let ((debug-on-error t))
        (gpb-git--trace-funcall func args)
        (apply func args)))

    ;; Add the bash process back to the worker pool.
    (setq gpb-git:worker-pool (nreverse
                               (cons (cons default-directory (current-buffer))
                                     (nreverse gpb-git:worker-pool))))))


(defun gpb-git:async-shell-command-1 (cmd dir callback)
  "Execute CMD in DIR and call CALLBACK as output becomes available.

CMD is a string that is passed through to a Bash or cmd.exe
process.  This string must be properly quoted by the caller.  DIR
is a string giving the working directory in which the command is
executed.  Remote TRAMP directories are supported.  CALLBACK is a
function that accepts (BUF START END COMPLETE).  It is called in
the buffer which was active when `gpb-git:async-shell-command-1'
was called as output become available.  If that buffer is no
longer alive, CALLBACK is not be called.

CALLBACK is called as output is read from the worker process.
The buffer containing process output is passed to CALLBACK as
BUF.  START and END give the region that contains new output.
COMPLETE is t when this worker has finished processing command
and nil otherwise."
  (gpb-git--trace-funcall)
  (let ((buf (current-buffer))
        (server-buf (gpb-git:get-git-server-buf dir))
        proc proc-cmd)
    (with-current-buffer server-buf
      ;; Set variables for `gpb-git:async-shell-command-2'.
      (setq-local callback-buf buf)
      (setq-local callback-func callback)
      (setq proc (get-buffer-process (current-buffer)))

      ;; Try to flush any pending output.  There are no guarantees that
      ;; this is really all pending output; it is possible that we get
      ;; unlucky and additional pending output shows up right after we call
      ;; `accept-process-output`.
      (set-process-filter proc nil)
      (accept-process-output proc 0 nil t)
      (set-marker output-start-marker nil)
      (set-process-filter proc #'gpb-git:async-shell-command-2)

      (setq proc-cmd (format "echo %s && echo %s && %s && echo %s\n"
                             cmd gpb-git:output-start cmd gpb-git:output-end))

      (set-marker command-start-marker (process-mark proc))
      (process-send-string proc proc-cmd)

      (let ((inhibit-message t))
        (message "Sent command %S to %S" cmd (current-buffer))))))


(defun gpb-git:async-shell-command-2 (proc string)
  "Process filter for `gpb-git:async-shell-command-1'."
  (gpb-git--trace-funcall)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)

      (let ((start-regex (format "^%s\n" gpb-git:output-start))
            (end-regex (format "^%s\n" gpb-git:output-end))
            (callback callback-func)
            output-start output-end complete)

        ;; Insert `string`, advance the process marker, and record the
        ;; beginning and end of the inserted text as `output-start` and
        ;; `output-end`.
        (save-excursion
          (goto-char (process-mark proc))
          (setq output-start (save-excursion (beginning-of-line) (point)))
          (insert string)
          (set-marker (process-mark proc) (point))
          (setq output-end (save-excursion (beginning-of-line) (point))))

        (save-excursion
          ;; `command-start-marker` tracks the beginning of the process
          ;; output for this command.  `output-start-marker` and
          ;; `output-start-marker` tracks the portion of that output we
          ;; care about that lives between the strings
          ;; `gpb-git:output-start` and `gpb-git:output-end`.
          ;; If we have not yet found the start of the output, look for it.
          (unless (marker-position output-start-marker)
            (goto-char command-start-marker)
            (when (re-search-forward start-regex nil t)
              (set-marker output-start-marker (match-end 0))
              (add-text-properties (match-beginning 0) (match-end 0)
                                   `(display "\n"))))

          ;; If we have found the start of the output, look for the end and
          ;; send new output back to the callback function.
          (when (marker-position output-start-marker)
            (setq output-start (max output-start output-start-marker))
            (goto-char output-start)
            (forward-line 0)
            (when (re-search-forward end-regex nil t)
              (setq output-end (match-beginning 0)
                    complete t)

              ;; Add the bash process back to the end of the worker pool.
              (setq gpb-git:worker-pool
                    (nreverse
                     (cons (cons default-directory (current-buffer))
                           (nreverse gpb-git:worker-pool))))

              (add-text-properties (match-beginning 0) (match-end 0)
                                   `(display "\n")))

            (save-restriction
              (narrow-to-region output-start-marker output-end)
              (with-current-buffer callback-buf
                (funcall callback (process-buffer proc)
                         output-start output-end complete)))))))))


(provide 'gm-exec-async2)
