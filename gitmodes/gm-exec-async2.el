(defvar gpb-git:worker-prompt "cc412e81x7f5dxf624x93f3xab948734143a"
  "Arbitrary string that is used as the bash prompt.

We want something that is unlikely to show up in any Git output
and doesn't require any shell quoting.")

(defvar gpb-git:output-start "f691081fx1b3bx5ab4x8233x4f382c9cba21"
  "Arbitrary string that is used to mark output.")

(defvar gpb-git:worker-pool nil
  "An alist of working directories and buffers.

Each buffer contains a bash processes with the associated working
directory.  The working directory may be a TRAMP remote filename,
so these processes may be running on different machines.")

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
      (setq default-directory repo-root
            proc (if use-cmd
                     (start-file-process "cmd-git-server" buf "cmd")
                   (start-file-process "bash-git-server" buf "bash")))
      (if use-cmd
          (process-send-string proc (format "prompt %s$_\n"
                                            gpb-git:worker-prompt))
        (process-send-string proc (format "export PS1=\"\\n%s\\n\"\n"
                                          gpb-git:worker-prompt))))
    buf))

(defun gpb-git:exec-async2 (cmd dir callback &rest args)
  "Execute an asyncronous command and then call CALLBACK.

CMD is a list of strings that is passed through to a bash
process.  CALLBACK is a function that will (eventually) be called
in the buffer which was active when `gpb-git:exec-async2` was
called.  If that buffer is no longer alive when CMD finishes, the
CALLBACK will not be called.  The CALLBACK function is passed the
buffer that contains the process output followed by any
additional arguments in ARGS."
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
        (process-send-string proc (format "echo %s && %s\n"
                                          gpb-git:output-start cmd-string)))
      (let ((inhibit-message t))
        (message "Sent command %S to %S" cmd (current-buffer))))))

(defun gpb-git:exec-async2--process-filter (proc string)
  "Process sentinel used handle asyncronous Git commands."
  (gpb-git--trace-funcall #'gpb-git:exec-async2--process-filter
                          `(,proc ,string))
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (save-excursion
        ;; Insert the text, advancing the process marker.
        (goto-char (process-mark proc))
        (insert string)
        (set-marker (process-mark proc) (point)))

      (let ((start-regex (format "%s *\n" gpb-git:output-start))
            (end-regex (format "^?\n%s?\n" gpb-git:worker-prompt)))
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

(provide 'gm-exec-async2)
