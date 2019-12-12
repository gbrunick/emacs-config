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
      (setq-local output-start-marker (copy-marker (point-min)))
      (setq-local output-end-marker (copy-marker (point-min))))
    buf))


(defun gpb-git:shell-command (cmd)
  "Execute CMD in a new buffer and pop to that buffer.

CMD is a string that is passed through to an interactive bash or
cmd.exe process."
  (interactive "sGit Shell Command: ")
  (let ((repo-root (gpb-git--find-repo-root))
        (buf (get-buffer-create "*Git Shell Command*"))
        proc output)

    (with-current-buffer buf
      (let ((inhibit-read-only t)) (erase-buffer))
      (view-mode)
      (setq default-directory repo-root)
      (setq-local output-marker (copy-marker (point-min)))
      (gpb-git:async-shell-command cmd repo-root #'gpb-git:shell-command-1))

    (switch-to-buffer buf)))


(defun gpb-git:shell-command-1 (buf start end complete)
  (gpb-git--trace-funcall)
  (let ((new-text (with-current-buffer buf
                    (buffer-substring-no-properties start end)))
        (inhibit-read-only t))
    (save-excursion
      (goto-char output-marker)
      (insert new-text)

      ;; Delete overwriten output.
      (save-excursion
        (goto-char output-marker)
        (while (re-search-forward "^[^]*" nil t)
          (delete-region (match-beginning 0) (match-end 0))))

      ;; Color new input.
      (ansi-color-apply-on-region output-marker (point))

      ;; Update the marker.
      (move-marker output-marker (point)))))


(defun gpb-git:async-shell-command (cmd dir callback)
  "Execute CMD in DIR and call CALLBACK as output becomes available.

CMD is a string that is passed through to a Bash or cmd.exe
process.  This string must be properly quoted by the caller.  DIR
is a string giving the working directory in which the command is
executed.  Remote TRAMP directories are supported.  CALLBACK is a
function that accepts (BUF START END COMPLETE).  It is called in
the buffer which was active when `gpb-git:async-shell-command'
was called as output become available.  If that buffer is no
longer alive, CALLBACK is not be called.

CALLBACK is called as output is read from the worker process.
The buffer containing process output is passed to CALLBACK as
BUF.  START and END give the region that contains new output.
The new output only contains complete lines (i.e., newline
terminated).  COMPLETE is t when this worker has finished
processing command and nil otherwise."
  (gpb-git--trace-funcall)
  (let ((buf (current-buffer))
        (server-buf (gpb-git:get-git-server-buf dir))
        proc proc-cmd)
    (with-current-buffer server-buf
      ;; Set variables for `gpb-git:async-shell-command--process-filter'.
      (setq-local callback-buf buf)
      (setq-local callback-func callback)
      (setq proc (get-buffer-process (current-buffer)))

      (set-process-filter proc #'gpb-git:async-shell-command--process-filter)
      (setq proc-cmd (format "echo %s && %s && echo %s\n"
                             gpb-git:output-start cmd gpb-git:output-end))
      (process-send-string proc proc-cmd)

      (let ((inhibit-message t))
        (message "Sent command %S to %S" cmd (current-buffer))))))


(defun gpb-git:async-shell-command--process-filter (proc string)
  "Process filter for `gpb-git:async-shell-command'."
  (gpb-git--trace-funcall)
  (when (buffer-live-p (process-buffer proc))
    (let ((proc-buf (process-buffer proc))
          ;; echo includes the trailing space when called from cmd.exe.
          (start-regex (format "^%s ?\n" gpb-git:output-start))
          (end-regex (format "^%s ?\n" gpb-git:output-end))
          ;; The let-bound variables `output-start' and `output-end' will
          ;; delimit the new lines of output that result from inserting
          ;; `string'.
          output-start output-end complete)

      (with-current-buffer proc-buf
        ;; Buffer local variables set by gpb-git:async-shell-command:
        ;;
        ;; * `output-start-marker' and `output-end-marker' give the
        ;;   location of the current matches to `start-regex' and
        ;;   `end-regex'.  The markers are both initialized to the start of
        ;;   the buffer prior to the first shell command.
        ;;
        ;; * `callback-func' and `callback-buf' give the callback function
        ;;   and the buffer in which it should be called.
        ;;
        ;; Insert `string`, advance the process marker, and record the
        ;; beginning and end of the inserted text as `output-start` and
        ;; `output-end`.
        (save-excursion
          (goto-char (process-mark proc))
          ;; We return complete lines.
          (setq output-start (save-excursion (beginning-of-line) (point)))
          (insert string)
          (set-marker (process-mark proc) (point))
          (setq output-end (save-excursion (beginning-of-line) (point))))

        (when (<= output-start-marker output-end-marker)
          ;; We are waiting on the next `gpb-git:output-start` marker.
          (save-excursion
            (goto-char output-end-marker)
            (when (re-search-forward start-regex nil t)
              (set-marker output-start-marker (match-end 0)))))

        (when (> output-start-marker output-end-marker)
          ;; We are waiting on the next `gpb-git:output-end` marker.
          (setq output-start (max output-start output-start-marker))
          (goto-char output-start)
          (forward-line 0)
          (when (re-search-forward end-regex nil t)
            (setq complete t)
            (set-marker output-end-marker (match-beginning 0))
            ;; Trim the current output region.
            (setq output-end (min output-end output-end-marker))
            (push `(,default-directory . ,proc-buf) gpb-git:worker-pool))

          (save-restriction
            (narrow-to-region output-start-marker output-end)
            ;; Capture the buffer local variable `callback-func' so we can
            ;; call it in the buffer `callback-buf'.
            (let ((f callback-func))
              (with-current-buffer callback-buf
                (funcall f proc-buf output-start output-end complete)))))))))


(provide 'gm-shell-commands)
