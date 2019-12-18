(defvar gpb-git:worker-pool nil
  "An alist of working directories and buffers.

Each buffer contains a bash processes with the associated working
directory.  The working directory may be a TRAMP remote filename,
so these processes may be running on different machines.")

(defvar gpb-git:process-output-marker "e7010240-6f57-4d86-84f9-62fb8958b7a6"
  "Arbitrary string that is used to mark locations within process output.

We want this string to be something that is unlikely to show up
at the start of a line in any Git command output and doesn't
require any shell quoting.")

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
  (gpb-git--trace-funcall)
  (let* ((buf (gpb-git--get-new-buffer "*git server" "*"))
         (dir default-directory)
         (process-environment process-environment)
         proc)

    (with-current-buffer buf
      (insert (format "Repo Dir: %s\n\n" repo-root))

      ;; Set Git environment variables to use a custom editor script.
      (cond
       ;; If we are working locally on a Windows machine, use the CMD script.
       ((gpb-git:use-cmd-exe dir)
        (push (format "GIT_EDITOR=%s" (locate-library "git-editor.cmd"))
              process-environment)
        (push (format "GIT_SEQUENCE_EDITOR=%s"
                      (locate-library "git-editor.cmd"))
              process-environment))

       ;; Othewise, use the BASH script.  We could be working remotely, so we
       ;; write the script to a file on the machine where the Git command is
       ;; running.
       (t
        (let ((script-file (locate-library "git-editor.bash"))
              (tmpfile-name (gpb-git:get-temporary-file "git-editor.bash")))
          (assert (not (null script-file)))
          (with-temp-file tmpfile-name (insert-file-contents script-file))
          ;; Set the permissions so the owner can read, write and execute.
          ;; 448 = 7 * 8 * 8
          (set-file-modes tmpfile-name 448)
          (push (format "GIT_EDITOR=bash %s" (file-local-name tmpfile-name))
                process-environment)
          (push (format "GIT_SEQUENCE_EDITOR=bash %s"
                        (file-local-name tmpfile-name))
                process-environment))))

      (setq default-directory repo-root
            proc (if (gpb-git:use-cmd-exe repo-root)
                     (start-file-process "cmd-git-server" buf "cmd")
                   (start-file-process "bash-git-server" buf "bash")))
      (setq-local kill-buffer-query-functions nil)
      (setq-local output-start-marker (copy-marker (point-min)))
      (setq-local output-end-marker (copy-marker (point-min)))


      )
    buf))


(defvar git-command-output-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap shell-command] 'gpb-git:shell-command)
    map))

(define-derived-mode git-command-output-mode special-mode "Git Command")

(defun gpb-git:shell-command (cmd)
  "Execute CMD in a new buffer and pop to that buffer.

CMD is a string that is passed through to an interactive bash or
cmd.exe process."
  (interactive "sGit Shell Command: ")
  (let ((buf (get-buffer-create "*Git Shell Command*"))
        (repo-root (gpb-git--find-repo-root)))

    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (git-command-output-mode)
        (setq mode-line-process ":running")
        (insert (format "Repo: %s\n\n" default-directory))
        (insert (format "%s\n\n" cmd))
        (setq-local output-marker (gpb-git:insert-spinner)))
      (view-mode)
      (setq default-directory repo-root)
      (gpb-git:async-shell-command cmd repo-root #'gpb-git:shell-command-1))

    (switch-to-buffer buf)))


(defun gpb-git:shell-command-1 (buf start end complete)
  (gpb-git--trace-funcall)
  (let ((output-buf (current-buffer))
        (new-text "")
        (inhibit-read-only t)
        (remote-prefix (or (file-remote-p default-directory) ""))
        this-line move-pt)

    ;; Read the new output line by line, looking for special output markers.
    (with-current-buffer buf
      (save-excursion
        (save-match-data
          (goto-char start)
          (assert (bolp))
          (while (< (point) end)
            (cond
             ;; Skip over any initial output that is later overwritten
             ;; through the use of carriage returns.
             ((looking-at (format "^\\(?:[^]*\\)*%s:edit-file:\\(.*\\)"
                                  gpb-git:process-output-marker))
              (with-current-buffer (find-file (concat remote-prefix
                                                      (match-string 1)))
                (gpb-git:edit-mode)
                ;; Used by `gpb-git:finish-edit`.
                (setq-local command-buffer output-buf)
                ;; This happens in a callback so call any post command hooks so
                ;; they can respond to the updated current buffer.
                (run-hooks 'post-command-hook)))

             ((looking-at (format "^%s:pipe-file:\\(.*\\)"
                                  gpb-git:process-output-marker))
              ;; `gpb-git:send-signal-to-git' uses this value.
              (let ((match-str (match-string 1)))
                (message "Pipe File: %s" match-str)
                (with-current-buffer output-buf
                  (setq-local pipe-file match-str))))

             (t
              (setq this-line (buffer-substring
                               (point) (save-excursion (end-of-line) (point)))
                    new-text (format "%s%s\n" new-text this-line))))
            (forward-line 1)))))

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

      (when complete
        (with-current-buffer output-buf
          ;; Delete the spinner
          (delete-region output-marker (point-max))
          (setq mode-line-process ":complete"))))

    (when move-pt
      (goto-char output-marker))))


(defun gpb-git:async-shell-command (cmd dir &optional callback)
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
      (setq proc-cmd (format (if (gpb-git:use-cmd-exe)
                                 "echo %s:output-start & %s & echo %s:output-end"
                               "echo %s:output-start ; %s ; echo %s:output-end")
                             gpb-git:process-output-marker
                             cmd
                             gpb-git:process-output-marker))

      (unless (gpb-git:use-cmd-exe)
        ;; Bash doesn't echo the command, so we echo it into the buffer
        ;; directly.  This ensures that each
        ;; `gpb-git:process-output-marker` we echo above starts at the
        ;; beginning of an output line.
        (setq proc-cmd (format "echo \"%s\" && %s" proc-cmd proc-cmd)))

      (process-send-string proc (format "%s\n" proc-cmd))
      (let ((inhibit-message t))
        (message "Sent command %S to %S" cmd (current-buffer))))))


(defun gpb-git:async-shell-command--process-filter (proc string)
  "Process filter for `gpb-git:async-shell-command'."
  (gpb-git--trace-funcall)
  (when (buffer-live-p (process-buffer proc))
    (let ((proc-buf (process-buffer proc))
          ;; echo includes the trailing space when called from cmd.exe.
          (start-regex (format "^%s:output-start ?\n"
                               gpb-git:process-output-marker))
          (end-regex (format "^%s:output-end ?\n"
                             gpb-git:process-output-marker))
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
          ;; We are waiting on the next output start marker.
          (save-excursion
            (goto-char output-end-marker)
            (when (re-search-forward start-regex nil t)
              (set-marker output-start-marker (match-end 0)))))

        (when (> output-start-marker output-end-marker)
          ;; We are waiting on the next output end marker.
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
            (when callback-func
              (let ((f callback-func))
                (with-current-buffer callback-buf
                  (funcall f proc-buf output-start output-end complete))))))))))


(defun gpb-git:send-signal-to-git ()
  (gpb-git--trace-funcall)
  (if (gpb-git:use-cmd-exe)
      (unless (= (call-process-shell-command "waitfor /si EmacsEditDone") 0)
        (error "Could not send signal to Git process"))
    (gpb-git:async-shell-command
     (format "echo done. > %s" pipe-file) default-directory)))

(defun gpb-git:commit (&optional amend)
  (interactive "P")
  (if amend
      (gpb-git:shell-command "git commit --amend")
    (gpb-git:shell-command "git commit")))

(defun gpb-git:interactive-rebase (hash)
  (interactive (list (read-string "Base Commit: ")))
  (gpb-git:shell-command (format "git rebase -i %s" hash)))


(define-derived-mode gpb-git:edit-mode diff-mode "Git Edit"
  "Mode for editing a file during an interactive Git command."
  (local-set-key "\C-c\C-c" 'gpb-git:finish-edit)
  (add-hook 'kill-buffer-hook 'gpb-git:kill-buffer-hook nil t))

(defun gpb-git:kill-buffer-hook ()
  (ignore-errors
    (with-current-buffer command-buffer
      (gpb-git:send-signal-to-git))))

(defun gpb-git:finish-edit ()
  "Finish edit and show Git process buffer."
  (interactive)
  (save-buffer)
  ;; The buffer local variable `command-buffer' was set by
  ;; `gpb-git:shell-command-1'.
  (let ((buf command-buffer))
    (kill-buffer)
    (with-current-buffer buf (gpb-git:send-signal-to-git))
    (switch-to-buffer buf)))


(defun gpb-git:use-cmd-exe (&optional dir)
  "Are we using cmd.exe rather than bash?

We use cmd.exe when we are on the local filesystem of a Windows
machine.  In all other cases (i.e., not on Windows machine or on
a Windows machine but working remotely via TRAMP) we use bash."
  (let ((dir (or dir default-directory)))
    (and (eq window-system 'w32)
         (ignore-errors (not (file-remote-p default-directory))))))


(provide 'gm-shell-commands)

