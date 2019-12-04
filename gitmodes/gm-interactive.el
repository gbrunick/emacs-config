(define-derived-mode gpb-git:edit-mode diff-mode "Git Edit"
  "Mode for editing a file during an interactive Git command."
  (local-set-key "\C-c\C-c" 'gpb-git:finish-edit))

(defun gpb-git:commit (&optional amend)
  (interactive "P")
  (if amend
      (gpb-git:interactive-git-command "git" "commit" "--amend")
    (gpb-git:interactive-git-command "git" "commit")))

(defun gpb-git:interactive-rebase (hash)
  (interactive (list (read-string "Base Commit: ")))
  (gpb-git:interactive-git-command "git" "rebase" "-i" hash))

(defun gpb-git:interactive-git-command (&rest cmd)
  "Perform an interactive Git command."
  (let ((buf (gpb-git:get-git-server-buf))
        (process-environment process-environment)
        (cmd-string (mapconcat 'identity cmd " "))
        proc)

    (cond
     ;; If we are working locally on a Windows machine, use the CMD script.
     ((and (eq window-system 'w32) (not (file-remote-p default-directory)))
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

    (with-current-buffer buf
      (erase-buffer)
      (insert (format "%s\n" cmd-string))
      (setq proc (apply 'start-file-process cmd-string buf cmd))
      (set-process-filter proc 'gpb-git:interactive-git-command--filter))

    (switch-to-buffer buf)))

(defun gpb-git:interactive-git-command--filter (proc string)
  "Implementation detail of `gpb-git:interactive-git-command'

This filter watches the output for a markers that the script
git-editor.bash writes that tell Emacs the name of a file to
edit, and the FIFO pipe to use for syncronization."
  (gpb-git--trace-funcall #'gpb-git:interactive-git-command--filter
                          `(,proc ,string))
  (let ((buf (process-buffer proc))
        (regex "^Edit File: \\(.*\\)\nPipe File: \\(.*\\)\n")
        (remote-prefix (or (file-remote-p default-directory) ""))
        edit-file pipe-file)
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (goto-char (process-mark proc))
        (insert string)
        (set-marker (process-mark proc) (point))
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward regex nil t)
            (setq edit-file (match-string 1)
                  pipe-file (match-string 2)
                  proc-buffer buf)
            (message "edit-file: %s" edit-file)
            (message "pipe-file: %s" pipe-file)
            (delete-region (match-beginning 0) (match-end 0))))

        ;; Git uses carriage returns to overwrite the current line output.
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward "^[^\r\n]*\r" nil t)
            (delete-region (match-beginning 0) (match-end 0)))))

      (when edit-file
        (with-current-buffer (find-file (concat remote-prefix edit-file))
          (gpb-git:edit-mode)
          (setq-local pipe-file pipe-file)
          (add-hook 'kill-buffer-hook 'gpb-git:kill-buffer-hook nil t)
          (run-hooks 'post-command-hook))))))

(defun gpb-git:send-signal-to-git ()
  (gpb-git--trace-funcall #'gpb-git:send-signal-to-git `(,pipe-file))
  (if (and (eq window-system 'w32) (not (file-remote-p default-directory)))
      (shell-command "waitfor /si EmacsEditDone")
    (process-file-shell-command
     (format "bash -c \"echo done. > %s\"" pipe-file))))

(defun gpb-git:kill-buffer-hook ()
  (ignore-errors (gpb-git:send-signal-to-git)))

(defun gpb-git:finish-edit ()
  "Finish edit and show Git process buffer."
  (interactive)
  (save-buffer)
  (gpb-git:send-signal-to-git)
  (kill-buffer)
  (switch-to-buffer proc-buffer))


(provide 'gm-interactive)

