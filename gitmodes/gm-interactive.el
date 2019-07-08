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
  (let ((buf (get-buffer-create (apply 'format "*%s %s*" cmd)))
        (script-file (locate-library "git-editor.bash"))
        (tmpfile (make-nearby-temp-file "git-editor-" nil ".bash"))
        (process-environment process-environment)
        (cmd-string (mapconcat 'identity cmd " "))
        proc)
    (assert (not (null script-file)))
    (with-current-buffer buf
      (erase-buffer)
      (setq-local tmpfile tmpfile)
      (insert (format "%s\n" cmd-string)))
    (with-temp-file tmpfile (insert-file-contents script-file))
    ;; Set the permissions so the owner can read, write and execute.
    ;; 448 = 7 * 8 * 8
    (set-file-modes tmpfile 448)
    (push (format "GIT_EDITOR=%s" (file-local-name tmpfile))
          process-environment)
    (push (format "GIT_SEQUENCE_EDITOR=%s" (file-local-name tmpfile))
          process-environment)
    (setq proc (apply 'start-file-process cmd-string buf cmd))
    (set-process-filter proc 'gpb-git:interactive-git-command--filter)
    (switch-to-buffer buf)))


(defun gpb-git:interactive-git-command--filter (proc string)
  (let ((buf (process-buffer proc))
        (regex "^Edit File: \\(.*\\)\nPipe File: \\(.*\\)\n")
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
            (delete-region (match-beginning 0) (match-end 0)))))
      (when edit-file
        (with-current-buffer (find-file edit-file)
          (gpb-git:edit-mode)
          (setq-local pipe-file pipe-file)
          (add-hook 'kill-buffer-hook 'gpb-git:write-to-pipe nil t)
          (run-hooks 'post-command-hook))))))


(defun gpb-git:write-to-pipe ()
  (process-file-shell-command (format "echo done. > %s" pipe-file)))

(defun gpb-git:kill-buffer-hook ()
  (gpb-git:finish-edit t))

(defun gpb-git:finish-edit (&optional dont-save-file)
  "Finish edit and show Git process buffer."
  (interactive)
  (save-buffer)
  (kill-buffer)
  (switch-to-buffer proc-buffer))


(provide 'gm-commit)

