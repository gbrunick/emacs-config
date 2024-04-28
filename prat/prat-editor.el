;; prat-editor.el
;;
;; Wrappers for the Git commit and interactive rebase commands that require
;; the user to edit file.


(defun prat-commit (&optional amend)
  "Commit currently staged changes to Git.
With a prefix argument, amends previous commit."
  (interactive "P")
  (prat-editor-command (if amend "git commit --amend" "git commit")))


(defun prat-editor-command (cmd)
  "Execute a Git command that requires editing a file."
  (interactive "P")
  (prat-trace-funcall)
  (let* ((editor-script
          "c:/Users/gbrun/test path/with spaces/test-prat-editor.cmd")
          ;; (locate-library "prat-editor.cmd"))
         (env-vars (list (format "GIT_EDITOR=\"%s\"" editor-script)
                         (format "GIT_SEQUENCE_EDITOR=\"%s\"" editor-script)
                         (format "PRAT_GIT_CMD=%s" cmd)
                         "TERM=dumb"))
         (repo-root (prat-find-repo-root)))

    ;; `prat-editor-callback' opens a buffer to edit the required file.
    (shpool-async-shell-command cmd repo-root #'prat-editor-callback env-vars)))


(defun prat-editor-callback (buf start end complete)
  "Callback function for commit and interactive rebase commands"
  (prat-trace-funcall)
  ;; Remove any quotes around filenames.
  (let ((file-regex (format "edit-file:\"?\\([^\"\n]+\\)\"?$"))
        (pipe-regex (format "pipe-file:\"?\\([^\"\n]+\\)\"?$"))
        (inhibit-read-only t)
        file-name pipe-name output-txt)
    (with-current-buffer buf
      (save-excursion
        (save-match-data
          (goto-char start)
          (cond
           (complete
            (re-search-forward pipe-regex end)
            (forward-line 1)
            (setq output-txt (buffer-substring-no-properties (point) end))
            (with-current-buffer (get-buffer-create "*Git Commit*")
              (erase-buffer)
              (save-excursion (insert output-txt))
              (switch-to-buffer (current-buffer))))
           (t
            ;; The process waits for the completion of editing, so we have
            ;; to look for these string before the process completes.
            (when (re-search-forward pipe-regex end t)
              (setq pipe-name (match-string-no-properties 1))
              (message "Pipe File: %s" pipe-name)
              (forward-line -1)
              (re-search-forward file-regex)
              (setq file-name (match-string-no-properties 1))
              (message "Edit file: %s" file-name)

              (with-current-buffer (find-file file-name)
                (prat-edit-mode)
                ;; `prat-kill-buffer-hook' uses this value.
                (setq-local pipe-file pipe-name)
                ;; This happens in a callback so call any post command hooks so
                ;; they can respond to the updated current buffer.
                (run-hooks 'post-command-hook))))))))))


(defun prat-get-editor-script (&optional dir)
  (cond
   ;; If we are working locally on a Windows machine, use the
   ;; CMD script.
   ((prat-use-cmd-exe dir)
    (let ((path (concat(file-name-as-directory
                        prat-windows-editor-temp-dir)
                       "prat-editor.cmd")))
      (unless (file-exists-p path)
        (copy-file (locate-library "prat-editor.cmd") path))
      path))

   (t
    (let ((script-file (locate-library "prat-editor.bash"))
          (tmpfile-name (prat-get-temporary-file "prat-editor.bash"))
          ;; If we are on windows, we still need to write a Bash script
          ;; with Unix line endings.
          (coding-system-for-write 'us-ascii-unix))
      (cl-assert (not (null script-file)))
      (with-temp-file tmpfile-name
        (insert-file-contents script-file)
        ;; One more attempt to avoid DOS line endings.
        (delete-trailing-whitespace))
      ;; Set the permissions so the owner can read, write and execute.
      ;; 448 = 7 * 8 * 8
      (set-file-modes tmpfile-name 448)
      tmpfile-name))))


(defun prat-interactive-rebase (hash)
  (interactive (list (read-string "Base Commit: ")))
  (prat-shell-command (format "git rebase -i %s" hash)))

(defvar prat-edit-mode-keywords
  (list "^pick" "^reword" "^edit" "^squash" "^fixup" "^exec" "^drop"))

(defvar prat-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'prat-finish-edit)
    map)
  "Keymap for `prat-edit-mode'.")

(defvar prat-edit-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    table)
  "Lines starting with # are comments.")

(define-derived-mode prat-edit-mode text-mode "Git Edit"
  "Mode for editing a file during an interactive Git command."
  ;; :syntax-table prat-edit-mode-syntax-table
  (use-local-map prat-edit-mode-map)
  (setq font-lock-defaults (list 'prat-edit-mode-keywords))
  (font-lock-fontify-buffer)
  (add-hook 'kill-buffer-hook 'prat-editor-kill-buffer-hook nil t))

(defun prat-editor-kill-buffer-hook ()
  (prat-trace-funcall)
  (ignore-errors
    (cond
     ((prat-use-cmd-exe)
      ;; prat-editor.cmd waits for this signal.
      (shpool-async-shell-command "waitfor /si EmacsEditDone"))
     (t
      ;; prat-editor.sh reads from this named PIPE.
      (shpool-async-shell-command
       (format "echo done. > %s" pipe-file) default-directory)))))

(defun prat-finish-edit ()
  "Finish edit and show Git process buffer."
  (interactive)
  (prat-trace-funcall)
  (save-buffer)
  (remove-hook 'kill-buffer-hook 'abort-recursive-edit t)
  (kill-buffer)
  (message "Committed changes: %s" (current-buffer)))


