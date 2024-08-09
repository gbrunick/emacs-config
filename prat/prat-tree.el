(defvar prat-show-tree-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'prat-show-tree--show-file)
    (define-key map "g" 'prat-refresh-file-tree)
    map)
  "The keymap used when viewing the files associated with a commit.")


(define-derived-mode prat-show-tree-mode prat-base-mode
  "File List"
  "\nMode for buffers displaying the files in a Git commit.

\\{prat-show-tree-mode-map}\n")


(defun prat-show-file-tree (treeish)
  (let* ((cmd (format "git ls-tree --name-only -r %s" treeish))
         (buf (get-buffer-create "*git ls-tree*"))
         (inhibit-read-only t))

    (with-current-buffer buf
      (erase-buffer)
      (prat-show-tree-mode)
      (insert "Repo: " default-directory "\n\n")
      (insert "> " cmd "\n\n")
      (setq-local shell-command cmd
                  output-marker (point)
                  prat-treeish treeish)
      (prat-refresh-file-tree))
    (switch-to-buffer buf)))


(defun prat-refresh-file-tree ()
  "Update the file tree in the current buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (setq-local refresh t)
    (prat-async-shell-command shell-command #'prat-refresh-file-tree-1)))


(defun prat-refresh-file-tree-1 (buf start end complete)
  (unless complete
    (let ((new-output (with-current-buffer buf
                        (buffer-substring-no-properties start end)))
          (inhibit-read-only t)
          pos)
      (when refresh
        (delete-region output-marker (point-max))
        (setq-local refresh nil))
      (setq pos (point-max))
      (save-excursion
        (goto-char pos)
        (insert new-output)
        (ansi-color-apply-on-region pos (point-max))
        (prat-commit-graph--add-text-properties pos (point-max))))))


(defun prat-show-tree--show-file (treeish filename)
  (interactive (list prat-treeish
                     (buffer-substring
                      (save-excursion (forward-line 0) (point))
                      (save-excursion (forward-line 1) (1- (point))))))
  (let* ((bufname (format "%s in %s" filename treeish))
         (buf (get-buffer-create bufname))
         (cmd `("git" "show" ,(format "%s:%s" treeish filename)))
         (inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer)
      (prat-base-mode)
      (let ((buffer-file-name filename))
        (set-auto-mode))
      (prat-async-shell-command cmd 'prat-show-tree--show-file-1))

    (switch-to-buffer buf)))


(defun prat-show-tree--show-file-1 (buf beg end commit)
  (when commit
    (save-excursion (insert (with-current-buffer buf
                              (buffer-substring beg end))))
    (set-buffer-modified-p nil)))


(provide 'prat-tree)
