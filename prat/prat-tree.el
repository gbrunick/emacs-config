(defvar prat-show-tree-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'prat-show-file)
    (define-key map "g" 'prat-refresh-file-tree)
    map)
  "The keymap used when viewing the files associated with a commit.")

(define-derived-mode prat-show-tree-mode prat-base-mode
  "File List"
  "Mode for buffers displaying the files in a Git commit/tree.
\\{prat-show-tree-mode-map}")


(defvar-local prat-treeish nil
  "The current treeish being displayed by `prat-show-file-tree'")

(defun prat-show-file-tree (treeish)
  (with-current-buffer (prat-shell-command
                        (format "git ls-tree --name-only -r %s" treeish)
                        "*File Tree*"
                        (format "%s in %s" treeish default-directory)
                        #'prat-show-tree-mode)
    (setq-local prat-treeish treeish)))

(defun prat-show-file (treeish filename)
  "Show FILE in TREEISH"
  (interactive (list (or prat-treeish "HEAD")
                     (thing-at-point 'filename)))
  (let* ((bufname (format "%s in %s" filename treeish))
         (buf (get-buffer-create bufname))
         (cmd (format "git show %s:%s" treeish filename))
         (inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer)
      (prat-base-mode)
      (let ((buffer-file-name filename))
        (set-auto-mode))
      (setq-local header-line-format (concat "> " cmd))
      (prat-async-shell-command cmd 'prat-show-file-1))

    (switch-to-buffer buf)))

(defun prat-show-file-1 (buf beg end commit)
  (when commit
    (save-excursion
      (let ((inhibit-read-only t))
        (insert (with-current-buffer buf (buffer-substring beg end)))
        (set-buffer-modified-p nil)))))


(provide 'prat-tree)
