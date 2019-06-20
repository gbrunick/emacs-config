(defun gpb-git--blend-colors (c1 c2 &optional alpha1 alpha2)
  "Blend the two colors C1 and C2 with ALPHA."
  (let ((alpha1 (or alpha1 0.5))
        (alpha2 (or alpha2 (- 1 alpha1))))
    (apply 'format "#%02x%02x%02x"
           (cl-mapcar (lambda (x y) (* 256 (+ (* alpha1 x) (* alpha2 y))))
                      (color-name-to-rgb c1)
                      (color-name-to-rgb c2)))))


(defvar gpb-git--repo-dir-history nil
  "This symbol is used to remember the history of repository roots.")

(defun gpb-git--repo-root-p (dir)
  (file-exists-p (concat (file-name-as-directory dir) ".git")))

(defun gpb-git--find-repo-root (&optional dir)
  "Find the root of the Git repository.
Looks for the .git directory rather than calling Git."
  (let ((dir (file-name-as-directory (or dir default-directory))) next)
    (while (and dir (not (file-exists-p (concat dir ".git"))))
      (setq next (file-name-directory (directory-file-name dir))
            dir (if (string= next dir) nil next)))
    dir))

(defun gpb-git--read-repo-dir ()
  "Prompt the user for a Git repository directory.

Maintains a separate history list from `read-directory-name'."
  (let* ((dir (or (gpb-git--find-repo-root default-directory)
                  default-directory))
         (file-name-history (copy-list gpb-git--repo-dir-history))
         (repo-dir (read-directory-name "Repo root: " dir dir nil "")))
    (unless (gpb-git--repo-root-p repo-dir)
      (user-error "Invalid Git repo dir: %s" repo-dir))
    (setq gpb-git--repo-dir-history file-name-history)
    repo-dir))


(defun gpb-git--center-string (txt)
  (let ((indent (max (- (/ (- (window-width) (length txt)) 2) 1) 0)))
    (concat (make-string indent ?\ )
            (propertize txt 'face '(:weight bold)))))


(defun gpb-git--get-new-buffer (prefix suffix)
  "Get a new buffer whose name starts PREFIX with and ends with SUFFIX."
  (if (null (get-buffer (concat prefix suffix)))
      (get-buffer-create (concat prefix suffix))
    (let ((i 2))
      (while (get-buffer (concat prefix "<" (int-to-string i) ">" suffix))
        (incf i))
      (get-buffer-create (concat prefix "<" (int-to-string i) ">" suffix)))))


(defun gpb-git--abbreviate-file-name (dir)
  (dolist (remote-dir gpb-git:remote-home-dirs)
    (when (string-prefix-p remote-dir dir)
      (assert (file-remote-p remote-dir))
      (setq dir (replace-regexp-in-string (regexp-quote remote-dir)
                                          (file-remote-p remote-dir)
                                          dir))))
  dir)


(defun gpb-git--trace-funcall (func args)
  "Write tracing output to buffer."
  (let ((buf (current-buffer))
        (bufname "*trace gpb-git:exec-async*"))
    (unless (get-buffer bufname)
      (with-current-buffer (get-buffer-create bufname)
        (setq truncate-lines t)))
    (with-current-buffer (get-buffer bufname)
      (let ((args-string (mapconcat (lambda (y)
                                      (truncate-string-to-width
                                       (prin1-to-string y) 1000 nil nil t))
                                    args "\n  ")))
        (save-excursion
          (goto-char (point-max))
          (unless (bobp) (insert "\n"))
          (insert (format "%S in %S:\n  %s\n" func buf args-string)))))))


(defun gpb-git:insert-placeholder (text)
  (add-text-properties
   (point)
   (progn
     (insert text)
     (gpb-git:insert-spinner)
     (point))
   '(face (background-color . "light gray"))))


(defun gpb-git:delete-placeholder (text)
  (goto-char (point-min))
  (re-search-forward text)
  (delete-region (match-beginning 0) (progn (forward-line 1) (point))))




(provide 'gm-util)


