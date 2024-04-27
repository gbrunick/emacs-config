(defvar prat-show-tracing-info t
  "When true, we write tracing info into a tracing buffer.

See also `prat-tracing-buffer-name'")

(defvar prat-tracing-buffer-name "*trace prat-exec-async*"
  "The name of the buffer used to hold tracing information.")

(defun prat-blend-colors (c1 c2 &optional alpha1 alpha2)
  "Blend the two colors C1 and C2 with ALPHA."
  (let ((alpha1 (or alpha1 0.5))
        (alpha2 (or alpha2 (- 1 alpha1))))
    (apply 'format "#%02x%02x%02x"
           (cl-mapcar (lambda (x y) (* 256 (+ (* alpha1 x) (* alpha2 y))))
                      (color-name-to-rgb c1)
                      (color-name-to-rgb c2)))))

(defvar prat-repo-dir-history nil
  "This symbol is used to remember the history of repository roots.")

(defun prat-repo-root-p (dir)
  (file-exists-p (concat (file-name-as-directory dir) ".git")))

(defun prat-find-repo-root (&optional dir)
  "Find the root of the Git repository.
Looks for the .git directory rather than calling Git."
  (let ((dir (file-name-as-directory (or dir default-directory))) next)
    (while (and dir (not (file-exists-p (concat dir ".git"))))
      (setq next (file-name-directory (directory-file-name dir))
            dir (if (string= next dir) nil next)))
    dir))

(defun prat-read-repo-dir (&optional force)
  "Prompt the user for a Git repository directory.

Maintains a separate history list from `read-directory-name'.
When FORCE is true, , we always prompt the user for the root
directory."
  (let ((repo-root (prat-find-repo-root default-directory)))
    (if (and repo-root (not force))
        repo-root
      (let* ((file-name-history (cl-copy-list prat-repo-dir-history))
             (repo-dir (read-directory-name "Repo root: " default-directory
                                            default-directory nil "")))
        (unless (prat-repo-root-p repo-dir)
          (user-error "Invalid Git repo dir: %s" repo-dir))
        (setq prat-repo-dir-history file-name-history)
        repo-dir))))


(defun prat-center-string (txt)
  (let ((indent (max (- (/ (- (window-width) (length txt)) 2) 1) 0)))
    (concat (make-string indent ?\ )
            (propertize txt 'face '(:weight bold)))))


(defun prat-get-new-buffer (prefix suffix)
  "Get a new buffer whose name starts with and PREFIX ends with SUFFIX.

Returns buffers with names of the form PREFIX<i>SUFFIX."
  (let ((i 1))
    (while (get-buffer (concat prefix "<" (int-to-string i) ">" suffix))
      (cl-incf i))
    (get-buffer-create (concat prefix "<" (int-to-string i) ">" suffix))))


(defun prat-abbreviate-file-name (dir)
  (dolist (remote-dir prat-remote-home-dirs)
    (when (string-prefix-p remote-dir dir)
      (cl-assert (file-remote-p remote-dir))
      (setq dir (replace-regexp-in-string (regexp-quote remote-dir)
                                          (file-remote-p remote-dir)
                                          dir))))
  dir)


(defun prat-trace-funcall (&optional func args)
  "Write tracing output to buffer."
  (when prat-show-tracing-info
    (let* ((buf (current-buffer))
           (bufname prat-tracing-buffer-name)
           ;; If the nesting changes, the NFRAMES may change.
           (outer-call (backtrace-frame 5))
           (func (cadr outer-call))
           (args (cddr outer-call)))
      (with-current-buffer (get-buffer-create bufname)
        (setq truncate-lines t)
        (let ((args-string (mapconcat (lambda (y)
                                        (truncate-string-to-width
                                         (prin1-to-string y) 1000 nil nil t))
                                      args "\n  ")))
          (save-excursion
            (goto-char (point-max))
            (unless (bobp) (insert "\n"))
            (insert (format "%S called in buffer %S\n  %s\n"
                            func (buffer-name buf) args-string))))))))


(defun prat-insert-placeholder (text)
  (add-text-properties
   (point)
   (progn
     (insert text)
     (insert " ")
     (prat-insert-spinner)
     (point))
   '(face (background-color . "light gray"))))


(defun prat-delete-placeholder (text)
  (goto-char (point-min))
  (re-search-forward text)
  (delete-region (match-beginning 0) (progn (forward-line 1) (point))))



(defvar prat-temporary-dirs nil
  "One temporary directory per remote")

(defun prat-get-temporary-dir (path)
  (let* ((default-directory (file-name-directory path))
         (remote (or (file-remote-p path) 'local))
         (key-value (assoc remote prat-temporary-dirs))
         (tmpdir (cdr key-value)))
    ;; `file-directory-p' checks for existence.
    (unless (and tmpdir (file-directory-p tmpdir))
      (setq tmpdir (file-name-as-directory (make-nearby-temp-file nil t)))
      (push (cons remote tmpdir) prat-temporary-dirs))
    tmpdir))

(defun prat-get-temporary-file (name)
  (concat (prat-get-temporary-dir default-directory) name))

(defun prat-insert-spinner ()
  "Insert spinner at current point."
  (let ((m (copy-marker (point))))
    (set-marker-insertion-type m nil)
    (insert (propertize "|" 'spinner t 'sequence '("/" "-" "\\" "|")))
    (set-marker-insertion-type m t)
    (run-at-time 0.5 nil 'prat-insert-spinner--spin m)
    m))

(defun prat-insert-spinner--spin (m)
  "Implementation detail of `prat-insert-spinner'"
  (let ((buf (marker-buffer m)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (ignore-errors (get-text-property m 'spinner))
          (let* ((seq (get-text-property m 'sequence))
                 (next-seq (append (cdr seq) (list (car seq))))
                 (inhibit-read-only t)
                 props)
            (save-excursion
              (goto-char m)
              (setq props (text-properties-at m))
              (plist-put props 'sequence next-seq)
              (set-marker-insertion-type m nil)
              (insert (apply 'propertize (car seq) props))
              (set-marker-insertion-type m t)
              (delete-region (+ m 1) (+ m 2))))
          (run-at-time 0.5 nil 'prat-insert-spinner--spin m))))))



(provide 'prat-util)


