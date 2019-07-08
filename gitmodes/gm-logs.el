(defvar gpb-git:commit-graph-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "m" 'gpb-git:mark-line)
    (define-key map "d" 'gpb-git--commit-graph-mode--show-diff)
    (define-key map "g" 'gpb-git:refresh-buffer)
    (define-key map (kbd "RET") 'gpb-git:show-commit-graph--show-commit)
    (define-key map "!" 'gpb-git:shell-command)
    (define-key map "f" 'gpb-git:show-commit-graph--show-files)
    map)
  "The keymap used when viewing the commit graph.")


(define-derived-mode gpb-git:commit-graph-mode special-mode
  "Commit Graph"
  "\nMode for buffers displaying the Git commit graph.

\\{gpb-git:commit-graph-mode-map}\n"
  (gpb-git--init-marked-line-overlay))


(defvar gpb-git:show-commit-files-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'gpb-git:show-commit-graph--show-file-version)
    map)
  "The keymap used when viewing the commit graph.")


(define-derived-mode gpb-git:show-commit-files-mode special-mode
  "File List"
  "\nMode for buffers displaying the files in a Git commit.

\\{gpb-git:show-commit-files-mode-map}\n")


(defun gpb-git:show-commit-graph (&optional repo-root)
  (interactive (list (gpb-git--read-repo-dir)))
  (let* ((buf (get-buffer-create "*git log*"))
         (marker1 (make-marker))
         (marker2 (make-marker))
         (cmd '("git" "log" "--graph" "--oneline" "--decorate" "--color"))
         (repo-root (or repo-root default-directory)))
    (with-current-buffer buf
      (setq default-directory repo-root)
      (gpb-git--refresh-commit-graph)
      (setq-local refresh-cmd `(gpb-git--refresh-commit-graph)))
    (switch-to-buffer buf)))


(defun gpb-git--refresh-commit-graph (&optional callback)
  (let ((cmd '("git" "log" "--graph" "--oneline" "--decorate"
               "--color" "--all"))
        (inhibit-read-only t))
    (read-only-mode 1)
    (erase-buffer)
    (gpb-git:commit-graph-mode)
    (insert (format "Repo: %s\n\n" default-directory))
    (insert (format "%s\n\n" (mapconcat 'identity cmd " ")))
    (save-excursion (gpb-git:insert-placeholder "Loading commits"))
    (gpb-git:exec-async cmd default-directory #'gpb-git--refresh-commit-graph-1
                        callback)))

(defun gpb-git--refresh-commit-graph-1 (buf callback)
  (let ((inhibit-read-only t))
    (gpb-git:delete-placeholder "Loading commits")
    (save-excursion
      (insert (with-current-buffer buf (buffer-string)))
      (ansi-color-apply-on-region (point-min) (point-max))
      (goto-char (point-min))
      (while (re-search-forward "^[* \\|/]+ \\([a-f0-9]+\\) " nil t)
        (add-text-properties (progn (forward-line 0) (point))
                             (progn (forward-line 1) (point))
                             `(:commit-hash ,(match-string 1)))))
    (setq-local refresh-cmd `(gpb-git--refresh-commit-graph))
    (when callback (funcall callback))))


(defun gpb-git:mark-line ()
  "Mark the the current line so other commands can refer to it."
  (interactive)
  (let* ((bol-pt (save-excursion (forward-line 0) (point)))
         (eol-pt (save-excursion (forward-line 1) (point))))
    (move-overlay marked-line-overlay bol-pt eol-pt)))


(defun gpb-git--get-marked-revision ()
  "Get the hash of the currently marked revision."
  (gpb-get--get-revision-at-point (overlay-start marked-line-overlay)))


(defun gpb-get--get-revision-at-point (&optional pt)
  "Find the revision hash on the current line."
  (if pt
      (save-excursion
        (goto-char pt)
        (gpb-get--get-revision-at-point))
    (save-excursion
      (forward-line 0)
      (skip-chars-forward "* |/\\")
      (unless (looking-at "\\([0-9a-f]+\\) ")
        (error "Invalid revision line"))
      (match-string 1))))


(defun gpb-git--commit-graph-mode--show-diff ()
  "Diff the marked revision with the revision at the point."
  (interactive)
  (gpb-git:show-commit-diff (gpb-git--get-marked-revision)
                            (gpb-get--get-revision-at-point)
                            default-directory))

(defun gpb-git--init-marked-line-overlay ()
  (let ((new-ov (make-overlay (point-min) (point-min))))
    (overlay-put new-ov 'face 'gpb-git:marked-line-face)
    (setq-local marked-line-overlay new-ov)))


(defun gpb-git:show-commit-graph--show-commit ()
  (interactive)
  (let ((hash (get-text-property (point) :commit-hash))
        buf)
    (unless hash (error "No commit on line"))
    (setq buf (get-buffer-create (format "*commit: %s*" hash)))
    (with-current-buffer buf
      (gpb-git--show-commit hash))
    (pop-to-buffer buf)))


(defun gpb-git:show-commit-graph--show-files ()
  (interactive)
  (let* ((hash (get-text-property (point) :commit-hash))
         (cmd `("git" "ls-tree" "--name-only" "-r" ,hash))
         (inhibit-read-only t)
         buf)
    (unless hash (error "No commit on line"))
    (setq buf (get-buffer-create (format "*ls-tree: %s*" hash)))
    (with-current-buffer buf
      (erase-buffer)
      (gpb-git:show-commit-files-mode)
      (setq-local git-commit-hash hash)
      (save-excursion
        (insert (format "%s\n\n" (mapconcat 'identity cmd " ")))
        (apply 'start-file-process "*git ls-tree*" buf cmd)))
    (pop-to-buffer buf)))


(defun gpb-git:show-commit-graph--show-file-version ()
  (interactive)
  (let* ((filename (buffer-substring
                    (save-excursion (forward-line 0) (point))
                    (save-excursion (forward-line 1) (1- (point)))))
         (cmd `("git" "show" ,(format "%s:%s" git-commit-hash filename)))
         buf)
    (unless filename (error "No file on line"))
    (setq buf (get-buffer-create (format "*%s: %s*" git-commit-hash filename)))
    (with-current-buffer buf
      (erase-buffer)
      (save-excursion
        (insert (format "%s\n\n" (mapconcat 'identity cmd " ")))
        (apply 'start-file-process "*git show*" buf cmd))
      (read-only-mode 1)
      (view-mode))
    (pop-to-buffer buf)))



(provide 'gm-logs)
