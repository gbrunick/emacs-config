(defvar gpb-git:commit-graph-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "m" 'gpb-git:mark-line)
    (define-key map "d" 'gpb-git--commit-graph-mode--show-diff)
    (define-key map "g" 'gpb-git:refresh-buffer)
    (define-key map (kbd "RET") 'gpb-git:show-commit-graph--show-commit)
    (define-key map "!" 'gpb-git:shell-command)
    (define-key map "\C-c\C-f" 'gpb-git:show-commit-graph--show-files)
    map)
  "The keymap used when viewing the commit graph.")


(define-derived-mode gpb-git:commit-graph-mode special-mode
  "Commit Graph"
  "\nMode for buffers displaying the Git commit graph.

\\{gpb-git:commit-graph-mode-map}\n"
  (gpb-git--init-marked-line-overlay)
  (setq truncate-lines t))


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
         (repo-root (or repo-root default-directory)))
    (with-current-buffer buf
      (setq default-directory repo-root)
      (gpb-git--refresh-commit-graph)
      (setq-local refresh-cmd `(gpb-git--refresh-commit-graph)))
    (switch-to-buffer buf)))


(defcustom gpb-git--log-excluded-branches nil
  "Add globs to this list to exclude branches from git log output."
  :type '(repeat (string :tag "Commmand"))
  :group 'gpb-git)

(defun gpb-git--refresh-commit-graph (&optional callback)
  (gpb-git--trace-funcall)
  (let ((cmd "git log --graph --oneline --decorate --color")
        (inhibit-read-only t))
    (dolist (glob gpb-git--log-excluded-branches)
      (setq cmd (format "%s --exclude=\"%s\"" cmd glob)))
    (setq cmd (format "%s --all" cmd))
    (read-only-mode 1)
    (erase-buffer)
    (remove-overlays)
    (gpb-git:commit-graph-mode)
    (save-excursion
      (insert (format "Repo: %s\n\n" default-directory))
      (insert (format "%s\n\n" cmd))
      (setq-local output-marker (copy-marker (point))))
    (setq-local callback-func callback)
    (gpb-git:async-shell-command
     cmd default-directory #'gpb-git--refresh-commit-graph-1)))

(defun gpb-git--refresh-commit-graph-1 (buf start end complete)
  (gpb-git--trace-funcall)
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char output-marker)
      (save-excursion
        (insert (with-current-buffer buf
                  (buffer-substring-no-properties start end)))
        (ansi-color-apply-on-region output-marker (point))
        (move-marker output-marker (point)))
      (while (re-search-forward "^[* \\|/-]+\\.? +\\([a-f0-9]+\\) " nil t)
        (add-text-properties (progn (forward-line 0) (point))
                             (progn (forward-line 1) (point))
                             `(:commit-hash ,(match-string 1)))))
    (setq-local refresh-cmd `(gpb-git--refresh-commit-graph))
    (when (and complete callback-func) (funcall callback-func))))


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
      (skip-chars-forward "* |/\\-\.")
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

    (setq buf (gpb-git:shell-command (mapconcat 'identity cmd " ")
                                     (format "*ls-tree: %s*" hash)))
    (with-current-buffer buf
      (goto-char (point-min))
      (gpb-git:show-commit-files-mode)
      (setq-local git-commit-hash hash))))


(defun gpb-git:show-commit-graph--show-file-version ()
  (interactive)
  (let* ((filename (buffer-substring
                    (save-excursion (forward-line 0) (point))
                    (save-excursion (forward-line 1) (1- (point)))))
         (cmd `("git" "show" ,(format "%s:%s" git-commit-hash filename)))
         (inhibit-read-only t)
         buf)
    (unless filename (error "No file on line"))
    (setq buf (gpb-git:shell-command
               (mapconcat 'identity cmd " ")
               (format "*%s: %s*" git-commit-hash filename)))
    (with-current-buffer buf (goto-char (point-min)))))


(defun gpb-git:show-file-history (&optional filename)
  (interactive)
  (let* ((filename (or filename (buffer-file-name)))
         (basename (file-name-nondirectory filename))
         (cmd `("git" "log" "--follow" "-p" "--" ,basename))
         (buf (get-buffer-create (format "*git log: %s*" basename)))
         (dir default-directory)
         (inhibit-read-only t))
    (with-current-buffer buf
      (setq buffer-read-only t
            default-directory dir)
      (erase-buffer)
      (insert (format "%s\n\n" (mapconcat 'identity cmd " ")))
      (apply 'process-file (car cmd) nil t t (cdr cmd))
      (diff-mode))
    (pop-to-buffer buf)))


(provide 'gm-logs)
