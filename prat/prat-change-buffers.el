;;
;; prat-change-buffers.el
;;
;; Function to show changes/diffs in a buffer.
;;

;; Base modes

(defvar prat-hunk-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'prat-forward-command)
    (define-key map [(backtab)] 'prat-backward-command)
    (define-key map "p" 'prat-backward-command)
    (define-key map "P" 'prat-backward-file-command)
    (define-key map "n" 'prat-forward-command)
    (define-key map "N" 'prat-forward-file-command)
    (define-key map (kbd "RET") 'prat-goto-line)
    (define-key map "g" 'revert-buffer)
    (fset 'prat-hunk-view-mode-map map)
    map)
  "Base keymap for hunk viewing.")

(define-derived-mode prat-hunk-view-mode prat-base-mode
  "Hunk Buffer"
  "\nBase mode for buffers showing hunks."
  (setq-local header-line-format '(:eval (prat-compute-hunk-buffer-header)))
  (setq-local buffer-read-only t)
  (setq-local tab-width 4)
  (setq-local revert-buffer-function 'prat-revert-changes-buffer))

(defvar prat-hunk-selection-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "m" 'prat-mark-hunk-command)
    (define-key map "M" 'prat-mark-file-command)
    (define-key map "r" 'prat-mark-as-rename)
    (define-key map "u" 'prat-unmark-hunk-command)
    (define-key map "U" 'prat-unmark-file-command)
    (set-keymap-parent map 'prat-hunk-view-mode-map)
    (fset 'prat-hunk-selection-mode-map map)
    map)
  "Base keymap for hunk viewing and selection.")

(define-derived-mode prat-hunk-selection-mode prat-hunk-view-mode
  "Hunk Buffer"
  "\nBase mode for buffers showing hunks."
  (add-hook 'post-command-hook 'prat-post-command-hook))


;; Unstaged changes

(defvar prat-unstaged-changes-mode-map
  (let ((map (make-sparse-keymap)))
    ;; "add" new files to the index.
    (define-key map "a" 'prat-stage-hunks)
    ;; "delete" changes.
    (define-key map "d" 'prat-revert-marked-hunks)
    (define-key map "w" 'prat-toggle-whitespace-diff-args)
    (set-keymap-parent map 'prat-hunk-selection-mode-map)
    (fset 'prat-unstaged-changes-mode-map map)
    map)
  "The keymap used for unstaged hunks.")

(define-derived-mode prat-unstaged-changes-mode prat-hunk-selection-mode
  "Unstaged Changes"
  "\nMode for selecting unstages changes to be added to the index.

\\{prat-unstaged-changes-mode-map}\n"
  (setq-local staged-changes-buffer nil))

(defun prat-show-unstaged-changes (&optional cmd buf repo-dir)
  (interactive)
  (let ((cmd (or cmd "git diff --histogram --find-renames"))
        (desc (format "Unstaged changes in %s" (prat-abbreviate-file-name
                                                default-directory)))
        (buf (or buf (get-buffer-create prat-staged-buffer-name))))
    (prat-show-changes cmd desc buf 'prat-unstaged-changes-mode repo-dir)))


;; Staged Changes

(defvar prat-staged-changes-mode-map
  (let ((map (make-sparse-keymap)))
    ;; "reset" marked hunks
    (define-key map "r" 'prat-unstage-hunks)
    (set-keymap-parent map 'prat-hunk-selection-mode-map)
    (fset 'prat-staged-changes-mode-map map)
    map)
  "The keymap used for staged hunks.")

(define-derived-mode prat-staged-changes-mode prat-hunk-selection-mode
  "Staged Changes"
  "\nMode for selecting stages changes to be removed from the index.

\\{prat-staged-changes-mode-map}\n"
  (setq-local staged-changes-buffer t))

(defun prat-show-staged-changes (&optional cmd buf repo-dir)
  (interactive)
  (let ((cmd (or cmd "git diff --cached --histogram --find-renames"))
        (desc (format "Staged changes in %s" (prat-abbreviate-file-name
                                              default-directory)))
        (buf (or buf (get-buffer-create prat-staged-buffer-name))))
    (prat-show-changes cmd desc buf 'prat-staged-changes-mode repo-dir)))


(defun prat-show-commit (hash &optional repo-dir callback)
  "Write information about the commit HASH into the current buffer."
  (let ((cmd (format "git show %s --" hash))
        (desc (format "Commit %s" hash))
        (buf (get-buffer-create (format "*commit: %s*" hash))))
    (prat-show-changes cmd nil buf 'prat-hunk-view-mode repo-dir)))


(defun prat-show-commit-diff (hash1 hash2 &optional repo-dir)
  "Display the changes from HASH1 to HASH2."
  (let ((cmd (format "git diff --stat --patch %s %s %s --"
                     "--histogram --find-renames" hash1 hash2))
        (buf (get-buffer-create (format "*%s...%s*" hash1 hash2))))
    (prat-show-changes cmd desc buf 'prat-hunk-view-mode repo-dir)))


(defun prat-show-changes (cmd desc buf major-mode &optional repo-dir)
  (interactive)
  (let ((repo-dir (or repo-dir (prat-find-repo-root)))
        (buffer-mode major-mode)
        (inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer)
      ;; Delete any existing hunk overlays in the buffer. Use evaporate?
      (dolist (ov (prat-get-hunk-overlays)) (delete-overlay ov))
      (funcall buffer-mode)
      (setq default-directory repo-dir)
      (when desc (insert (format "%s\n\n" desc)))
      (insert (format "%s\n\n" cmd))
      (setq-local prat-change-buffer-info
                  (list :cmd cmd :repo-dir repo-dir :output-start (point)))
      (prat-insert-placeholder "Loading hunks")
      (prat-async-shell-command cmd repo-dir #'prat-show-changes-1))
    (switch-to-buffer buf)))


(defun prat-show-changes-1 (buf start end complete)
  "Implementation detail of `prat-show-changes'."
  (prat-log-call)
  (when complete
    (let ((info (with-current-buffer buf
                  (save-excursion
                    (goto-char start)
                    ;; git show has info in front of the diff hunks.
                    (when (re-search-forward "^diff --" nil t)
                      (buffer-substring-no-properties
                       start (match-beginning 0))))))
          (hunks (with-current-buffer buf (prat-parse-diff buf start end)))
          (inhibit-read-only t))
      (goto-char (prat-delete-placeholder))
      (when info (insert info))
      (cond
       (hunks
        ;; Add a text button for each filename
        (insert "Files: ")
        (let* ((filenames (mapcar (lambda (hunk) (prat-aget hunk :filename1))
                                  hunks))
               (max-length (apply 'max (mapcar 'length filenames))))
          (dolist (filename (sort (delete-dups (delq nil filenames)) 'string<))
            (make-text-button (point)
                              (progn (insert filename) (point))
                              'action 'prat-jump-to-file-hunks
                              'filename filename)
            (insert (make-string (max (- max-length (length filename)) 0)
                                 ?\ ))
            (let ((file-hunks (cl-remove-if-not
                               (lambda (h)
                                 (equal (prat-aget h :filename1) filename))
                               hunks)))
              (insert (cond
                       ((= (length file-hunks) 1) " (1 hunk)")
                       (t (format " (%s hunks)"
                                  (length file-hunks))))))
            (insert "\n       ")))
        (forward-line 0)
        (delete-region (point) (line-end-position))
        (insert "\n\n")
        (prat-insert-hunks hunks))
       (t
        (insert "No changes")))
      (goto-char (point-min)))))


(defun prat-revert-changes-buffer (ignore-auto noconfirm)
  (let ((cmd (plist-get prat-change-buffer-info :cmd))
        (repo-dir (plist-get prat-change-buffer-info :repo-dir))
        (output-start (plist-get prat-change-buffer-info :output-start))
        (inhibit-read-only t))
    (save-excursion
      (goto-char output-start)
      (delete-region output-start (point-max))
      (prat-insert-placeholder "Updating hunks")
      (prat-async-shell-command cmd repo-dir #'prat-show-changes-1))))


(provide 'prat-change-buffers)
