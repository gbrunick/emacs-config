(require 'seq)
(require 'prat-shell-commands)
(require 'prat-stash)

(defvar prat-show-status-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'forward-button)
    (define-key map [(backtab)] 'backward-button)
    (define-key map "g" 'prat-show-status--refresh)
    (define-key map "m" 'prat-show-status--mark-file)
    (define-key map "u" 'prat-show-status--unmark-file)
    (define-key map "a" 'prat-show-status--add-files)
    (define-key map "r" 'prat-show-status--reset-files)
    (define-key map "s" 'prat-show-status--stash-files)
    (define-key map "!" 'prat-shell-command)
    map)
  "The keymap used when viewing git status output.")


(define-derived-mode prat-show-status-mode special-mode
  "Git Status"
  "\nMode for buffers displaying Git status output.

\\{prat-show-status-mode-map}\n"
  (read-only-mode 1)
  (setq tab-width 4))


(defun prat-show-status (&optional repo-dir)
  "Show the current Git status in a buffer."
  (interactive)
  (let* ((repo-dir (or repo-dir (prat-find-repo-root)))
         (buf (get-buffer-create prat-status-buffer-name))
         (inhibit-read-only t))

    (with-current-buffer buf
      (erase-buffer)
      (prat-erase-overlays)
      (prat-show-status-mode)
      (setq default-directory repo-dir)
      (setq-local prat-status-command
                  "git -c advice.statusHints=false status --show-stash")
      (insert (format "Status in %s\n\n> git status --show-stash\n\n" repo-dir))
      (setq-local prat-status-output-marker (point))
      (prat-insert-placeholder "Loading status")
      (prat-show-status--refresh))

    (switch-to-buffer buf)))


(defun prat-show-status--refresh (&rest args)
  "Update the current Git status buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (prat-async-shell-command
     prat-status-command default-directory #'prat-show-status--refresh-1)))


(defun prat-show-status--refresh-1 (buf start end complete)
  "Implementation detail of `prat-show-status--refresh'.

Asyncronous callback that add buttons and overlays to the Git
status output."
  (prat-log-call)
  (when complete
    (let ((status-text (with-current-buffer buf
                         (buffer-substring-no-properties start end)))
          (inhibit-read-only t))
      (delete-region prat-status-output-marker (point-max))
      (goto-char prat-status-output-marker)
      (save-excursion
        (insert status-text)
        (goto-char prat-status-output-marker)
        (prat-show-status--markup-output)))))


(defun prat-show-status--markup-output ()
  "Markup git status output in the current buffer."

  ;; Some version of Git (e.g. 1.8.3.1) prefix the status output with
  ;; comment characters.
  (save-excursion
    (while (re-search-forward "^# ?" nil t)
      (replace-match "")))

  (when (re-search-forward "Changes to be committed:" nil t)
    (insert " (")
    (insert-text-button "view" 'action 'prat-show-staged-changes)
    (insert ")")
    (forward-line 1)

    (while (looking-at "^\t[^\t]")
      (let* ((regex (concat "^\t\\(deleted:\\|modified:"
                            "\\|new file:\\|renamed:\\)?"
                            " *\\(.*\\)$")))

        ;; Put an overlay across the whole line for marking.
        (prat-make-overlay (point)
                           (progn (re-search-forward regex)
                                  (forward-line 1)
                                  (point))
                           'prat-state "staged"
                           'prat-filename (match-string 2))

        ;; Put a button on the filename itself.
        (make-text-button
         (match-beginning 2) (match-end 2)
         'action 'prat-show-status--show-staged-file-diff
         'filename (match-string 2)))))

  (when (re-search-forward "Changes not staged for commit:" nil t)
    (insert " (")
    (insert-text-button
     "view" 'action 'prat-show-unstaged-changes)
    (insert ")")
    (forward-line 1)

    (while (looking-at "^\t[^\t]")
      (let* ((regex (concat "^\t\\(deleted:\\|modified:\\|new file:\\)?"
                            " *\\(.*\\)$")))

        ;; Put an overlay across the whole line for marking.
        (prat-make-overlay (point)
                           (progn (re-search-forward regex)
                                  (forward-line 1)
                                  (point))
                           'prat-state "unstaged"
                           'prat-filename (match-string 2))

        ;; Put a button on the filename itself.
        (make-text-button (match-beginning 2) (match-end 2)
                          'action
                          'prat-show-status--show-unstaged-file-diff
                          'filename (match-string 2)))))

  (when (re-search-forward "Untracked files:" nil t)
    (forward-line 1)

    (while (looking-at "^\t[^\t]")
      (let* ((regex (concat "^\t\\(.*\\)$")))

        ;; Put an overlay across the whole line for marking.
        (prat-make-overlay (point)
                           (progn (re-search-forward regex)
                                  (forward-line 1)
                                  (point))
                           'prat-state "untracked"
                           'prat-filename (match-string 1)))))

  (when (re-search-forward "Your stash currently has" nil t)
    (end-of-line)
    (insert " (")
    (insert-text-button "view" 'action 'prat-show-stash-list)
    (insert ")"))

  (untabify (point-min) (point-max)))

(defun prat-show-status--forward-filename (n)
  (interactive "p")
  (while (> n 0)
    (goto-char (next-single-char-property-change (point) 'prat-filename))
    ;; If we moved forward to a line with no filename, move forward one
    ;; more time.
    (unless (or (eobp) (get-char-property (point) 'prat-filename))
      (goto-char (next-single-char-property-change (point) 'prat-filename)))
    (cl-decf n))

  (while (< n 0)
    (goto-char (previous-single-char-property-change (point) 'prat-filename))
    ;; If we moved back to a line with no filename, move forward one
    ;; more time.
    (unless (or (bobp) (get-char-property (point) 'prat-filename))
      (goto-char (next-single-char-property-change (point) 'prat-filename)))
    (cl-incf n)))


(defun prat-show-status--backward-filename (n)
  (interactive "p")
  (prat-show-status--backward-filename (- n)))


(defun prat-show-status--mark-file (&optional unmark)
  "Mark the file on the current line.
Unmarks the file if UNMARK is non-nil."
  (interactive)
  (cond
   ;; If the region is active, toggle each line in the region.
   ((use-region-p)
    (let ((beg (region-beginning))
          (end (region-end))
          (i 0))
      (deactivate-mark)
      (save-excursion
        (goto-char beg)
        (forward-line 0)
        (while (< (point) end)
          (if (ignore-errors (prat-show-status--mark-file unmark) t)
              (cl-incf i)
            (forward-line 1)))
        (if unmark (message "Unmarked %s files" i)
          (message "Marked %s files" i)))))

   (t
    (unless (get-char-property (point) 'prat-filename)
      (prat-show-status--forward-filename 1))

    (let* ((mark-line (not unmark))
           (overlay (prat-overlays-at (point) 'prat-filename :not-nil))
           filename file-state)
      (cl-assert (<= (length overlay) 1))
      (when (= (length overlay) 0) (error "No file at point."))
      (setq overlay (car overlay))

      (setq filename (overlay-get overlay 'prat-filename)
            file-state (overlay-get overlay 'prat-state))

      (overlay-put overlay 'prat-marked mark-line)
      (overlay-put overlay 'face (when mark-line 'prat-marked-line-face))

      ;; We can only perform one type of action at a time, so we remove any
      ;; conflicting marks.
      (when mark-line
        (let ((clear-states (pcase file-state
                              ("staged"    '("unstaged" "untracked"))
                              ("unstaged"  '("staged"))
                              ("untracked" '("staged")))))
          (dolist (ov (prat-filter-overlays (overlays-in (point-min) (point-max))
                                            'prat-state clear-states))
            (overlay-put ov 'prat-action nil)
            (overlay-put ov 'face nil))))

      ;; Emit an appropriate message
      (if mark-line
          (message "Marked %s" filename)
        (message "Umarked %s" filename))

      (prat-show-status--forward-filename 1)))))


(defun prat-show-status--unmark-file ()
  "Unmark the file on the current line."
  (interactive)
  (prat-show-status--mark-file 'unmark))


(defun prat-show-status--do-action (git-cmd file-states)
  "Perform GIT-CMD on marked files with state in FILE-STATES."
  (interactive)
  (when (region-active-p) (prat-show-status--mark-file))
  (let* ((marked-overlays (prat-filter-overlays
                           (overlays-in (point-min) (point-max))
                           'prat-marked :not-nil
                           'prat-state file-states)))

    ;; If not lines have been marked, operate on the current line if it has
    ;; the correct state.
    (unless marked-overlays
      (setq marked-overlays (prat-overlays-at (point) 'prat-state file-states)))

    (unless marked-overlays
      (user-error "No files to add"))

    (let* ((filenames (mapcar (lambda (ov) (overlay-get ov 'prat-filename))
                              marked-overlays))
           (quoted-filenames (mapconcat 'shell-quote-argument filenames " "))
           (cmd (format "git %s -- %s" git-cmd quoted-filenames)))

      (message "%s" cmd)
      (prat-async-shell-command cmd default-directory
                                'prat-show-status--refresh))))


(defun prat-show-status--add-files ()
  (interactive)
  (prat-show-status--do-action "add" '("unstaged" "untracked")))

(defun prat-show-status--reset-files ()
  (interactive)
  (prat-show-status--do-action "reset" '("staged")))

(defun prat-show-status--stash-files ()
  (interactive)
  (prat-show-status--do-action "stash -u" '("unstaged" "untracked")))


(defun prat-show-status--show-unstaged-file-diff (button)
  (let* ((filename (button-get button 'filename))
         (cmd (format "git diff -- \"%s\"" filename))
         (buf (get-buffer-create (format "*unstaged: %s*" filename)))
         (repo-dir default-directory))
    (prat-show-changes cmd nil buf 'prat-unstaged-changes-mode repo-dir)))


(defun prat-show-status--show-staged-file-diff (button)
  (let* ((filename (button-get button 'filename))
         (cmd (format "git diff --cached -- \"%s\"" filename))
         (buf (get-buffer-create (format "*staged: %s*" filename)))
         (repo-dir default-directory))
    (prat-show-changes cmd nil buf 'prat-unstaged-changes-mode repo-dir)))


(provide 'prat-status)
