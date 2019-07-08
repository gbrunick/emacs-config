(defvar gpb-git:show-status-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'forward-button)
    (define-key map [(backtab)] 'backward-button)
    (define-key map "g" 'gpb-git:show-status--refresh)
    (define-key map "m" 'gpb-git:show-status--mark-file)
    (define-key map "u" 'gpb-git:show-status--unmark-file)
    (define-key map "a" 'gpb-git:show-status--stage-marked-files)
    (define-key map "r" 'gpb-git:show-status--unstage-marked-files)
    (define-key map "d" 'gpb-git:show-status--show-diff)
    (define-key map "!" 'gpb-git:shell-command)
    map)
  "The keymap used when viewing git status output.")


(define-derived-mode gpb-git:show-status-mode special-mode
  "Git Status"
  "\nMode for buffers displaying Git status output.

\\{gpb-git:show-status-mode-map}\n"
  (read-only-mode 1)
  (setq tab-width 4)

  ;; Delete any existing filename overlays.
  (mapcar (lambda (ov) (let ((fn (overlay-get ov 'filename)))
                         (when fn (delete-overlay ov))))
          (overlays-in (point-min) (point-max))))


(defun gpb-git:show-status (&optional repo-dir)
  "Show the current Git status in a buffer."
  (interactive (list (gpb-git--read-repo-dir)))
  (let* ((repo-dir (or repo-dir default-directory))
         (buf (get-buffer-create gpb-git:status-buffer-name)))

    (with-current-buffer buf
      (gpb-git:show-status-mode)
      (setq default-directory repo-dir)
      (gpb-git:show-status--refresh))

    (switch-to-buffer buf)))


(defun gpb-git:show-status--refresh (&optional buf)
  "Update the current Git status buffer."
  (interactive)
  (let* ((buf (or buf
                  (and (derived-mode-p 'gpb-git:show-status-mode)
                       (current-buffer))
                  (get-buffer gpb-git:status-buffer-name)))
         (cmd `("git" "-c" "advice.statusHints=false" "status" "-u"))
         (inhibit-read-only t)
         pt)

    (when buf
      (with-current-buffer buf
        (setq pt (point))
        (dolist (ov (gpb-git:show-status--get-overlays 'filename))
          (delete-overlay ov))
        (erase-buffer)
        (save-excursion
          (insert (format "Repo: %s\n\n%s\n\n"
                          (gpb-git--abbreviate-file-name default-directory)
                          (mapconcat 'identity cmd " ")))
          (setq-local original-point pt)
          (setq-local put-status-here (point))
          (gpb-git:exec-async cmd default-directory
                              #'gpb-git:show-status--refresh-1))))))


(defun gpb-git:show-status--refresh-1 (output-buffer)
  "Implementation detail of `gpb-git:show-status--refresh'.

Asyncronous callback that add buttons and overlays to the Git
status output."
  (save-excursion
    (let ((status-text (with-current-buffer output-buffer (buffer-string)))
          (inhibit-read-only t))

      (goto-char put-status-here)
      (insert status-text)
      (goto-char put-status-here)
      (when (re-search-forward "Changes to be committed:" nil t)
        (insert " (")
        (insert-text-button "view all"
                            'action 'gpb-git:show-status--show-staged-changes
                            'repo-dir default-directory)
        (insert ")")
        (forward-line 1)

        (while (looking-at "^\t[^\t]")
          (let* ((regex (concat "^\t\\(deleted:\\|modified:"
                                "\\|new file:\\|renamed:\\)?"
                                " *\\(.*\\)$"))
                 (ov (make-overlay (point)
                                   (progn (re-search-forward regex)
                                          (forward-line 1)
                                          (point))))
                 (filename (match-string 2)))

            (overlay-put ov 'staged t)
            (overlay-put ov 'filename filename)
            (make-text-button (match-beginning 2) (match-end 2)
                              'action 'gpb-git:show-status--show-staged-file-diff
                              'filename filename))))

      (when (re-search-forward "Changes not staged for commit:" nil t)
        (insert " (")
        (insert-text-button
         "view all" 'action 'gpb-git:show-status--show-unstaged-changes
         'repo-dir default-directory)
        (insert ")")
        (forward-line 1)

        (while (looking-at "^\t[^\t]")
          (let* ((regex (concat "^\t\\(deleted:\\|modified:\\|new file:\\)?"
                                " *\\(.*\\)$"))
                 (ov (make-overlay (point)
                                   (progn (re-search-forward regex)
                                          (forward-line 1)
                                          (point))))
                 (filename (match-string 2)))

            (overlay-put ov 'unstaged t)
            (overlay-put ov 'filename filename)
            (make-text-button (match-beginning 2) (match-end 2)
                              'action
                              'gpb-git:show-status--show-unstaged-file-diff
                              'filename filename))))

      (when (re-search-forward "Untracked files:" nil t)
        (forward-line 1)

        (while (looking-at "^\t[^\t]")
          (let* ((regex (concat "^\t\\(.*\\)$"))
                 (ov (make-overlay (point)
                                   (progn (re-search-forward regex)
                                          (forward-line 1)
                                          (point))))
                 (filename (match-string 1)))

            (overlay-put ov 'untracked t)
            (overlay-put ov 'filename filename))))


      (untabify (point-min) (point-max))))
  (goto-char original-point)
  (forward-line 0))


(defun gpb-git:show-status--mark-file ()
  "Mark the the file on the current line so other commands can refer to it."
  (interactive)
  (cond
   ;; If the region is active, mark each line in the region.
   ((use-region-p)
    (let ((beg (region-beginning))
          (end (region-end))
          (i 0))
    (deactivate-mark)
    (save-excursion
      (goto-char beg)
      (forward-line 0)
      (while (< (point) end)
        (message "%S" (point))
        (if (ignore-errors (gpb-git:show-status--mark-file) t)
            (incf i)
          (forward-line 1)))
      (message "Marked %s files" i))))

   (t
    (let* ((filename-overlay (get-char-property-and-overlay (point) 'filename))
           (filename (car filename-overlay))
           (ov (cdr filename-overlay)))
      (unless ov (error "No file at point."))
      (overlay-put ov 'marked t)
      (overlay-put ov 'face 'gpb-git:marked-line-face)
      (overlay-put ov 'priority -100)

      ;; If you can mark an unstaged file, we unmark all staged files.
      (when (overlay-get ov 'unstaged)
        (mapcar (lambda (ov) (when (overlay-get ov 'staged)
                               (overlay-put ov 'marked nil)
                               (overlay-put ov 'face nil)))
                (overlays-in (point-min) (point-max))))

      ;; If you can mark a staged file, we unmark all unstaged files.
      (when (overlay-get ov 'staged)
        (mapcar (lambda (ov) (when (overlay-get ov 'unstaged)
                               (overlay-put ov 'marked nil)
                               (overlay-put ov 'face nil)))
                (overlays-in (point-min) (point-max))))


      (overlays-in (point-min) (point-max))
      (message "Marked %s" filename)
      (forward-line 1)))))


(defun gpb-git:show-status--unmark-file ()
  "Mark the the file on the current line so other commands can refer to it."
  (interactive)
  (let* ((filename-overlay (get-char-property-and-overlay (point) 'filename))
        (filename (car filename-overlay))
        (ov (cdr filename-overlay)))
    (unless ov (error "No file at point."))
    (overlay-put ov 'marked nil)
    (overlay-put ov 'face nil)
    (message "Unmarked %s" filename)
    (forward-line 1)))


(defun gpb-git:show-status--get-overlays (&rest keys)
  (let ((overlays (sort (overlays-in (point-min) (point-max))
                        (lambda (x y) (< (overlay-start x)
                                         (overlay-start y))))))
    (dolist (key (cons 'filename keys))
      (setq overlays (seq-filter (lambda (ov) (overlay-get ov key))
                                 overlays)))
    overlays))


(defun gpb-git:show-status--get-filenames (&rest keys)
  (mapcar (lambda (ov) (overlay-get ov 'filename))
          (apply 'gpb-git:show-status--get-overlays keys)))


(defun gpb-git:show-status--stage-files ()
  (interactive)
  (when (region-active-p) (gpb-git:show-status--mark-file))
  (let* ((filenames (append
                     (gpb-git:show-status--get-filenames 'unstaged 'marked)
                     (gpb-git:show-status--get-filenames 'untracked 'marked)))
         (cmd (apply 'list "git" "add" "--" filenames)))
    (message "Command: %S" cmd)
    (apply 'process-file (car cmd) nil nil nil (cdr cmd))
    (gpb-git:show-status)))


(defun gpb-git:show-status--unstage-files ()
  (interactive)
  (when (region-active-p) (gpb-git:show-status--mark-file))
  (let* ((filenames (gpb-git:show-status--get-filenames 'staged 'marked))
         (cmd (apply 'list "git" "reset" "--" filenames)))
    (message "Command: %S" cmd)
    (apply 'process-file (car cmd) nil nil nil (cdr cmd))
    (gpb-git:show-status)))


(defun gpb-git:show-status--show-staged-changes (button)
  (let ((buf (get-buffer-create gpb-git:staged-buffer-name))
        (repo-dir default-directory))
    (assert repo-dir)
    (with-current-buffer buf
      (gpb-git--refresh-staged-changes repo-dir))
    (pop-to-buffer buf)))


(defun gpb-git:show-status--show-unstaged-changes (button)
  (let ((buf (get-buffer-create gpb-git:unstaged-buffer-name))
        (repo-dir default-directory))
    (assert repo-dir)
    (with-current-buffer buf
      (gpb-git--refresh-unstaged-changes repo-dir))
    (pop-to-buffer buf)))


(defun gpb-git:show-status--show-diff ()
  (interactive)
  (when (use-region-p) (gpb-git:show-status--mark-file))
  (let ((unstaged-filenames (gpb-git:show-status--get-filenames
                             'unstaged 'marked))
        (staged-filenames (gpb-git:show-status--get-filenames
                           'staged 'marked))
        filenames staged)

    (when (and (null unstaged-filenames) (null staged-filenames))
      (gpb-git:show-status--mark-file)
      (setq unstaged-filenames (gpb-git:show-status--get-filenames
                                'unstaged 'marked))
      (setq staged-filenames (gpb-git:show-status--get-filenames
                              'staged 'marked)))

    (cond
     (staged-filenames
      (assert (null unstaged-filenames))
      (setq staged t)
      (setq filenames staged-filenames))
     (unstaged-filenames
      (assert (null staged-filenames))
      (setq staged nil)
      (setq filenames unstaged-filenames)))

    (when (null filenames) (error "No file at point"))

    (let ((repo-dir default-directory)
          (cmd (if staged
                   `("git" "diff" "--cached" "--" ,@filenames)
                 `("git" "diff" "--" ,@filenames)))
          (buf (get-buffer-create (if staged
                                      gpb-git:staged-buffer-name
                                    gpb-git:unstaged-buffer-name)))
          (inhibit-read-only t))

      (with-current-buffer buf
        (if staged
            (gpb-git--refresh-staged-changes repo-dir cmd)
          (gpb-git--refresh-unstaged-changes repo-dir cmd))
        (pop-to-buffer buf)))))


(defun gpb-git:show-status--show-unstaged-file-diff (button)
  (let* ((filename (button-get button 'filename))
         (cmd `("git" "diff" "--" ,filename))
         (buf (get-buffer-create (format "*staged: %s*" filename)))
         (repo-dir default-directory))
    (with-current-buffer buf
      (gpb-git--refresh-unstaged-changes repo-dir cmd))
    (pop-to-buffer buf)))


(defun gpb-git:show-status--show-staged-file-diff (button)
  (let* ((filename (button-get button 'filename))
         (cmd `("git" "diff" "--cached" "--" ,filename))
         (buf (get-buffer-create (format "*staged: %s*" filename)))
         (repo-dir default-directory))
    (with-current-buffer buf
      (gpb-git--refresh-staged-changes repo-dir cmd))
    (pop-to-buffer buf)))


(defun gpb-git:shell-command ()
  (interactive)
  (call-interactively 'shell-command)
  (gpb-git:show-status--refresh))


(provide 'gm-status)

