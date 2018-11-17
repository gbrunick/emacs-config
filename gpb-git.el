;;
;;  Git-related functions
;;

(defvar-local file-section-header-alist nil
  "Mapping from filenames to diff section header strings")

(defvar-local file-name-overlay-alist nil
  "Mapping from filenames to overlays on those filenames")

(defvar-local file-hunk-overlays-alist nil
  "Mapping from filenames to lists of overlays cover hunk diffs")


(defun gpb:stage-some-changes ()
  (interactive)
  (let ((repo-root (gpb-git:find-repo-root))
        (temp-dir (temporary-file-directory))
        (unstaged-buf (gpb-git:get-diff-buffer "*unstaged changes*"))
        (to-stage-buf (gpb-git:get-diff-buffer "*changes to stage*")))
    (when (null repo-root) (user-error "Not in a Git repo"))
    (let ((default-directory repo-root)
          (inhibit-read-only t))
      (process-file "git" nil unstaged-buf nil "diff")
      (with-current-buffer to-stage-buf
        (insert (with-current-buffer unstaged-buf
                  (buffer-substring (point-min) (point-max))))))
    (gpb-git:init-diff-buffer unstaged-buf)
    (gpb-git:init-diff-buffer to-stage-buf t)
    (delete-other-windows)
    (set-window-buffer (selected-window) unstaged-buf)
    (set-window-point (selected-window) (with-current-buffer unstaged-buf
                                          (point-min)))
    (with-current-buffer to-stage-buf
      (setq-local other-buffer unstaged-buf)
      (setq-local other-window (selected-window)))

    (let ((win2 (split-window-horizontally)))
      (set-window-buffer win2 to-stage-buf)
      (set-window-point win2 (with-current-buffer to-stage-buf
                               (point-min)))
      (with-current-buffer unstaged-buf
        (setq-local other-buffer to-stage-buf)
        (setq-local other-window win2)))))


(defun gpb-git:get-diff-buffer (name)
  (let ((buf (get-buffer-create name))
        (inhibit-read-only t))
    (with-current-buffer name
      (erase-buffer))
    buf))


(defun gpb-git:init-diff-buffer (buf &optional hide)
  (with-current-buffer buf
    (kill-all-local-variables)
    (setq-local diff-font-lock-refine t)
    (setq-local show-help-function 'tooltip-show-help-non-mode)
    (setq-local gpb-git:currently-highlighted-hunk nil)
    (setq-local header-line-format '(:eval (gpb-git:compute-header)))
    (setq-local current-file nil)
    (face-remap-add-relative 'header-line 'fringe)
    (add-hook 'post-command-hook 'gpb-git:post-command-hook nil t)
    (set (make-local-variable 'font-lock-defaults) diff-font-lock-defaults)
    (font-lock-mode -1)
    (font-lock-fontify-buffer)
    (gpb-git:add-file-name-overlays buf hide)
    (gpb-git:add-hunk-overlays buf hide)
    (use-local-map gpb-git:staging-hunks-keymap)
    (setq buffer-read-only t)))


(defun gpb-git:add-file-name-overlays (&optional buf hide)
  (with-current-buffer (or buf (current-buffer))
    (setq-local file-section-header-alist nil)
    (setq-local file-name-overlay-alist nil)
    (save-excursion
      (goto-char (point-max))
      (while (re-search-backward "^diff --git a/\\(.*\\) b/" nil t)
        (let* ((filename (substring-no-properties (match-string 1)))
               ;; The end of the file header
               (header-end (save-excursion (re-search-forward "^@@")
                                           (match-beginning 0)))
               (header-text (buffer-substring-no-properties (point) header-end))
               ;; The end of the file section
               (file-end (copy-marker (or (next-single-property-change
                                           (point) 'filename)
                                          (point-max))))
               (inhibit-read-only t)
               ov)
          (setq file-section-header-alist (cons `(,filename . ,header-text)
                                           file-section-header-alist))
          ;; Replace the header with the filename and put an overlay on the
          ;; filename.
          (delete-region (point) header-end)
          (insert "\n")
          (setq ov (make-overlay (point) (save-excursion (insert filename)
                                                         (insert "\n")
                                                         (point))))
          (overlay-put ov 'evaporate t)
          (overlay-put ov 'invisible hide)
          (overlay-put ov 'face '((:height 160) diff-file-header diff-header))
          (setq file-name-overlay-alist (cons `(,filename . ,ov)
                                         file-name-overlay-alist))
          (add-text-properties (point) file-end `(filename ,filename)))))
    file-name-overlay-alist))




(defun gpb-git:add-hunk-overlays (&optional buf hide)
  (with-current-buffer (or buf (current-buffer))
    (setq-local file-hunk-overlays-alist nil)
    (save-excursion
      (goto-char (point-max))
      (while (re-search-backward "^@@.*@@" nil t)
        (let* ((filename (get-text-property (point) 'filename))
               (ov (make-overlay
                    (point) (or (save-excursion
                                  (forward-line 1)
                                  (and (re-search-forward
                                        "\\(^@@.*@@\\|^diff --git\\)" nil t)
                                       (match-beginning 0)))
                                (point-max))))
               (current-hunks (aget file-hunk-overlays-alist filename nil)))
          (aput 'file-hunk-overlays-alist filename (cons ov current-hunks))
          (overlay-put ov 'invisible hide)
          (overlay-put ov 'hunk t)
          (overlay-put ov 'filename filename)
          (overlay-put ov 'evaporate t))))
    file-hunk-overlays-alist))


(defun gpb-git:hide-hunk (&optional pos)
  (let* ((pos (or pos (point)))
         (ov (or (cdr (get-char-property-and-overlay pos 'hunk))
                 (error "No hunk at %s" pos)))
         (filename (overlay-get ov 'filename))
         (file-header-overlay (aget file-name-overlay-alist filename))
         (other-overlays (aget file-hunk-overlays-alist filename)))
    (overlay-put ov 'invisible t)
    (when (cl-every (lambda (o) (overlay-get o 'invisible)) other-overlays)
      (overlay-put file-header-overlay 'invisible t))))


(defun gpb-git:show-hunk (&optional pos)
  (let* ((pos (or pos (point)))
         (ov (cdr (get-char-property-and-overlay pos 'hunk)))
         (filename (overlay-get ov 'filename))
         (file-header-overlay (aget file-name-overlay-alist filename)))
    (overlay-put ov 'invisible nil)
    (overlay-put file-header-overlay 'invisible nil)))


(defun gpb-git:hide-file-section ()
  (interactive)
  (let ((filename (get-text-property (point) 'filename))
        (beg (get-text-property (point) 'filename-beg))
        (end (get-text-property (point) 'filename-end))
        text)
    (if (get-text-property (point) 'display)
        (remove-text-properties beg end '(display))
      (setq text (concat
                  (propertize filename 'face '(diff-file-header diff-header))
                  (propertize "...\n" 'face 'diff-header)
                  "\n"))
      (add-text-properties beg end `(display ,text)))))


(defun gpb-git:find-repo-root ()
  (let ((dir default-directory))
    (while (and dir (not (file-exists-p (concat dir ".git"))))
      (setq dir (file-name-directory
                 (directory-file-name dir))))
    dir))


(defun gpb-git:forward-hunk (&optional arg)
  (interactive "P")
  (when (eobp) (user-error "End of buffer"))
  (let ((pt (point))
        (end (window-end))
        (regex "^@@.*@@"))
    (if arg
        (goto-char (or (next-single-property-change (point) 'filename)
                       (user-error "Last file")))

      (condition-case exc
          (progn (forward-line 1)
                 (re-search-forward regex)
                 ;; Skip through hidden hunks.
                 (while (overlay-get (gpb-git:get-current-hunk) 'invisible)
                   (re-search-forward regex))
                 (goto-char (match-beginning 0)))
        (search-failed
         (goto-char pt)
         (user-error "Last hunk"))))

    ;; If we scrolled all the way out of the initial window, put the hunk
    ;; the first line.
    (when (and (> (point) end) (not (eobp)))
      (recenter 0))))


(defun gpb-git:get-current-hunk (&optional pos)
  (let ((pos (or pos (point))))
    (cdr (get-char-property-and-overlay (point) 'hunk))))


(defun gpb-git:backward-hunk (arg)
  "Move back to the start of the previous hunk.
With a prefix argument, move to the first hunk of the current
file."
  (interactive "P")
  (when (bobp) (user-error "Beginning of buffer"))
  (if arg
      (goto-char (or (previous-single-property-change (point) 'filename)
                     (user-error "First file")))
    (let ((regex "^@@.*@@"))
      (re-search-backward regex)
      (while (overlay-get (cdr (get-char-property-and-overlay (point) 'hunk))
                          'invisible)
        (re-search-backward regex)))))


(defvar gpb-git:staging-hunks-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'gpb-git:forward-hunk)
    (define-key map [(backtab)] 'gpb-git:backward-hunk)
    (define-key map "\r" 'gpb-git:toggle-hunk)
    map))


(defun gpb-git:toggle-hunk ()
  (interactive)
  (let ((pt (point)))
    (gpb-git:hide-hunk pt)
    (with-current-buffer other-buffer (gpb-git:show-hunk pt))
    (set-window-point other-window pt)
    (gpb-git:forward-hunk t)))


(defface gpb-git:highlighted-hunk '((t :background "gray95"))
  "Highlighting for the current hunk")


(defvar gpb-git:currently-highlighted-hunk-beg nil)
(defvar gpb-git:currently-highlighted-hunk-end nil)

(defun gpb-git:post-command-hook ()
  (let ((prev-hunk gpb-git:currently-highlighted-hunk)
        (new-hunk (cdr (get-char-property-and-overlay (point) 'hunk))))
    (when (not (eq new-hunk prev-hunk))
      (when prev-hunk (gpb-git:highlight-hunk prev-hunk t))
      (when new-hunk (gpb-git:highlight-hunk new-hunk))
      (setq-local gpb-git:currently-highlighted-hunk new-hunk))))


(defun gpb-git:center-string (txt)
  (let ((indent (max (- (/ (- (window-width) (length txt)) 2) 1) 0)))
    (concat (make-string indent ?\ ) txt)))


(defun gpb-git:compute-header ()
  (let* ((ov (gpb-git:get-current-hunk))
         (window-start (window-start))
         (window-width (window-width))
         (face '((:height 160) diff-file-header diff-header))
         (filename (get-text-property (point) 'filename))
         (file-header-ov (aget file-name-overlay-alist filename)))
    (message "window-start: %S" window-start)
    (message "window-width %S" window-width)
    (message "filename: %S" filename)
    (cond
     (filename
      (gpb-git:center-string
       (format "%s: %s hunks" filename
               (length (aget file-hunk-overlays-alist filename nil)))))
     (t (gpb-git:center-string
         (format "%s files" (length file-hunk-overlays-alist)))))))


(defun gpb-git:compute-header-bak2 ()
  (let* ((window-start (window-start))
         (fringes (window-fringes))
         (face '((:height 160) diff-file-header diff-header))
         (filename (get-text-property (max 1 (1- window-start)) 'filename)))
    ;; (message "window-start: %S" window-start)
    ;; (message "filename: %S" filename)
    (when filename
      (concat (propertize " " 'display '((space :align-to 0)) 'face 'fringe)
              (propertize filename 'face face)
              (propertize " " 'display
                          '((space :align-to right)) 'face face)))))


(defun gpb-git:compute-header-bak ()
  (let* ((ov (gpb-git:get-current-hunk))
         (window-start (window-start))
         (face '((:height 160) diff-file-header diff-header))
         (filename (get-text-property (point) 'filename))
         (file-header-ov (aget file-name-overlay-alist filename)))
    (message "window-start: %S" window-start)
    (message "filename: %S" filename)
    (or
     (when ov
       ;; We only add the header if you can't see the filename in the
       ;; buffer.
       (message "(overlay-start file-header-ov): %S"
                (overlay-start file-header-ov))
       (message "(overlay-end file-header-ov): %S"
                (overlay-end file-header-ov))
       (when (> window-start (overlay-start file-header-ov))
         (concat " " (propertize filename 'face face))))

     ;;(propertize " " 'face '((:height 160)))
     )))


(defun gpb-git:highlight-hunk (&optional ov remove)
  (interactive)
  (message "gpb-git:highlight-hunk: start")
  (message "gpb-git:highlight-hunk: %S" (current-buffer))
  (let* ((ov (or ov (cdr (get-char-property-and-overlay (point) 'hunk))))
         (ov-beg (overlay-start ov))
         (ov-end (overlay-end ov))
         (inhibit-read-only t))
    (save-excursion
      (goto-char ov-beg)
      (while (< (point) ov-end)
        (let* ((face (get-text-property (point) 'face))
               (face-list (if (listp face) face `(,face)))
               (next-change (min ov-end (next-single-property-change
                                         (point) 'face)))
               (new-face (if remove (cl-remove 'gpb-git:highlighted-hunk
                                               face-list)
                           (append face-list '(gpb-git:highlighted-hunk)))))
          (put-text-property (point) next-change 'face new-face)
          (goto-char next-change))))
    (setq-local gpb-git:currently-highlighted-hunk ov))
  (message "gpb-git:highlight-hunk: done"))
