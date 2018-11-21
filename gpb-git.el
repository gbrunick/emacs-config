;;
;;  Tools for staging and unstaging hunks to the Git index
;;
;;  This package provides the command `gpb-git:stage-hunks' which opens two
;;  side-by-side buffers and for selecting unstaged changes that should be
;;  applied to the Git index and staged changes that should be removed.
;;
;;  This package does not attempt to provide a full Git porcelain.  In
;;  particular, you will still need to use the command line (or `vc' or
;;  `magit') to commit the changes you have staged.
;;
;;  Implementation overview:
;;
;;    We call `git diff HEAD` to find the changes in the working directory
;;    and then call `git diff --cached` to see which of these changes have
;;    already been staged.
;;    If the set of hunks returned by `git diff --cached` is not a subset
;;    of the hunks returned by `git diff HEAD` we give up; the current GUI
;;    doesn't
;;
;;    We insert the output from `git diff ...` into two buffers and then
;;    cover each hunk with an overlay so we can easily hide hunks.  The
;;    first buffer is used to show the changes that could be applied; the
;;    second buffer is used to show the changes that will be applied.
;;    Every hunk is inserted into both buffers, but is only visible in one
;;    buffer at a time giving the user the impression that they are moving
;;    hunks back and forth between the buffers.
;;
;;    Once we are done choosing hunks, we write all the visible hunks in
;;    the second buffer to a patch buffer, write the contents of that
;;    buffer to a temporary file, and then apply that patch to the index
;;    via `git apply --cached TEMPFILE`.
;;
(defvar gpb-git:delete-temp-files nil
  "If non-nil, clean up temporary files.  You may want to disable
  this for debugging purposes")

(defvar gpb-git:patch-buffer-name "*git patch*"
  "The name of the temporary buffer used to formulate patches.")

(defvar gpb-git:process-output-buffer-name "*git apply*"
  "The name of the buffer used to display Git output.
If the patch cannot be applied, this is the buffer that will be
used to show the errors to the user.")


;;
;;  Local variables used in the various editing buffers.
;;

(defvar-local file-section-header-alist nil
  "Mapping from filenames to diff section header strings.
These strings are removed from the hunk selection buffers and
stored here so we can add them back when we produce the patch
file for `git apply`.")

(defvar-local file-name-overlay-alist nil
  "Mapping from filenames to overlays on the text showing the filename.
We use these overlays to hide a filename when all of its hunks are hidden.")

(defvar-local file-hunk-overlays-alist nil
  "Mapping from filenames to lists of overlays that cover hunk diffs")

(defvar-local gpb-git:other-buffer nil
  "The linked buffer that is used for selecting hunks")


;;
;;  Faces
;;

(defface gpb-git:comment
  '((t :background "#eeeeee"))
  "Face used for coments and instructions")

(defface gpb-git:file-name
  '((t :background "#ffffff" :height 140))
  "Face used for the file name header")

(defface gpb-git:file-header '((t :background "grey95"))
  "Face used for the hunk header in the pathc buffer")

(defface gpb-git:hunk-header
  '((t :background "gray65"))
  "Face used for the hunk header")

(defface gpb-git:context-line
  '((t :background "gray99"))
  "Face used for context lines in a hunk")

(defface gpb-git:deleted-line
  ;; '((t :foreground "#aa2222" :background "#ffdddd"))
  '((t :foreground "#880000" :background "#ffdddd"))
  "Face used for the deleted lines in a hunk")

(defface gpb-git:added-line
  ;; '((t :foreground "#22aa22" :background "#ddffdd"))
  '((t :foreground "#005500" :background "#ddffdd"))
  "Face used for the added lines in a hunk")

(defface gpb-git:highlighted-hunk-header
  '((t :background "#9999ff"))
  "Face used for context lines in the highlighted hunk")

(defface gpb-git:highlighted-context-line
  '((t :background "#e0e0ff"))
  "Face used for context lines in the highlighted hunk")

(defface gpb-git:highlighted-added-line
  '((t :foreground "#005500" :background "#b8e0b8"))
  "Face used for the added lines in the highlighted hunk.")

(defface gpb-git:highlighted-deleted-line
  '((t :foreground "#880000" :background "#f0c0c0"))
  "Face used for deleted lines in the highlighted hunk.")

;;
;;  Keymaps
;;

(defvar gpb-git:hunk-choices-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'gpb-git:forward-hunk)
    (define-key map [(backtab)] 'gpb-git:backward-hunk)
    (define-key map "\r" 'gpb-git:toggle-hunk)
    (fset 'gpb-git:hunk-choices-keymap map)
    map)
  "The keymap used for choosing hunks.")

(defvar gpb-git:selected-hunks-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map 'gpb-git:hunk-choices-keymap)
    (define-key map "\C-c\C-c" 'gpb-git:apply-changes-command)
    (fset 'gpb-git:selected-hunks-keymap map)
    map)
  "The keymap used for removing and staging hunks.")

(defvar gpb-git:patch-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'gpb-git:apply-patch)
    map)
  "The keymap for the buffer containing a patch.")


;;
;;  Functions
;;

(defun gpb-git:update-index (&optional repo-root no-show)
  "Create the buffers used for hunk selection."
  (interactive)
  (let* ((diff-info (gpb-get:get-hunks repo-root))
         (choices-keymap gpb-git:hunk-choices-keymap)
         (selected-keymap gpb-git:selected-hunks-keymap)
         (default-directory (or repo-root default-directory))
         (unstaged-buf (gpb-git:create-hunk-buffer diff-info nil))
         (staged-buf (gpb-git:create-hunk-buffer diff-info t)))

    ;; Link the buffers
    (with-current-buffer unstaged-buf
      (setq-local gpb-git:other-buffer staged-buf)
      (setq-local gpb-git:staged-buffer staged-buf)
      (setq-local gpb-git:unstaged-buffer unstaged-buf))

    (with-current-buffer staged-buf
      (setq-local gpb-git:other-buffer unstaged-buf)
      (setq-local gpb-git:staged-buffer staged-buf)
      (setq-local gpb-git:unstaged-buffer unstaged-buf))

    ;; Show the buffers in two side-by-side windows in the current frame.
    (delete-other-windows)
    (set-window-buffer (selected-window) unstaged-buf)
    (let ((win2 (split-window-horizontally)))
      (set-window-buffer win2 staged-buf))))


;; (defun gpb-git:stage-hunks (arg)
;;   "Stage hunks to the Git index.
;; With a prefix argument, remove staged changes from the index."
;;   (interactive "P")
;;   (if arg
;;       ;; In this case, the candidate hunks change the index to agree HEAD,
;;       ;; so applying these hunks to index amounts to unstaging changes.
;;       (gpb-git:stage-hunks-1 "*staged changes*" "*changes to unstage*"
;;                              (concat "#\n#  Select the changes to remove from "
;;                                      "the index from the list below.\n#\n")
;;                              "diff" "--cached" "-R")
;;     (gpb-git:stage-hunks-1 "*unstaged changes*" "*changes to stage*"
;;                            (concat "#\n#  Select changes below to add "
;;                                    "them from the index.\n#\n")
;;                            "diff")))


;; (defun gpb-git:stage-hunks-1 (choices selected instructions &rest args)
;;   "Choose hunks to apply to the Git index.
;; CHOICES is the name of the buffer that will show the hunks that
;; can be selected.  SELECTED is the name of the buffer that will
;; show the hunks that have been selected.  ARGS is the list of
;; arguments that will be passed to Git to produce the list of
;; hunks.  Typically the first element of ARGS is \"diff\"."
;;   (let ((repo-root (gpb-git:find-repo-root))
;;         (choices-buf (gpb-git:get-diff-buffer choices))
;;         (selected-buf (gpb-git:get-diff-buffer selected))
;;         (choices-keymap gpb-git:hunk-choices-keymap)
;;         (selected-keymap gpb-git:selected-hunks-keymap))
;;     (when (null repo-root) (user-error "Not in a Git repo"))
;;     (let ((default-directory repo-root)
;;           (inhibit-read-only t))
;;       (apply 'process-file "git" nil choices-buf nil args)
;;       (gpb-git:copy-buffer choices-buf selected-buf))

;;     (gpb-git:init-hunk-buffer choices-buf choices-keymap selected-buf)
;;     (gpb-git:init-hunk-buffer selected-buf selected-keymap choices-buf t)

;;     (dolist (buf `(,choices-buf ,selected-buf))
;;       (with-current-buffer buf
;;         (setq default-directory repo-root)))

;;     (with-current-buffer choices-buf
;;     (save-excursion
;;       (let ((msg (propertize instructions 'face 'gpb-git:comment))
;;             (inhibit-read-only t))
;;         (insert "\n")
;;         (set-text-properties 1 2 `(display ,msg)))))

;;     (with-current-buffer selected-buf
;;       (save-excursion
;;         (let ((msg (propertize
;;                     (concat "#\n#  Use C-c C-c to apply these changes "
;;                             "the Git index.\n#  Add a prefix argument "
;;                             "to edit the patch before applying it.\n#\n")
;;                     'face 'gpb-git:comment))
;;               (inhibit-read-only t))
;;           (insert "\n")
;;           (set-text-properties 1 2 `(display ,msg)))))


;;     ;; Show the buffers in two side-by-side windows in the current frame.
;;     (delete-other-windows)
;;     (set-window-buffer (selected-window) choices-buf)
;;     (set-window-point (selected-window) (with-current-buffer choices-buf
;;                                           (point-min)))

;;     (let ((win2 (split-window-horizontally)))
;;       (set-window-buffer win2 selected-buf)
;;       (set-window-point win2 (with-current-buffer selected-buf
;;                                (point-min)))
;;       (with-current-buffer choices-buf
;;         (setq-local other-window win2)))))


;; (defun gpb-git:get-diff-buffer (name)
;;   (let ((buf (get-buffer-create name))
;;         (inhibit-read-only t))
;;     (with-current-buffer name
;;       (erase-buffer))
;;     buf))


;; (defun gpb-git:copy-buffer (src dst)
;;   (with-current-buffer dst
;;     (insert (with-current-buffer src
;;               (buffer-substring (point-min) (point-max))))))


;; (defun gpb-git:init-hunk-buffer (buf keymap other-buf &optional hide-hunks)
;;   "Initialize a hunk selection buffer."
;;   (with-current-buffer buf
;;     (kill-all-local-variables)
;;     (setq-local show-help-function 'tooltip-show-help-non-mode)
;;     (setq-local gpb-git:currently-highlighted-hunk nil)
;;     (setq-local header-line-format '(:eval (gpb-git:compute-header)))
;;     (setq-local current-file nil)
;;     (setq-local gpb-git:other-buffer other-buf)

;;     (add-hook 'kill-buffer-hook 'gpb-git:kill-buffer-hook nil t)
;;     (add-hook 'post-command-hook 'gpb-git:post-command-hook nil t)

;;     (let ((inhibit-read-only t))
;;       (gpb-git:put-text-faces)
;;       (gpb-git:add-file-name-overlays buf)
;;       (gpb-git:add-hunk-overlays buf))

;;     (use-local-map keymap)

;;     (when hide-hunks
;;       (dolist (filename-overlay (mapcar 'cdr file-name-overlay-alist))
;;         (overlay-put filename-overlay 'invisible t))
;;       (dolist (hunk (gpb-git:get-visible-hunks))
;;         (overlay-put hunk 'invisible t)))

;;     (goto-char (point-min))
;;     (setq buffer-read-only t)))


(defun gpb-git:put-text-faces (&optional beg end highlighted)
  (interactive "r\nP")
  (let ((beg (or beg (point-min)))
        (end (copy-marker (or end (point-max))))
        (hunk-header-face (if highlighted 'gpb-git:highlighted-hunk-header
                            'gpb-git:hunk-header))
        (context-face (if highlighted 'gpb-git:highlighted-context-line
                        'gpb-git:context-line))
        (added-face (if highlighted 'gpb-git:highlighted-added-line
                      'gpb-git:added-line))
        (deleted-face (if highlighted 'gpb-git:highlighted-deleted-line
                        'gpb-git:deleted-line))
        (inhibit-read-only t))
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (cond
         ((looking-at-p "^@@.*@@")
          ;; Remove anything following the second @@.  It is supposed to be
          ;; a function name, but Git doesn't seem to get this right with
          ;; the languages I commonly use.
          (delete-region (save-excursion (re-search-forward "^@@.*@@") (point))
                         (save-excursion (end-of-line) (point)))
          (put-text-property (point) (progn (forward-line 1) (point))
                             'face hunk-header-face))
         ((looking-at-p "^+")
          (put-text-property (point) (progn (forward-line 1) (point))
                             'face added-face))
         ((looking-at-p "^-")
          (put-text-property (point) (progn (forward-line 1) (point))
                             'face deleted-face))
         ((looking-at-p "^diff --git ")
          ;; This only happens in the patch buffer.  In the working
          ;; buffers, we replace the differ header with the file name in
          ;; a larger font.
          (put-text-property (point) (progn (re-search-forward "^@@")
                                            (goto-char (match-beginning 0))
                                            (point))
                             'face 'gpb-git:file-header))
         (t
          (put-text-property (point) (progn (forward-line 1) (point))
                             'face context-face)))))))


;; (defun gpb-git:add-file-name-overlays (&optional buf)
;;   (with-current-buffer (or buf (current-buffer))
;;     (setq-local file-section-header-alist nil)
;;     (setq-local file-name-overlay-alist nil)
;;     (save-excursion
;;       (goto-char (point-max))
;;       (while (re-search-backward "^diff --git [ab]/\\(.*\\) [ab]/" nil t)
;;         (let* ((filename (substring-no-properties (match-string 1)))
;;                ;; The end of the file header
;;                (header-end (save-excursion (re-search-forward "^@@")
;;                                            (match-beginning 0)))
;;                (header-text (buffer-substring-no-properties (point) header-end))
;;                ;; The end of the file section
;;                (file-end (copy-marker (or (next-single-property-change
;;                                            (point) 'filename)
;;                                           (point-max))))
;;                (filename-text (concat "\n"
;;                                       (propertize (concat filename "\n")
;;                                                   'face 'gpb-git:file-name)))
;;                (inhibit-read-only t)
;;                ov)
;;           (setq file-section-header-alist (cons `(,filename . ,header-text)
;;                                            file-section-header-alist))
;;           ;; Replace the header with the filename and put an overlay on the
;;           ;; filename.
;;           (delete-region (point) header-end)
;;           (setq ov (make-overlay (point) (save-excursion (insert filename-text)
;;                                                         (point))))
;;           (overlay-put ov 'filename-p t)
;;           (setq file-name-overlay-alist (cons `(,filename . ,ov)
;;                                          file-name-overlay-alist))
;;           (put-text-property (point) file-end 'filename filename))))
;;     file-name-overlay-alist))


;; (defun gpb-git:add-hunk-overlays (&optional buf )
;;   "Put overlays on the hunks.

;; Assumes that filename text properties have already been added to
;; the buffer."
;;   (with-current-buffer (or buf (current-buffer))
;;     (setq-local file-hunk-overlays-alist nil)
;;     (save-excursion
;;       (goto-char (point-max))
;;       (while (re-search-backward "^@@.*@@" nil t)
;;         (let* ((filename (get-text-property (point) 'filename))
;;                (filename-overlay (aget file-name-overlay-alist filename))
;;                (limit (save-excursion
;;                         (or (ignore-errors
;;                               (gpb-git:forward-file-section) (point))
;;                             (point-max))))
;;                (ov (make-overlay
;;                     (point) (or (save-excursion
;;                                   (forward-line 1)
;;                                   (and (re-search-forward
;;                                         "\\(^@@.*@@\\|^diff --git\\)" limit t)
;;                                        (match-beginning 0)))
;;                                 limit)))
;;                (current-hunks (aget file-hunk-overlays-alist filename nil)))
;;           (aput 'file-hunk-overlays-alist filename (cons ov current-hunks))
;;           (overlay-put ov 'hunk t)
;;           (overlay-put ov 'filename-overlay filename-overlay))))

;;     ;; Add the hunk number to the overlay so we can easily match them up.
;;     (dolist (hunk-lists (mapcar 'cdr file-hunk-overlays-alist))
;;       (dotimes (i (length hunk-lists))
;;         (overlay-put (nth i hunk-lists) 'hunk-number i)))

;;     file-hunk-overlays-alist))


(defun gpb-git:hide-hunk (ov &optional move-pt)
  (with-current-buffer (overlay-buffer ov)
    (overlay-put ov 'invisible t)
    (gpb-git:update-visible-files)))
    ;; (let* ((filename (or (get-char-property (overlay-start ov) 'filename)
    ;;                      (error "Assertion error")))
    ;;        (win (get-buffer-window))
    ;;        (pos (save-excursion (or (ignore-errors
    ;;                                   (gpb-git:forward-hunk) (point))
    ;;                                 (point-max)))))
    ;;   (unless (gpb-git:get-visible-hunks filename)
    ;;     (overlay-put (overlay-get ov 'filename-overlay) 'invisible t))
    ;;   (when move-pt
    ;;     ;; Update point in the buffer and the window.
    ;;     (goto-char pos)
    ;;     (set-window-point win pos)))))


(defun gpb-git:show-hunk (ov &optional move-pt)
  (with-current-buffer (overlay-buffer ov)
    (overlay-put ov 'invisible nil)
    (gpb-git:update-visible-files)))
    ;; (overlay-put (overlay-get ov 'filename-overlay) 'invisible nil)
    ;; (when move-pt
    ;;   (let* ((win (get-buffer-window))
    ;;          (pos (overlay-start other-hunk)))
    ;;     ;; Update point in the buffer and the window.
    ;;     (goto-char pos)
    ;;     (when win (set-window-point win pos))))))


;; (defun gpb-git:show-all-filenames ()
;;   (dolist (ov (mapcar 'cdr file-name-overlay-alist))
;;     (overlay-put ov 'invisible nil)))


;; (defun gpb-git:hide-file-section ()
;;   (interactive)
;;   (let ((filename (get-text-property (point) 'filename))
;;         (beg (get-text-property (point) 'filename-beg))
;;         (end (get-text-property (point) 'filename-end))
;;         text)
;;     (if (get-text-property (point) 'display)
;;         (remove-text-properties beg end '(display))
;;       (setq text (concat
;;                   (propertize filename 'face '(diff-file-header diff-header))
;;                   (propertize "...\n" 'face 'diff-header)
;;                   "\n"))
;;       (add-text-properties beg end `(display ,text)))))


(defun gpb-git:find-repo-root ()
  "Find the root of the Git repository.
Looks for the .git directory rather than calling Git."
  (let ((dir default-directory))
    (while (and dir (not (file-exists-p (concat dir ".git"))))
      (setq dir (file-name-directory
                 (directory-file-name dir))))
    dir))


(defun gpb-git:get-current-hunk (&optional pos)
  "Return the hunk overlay at POS."
  (let ((pos (or pos (point))))
    (cdr (get-char-property-and-overlay pos 'hunk))))


(defun gpb-git:get-visible-files (&optional filename)
  "Find the list of visible file overlays."
  (cl-remove-if (lambda (ov) (overlay-get ov 'invisible))
                (mapcar 'cdr file-name-overlay-alist)))


(defun gpb-git:get-visible-hunks (&optional filename)
  "Find the list of visible hunk overlays.
If FILENAME if given, only return hunks that change FILENAME."
  (let ((hunks (if filename
                   (aget file-hunk-overlays-alist filename nil)
                 (apply 'append (mapcar 'cdr file-hunk-overlays-alist)))))
    (cl-remove-if (lambda (ov) (overlay-get ov 'invisible)) hunks)))


(defun gpb-git:update-visible-files (&optional filename)
  "Align the list of visible filenames with the visible hunks."
  (dolist (filename-hunks file-hunk-overlays-alist)
    (let* ((filename (car filename-hunks))
           (filename-ov (aget file-name-overlay-alist filename))
           (filename-invisible (cl-every (lambda (hunk)
                                           (overlay-get hunk 'invisible))
                                           (cdr filename-hunks))))
      (overlay-put filename-ov 'invisible filename-invisible))))


(defun gpb-git:forward-file-section ()
  "Move forward to the start of the next file section."
  (interactive)
  (goto-char (or (next-single-property-change (point) 'filename)
                 (user-error "Last file"))))


(defun gpb-git:backward-file-section ()
  "Move backwards to the end of the previous file section."
  (interactive)
  (goto-char (or (previous-single-property-change (point) 'filename)
                 (user-error "First file"))))


(defun gpb-git:forward-hunk (&optional arg)
  (interactive "P")
  (when (eobp) (user-error "End of buffer"))
  (let ((pt (point))
        (end (window-end))
        (regex "^@@.*@@"))
    (when arg (gpb-git:forward-file-section))
    (condition-case exc
        (progn (forward-line 1)
               (re-search-forward regex)
               ;; Skip through hidden hunks.
               (while (overlay-get (gpb-git:get-current-hunk) 'invisible)
                 (re-search-forward regex))
               (goto-char (match-beginning 0)))
      (search-failed
       (goto-char pt)
       (user-error "Last hunk")))

    ;; If we scrolled all the way out of the initial window, display the
    ;; hunk on the first or second line so we can see as much as possible.
    (when (and (> (point) end) (not (eobp)))
      (if (save-excursion
            (forward-line -1)
            (get-char-property (point) 'filename-p))
          ;; The previous line is a filename; keep it visible.
          (recenter 1)
        (recenter 0)))))


(defun gpb-git:backward-hunk (&optional arg)
  "Move back to the start of the previous hunk.
With a prefix argument, move to the first hunk of the current
file."
  (interactive "P")
  (when (bobp) (user-error "Beginning of buffer"))
  (when arg (gpb-git:backward-file-section))
  (let ((pt (point))
        (regex "^@@.*@@")
        (start (window-start)))

    (condition-case exc
        (progn
          (re-search-backward regex)
          (while (overlay-get (cdr (get-char-property-and-overlay
                                    (point) 'hunk))
                              'invisible)
            (re-search-backward regex)))
      (search-failed
       (goto-char pt)
       (user-error "First hunk")))

    ;; If we had to scroll the window to make the hunk visible and the hunk
    ;; is the first hunk for a file, scroll to include the filename in the
    ;; current window.
    (when (and (< (point) start)
               (save-excursion
                 (forward-line -1)
                 (get-char-property (point) 'filename-p)))
      (recenter 1))))


(defun gpb-git:toggle-hunk ()
  "Toggle the selection of the hunk at the point."
  (interactive)
  (let* ((hunk (or (gpb-git:get-current-hunk)
                   (user-error "No hunk at point")))
         (hunk-num (or (overlay-get hunk 'hunk-number)
                       (error "Assertion error")))
         (filename (or (get-text-property (point) 'filename)
                       (error "Assertion error")))
         (other-hunk (with-current-buffer gpb-git:other-buffer
                       (nth hunk-num (aget file-hunk-overlays-alist filename)))))
    (gpb-git:hide-hunk hunk t)
    (gpb-git:show-hunk other-hunk t)
    (condition-case exc
        (gpb-git:forward-hunk)
      (error (gpb-git:backward-hunk)))))


(defun gpb-git:update-filename-visibility ()
  (dolist (filename-overlay file-name-overlay-alist)
    (let ((filename (car filename-overlay))
          (ov (cdr filename-overlay)))
      (overlay-put ov 'invisible (null (gpb-git:get-visible-hunks filename))))))


(defun gpb-git:post-command-hook ()
  "Updates hunk highlighting."
  (message "gpb-git:post-command-hook: %S" (current-buffer))
  (gpb-git:update-hightlights)
  (gpb-git:update-hightlights gpb-git:other-buffer))


(defun gpb-git:update-hightlights (&optional buf)
  "Updates hunk highlighting."
  (with-current-buffer (or buf (current-buffer))
    (let* ((prev-hunk gpb-git:currently-highlighted-hunk)
           (new-hunk (and (eq (window-buffer (selected-window))
                              (current-buffer))
                          (cdr (get-char-property-and-overlay (point) 'hunk)))))
      (when (not (eq new-hunk prev-hunk))
        (when prev-hunk (gpb-git:put-text-faces (overlay-start prev-hunk)
                                                (overlay-end prev-hunk)
                                                nil))
        (when new-hunk (gpb-git:put-text-faces (overlay-start new-hunk)
                                               (overlay-end new-hunk)
                                               t))
        (setq-local gpb-git:currently-highlighted-hunk new-hunk)))))


(defun gpb-git:center-string (txt)
  (let ((indent (max (- (/ (- (window-width) (length txt)) 2) 1) 0)))
    (concat (make-string indent ?\ )
            (propertize txt 'face '(:weight bold)))))


(defun gpb-git:compute-header ()
  "Construct a description string for the buffer header line."
  (let* ((ov (gpb-git:get-current-hunk))
         (window-start (window-start))
         (window-width (window-width))
         (face '((:height 160) diff-file-header diff-header))
         (filename (get-text-property (point) 'filename)))
    (cond
     (ov
      (let* ((visible-files (gpb-git:get-visible-files))
             (visible-files-before-pt (cl-remove-if
                                       (lambda (ov)
                                         (> (overlay-start ov) (point)))
                                       visible-files))
             (file-header-ov (aget file-name-overlay-alist filename))
             (visible-file-hunks (gpb-git:get-visible-hunks filename))
             (visible-file-hunks-before-pt (cl-remove-if
                                            (lambda (ov)
                                              (> (overlay-start ov) (point)))
                                            visible-file-hunks))
             (visible-hunks (gpb-git:get-visible-hunks))
             (visible-hunks-before-pt (cl-remove-if
                                       (lambda (ov)
                                         (> (overlay-start ov) (point)))
                                       visible-hunks)))
        (gpb-git:center-string
         (format "hunk %s/%s in %s  (file %s/%s, hunk %s/%s)"
                 (length visible-file-hunks-before-pt)
                 (length visible-file-hunks)
                 filename
                 (length visible-files-before-pt)
                 (length visible-files)
                 (length visible-hunks-before-pt)
                 (length visible-hunks)))))
     (t (gpb-git:center-string
         (format "%s files, %s hunks"
                 (length (gpb-git:get-visible-files))
                 (length (gpb-git:get-visible-hunks))))))))


(defun gpb-git:apply-changes-command (arg)
  "Apply the changes in the current buffer to the Git index.
With a prefix argument, we pop to a buffer containing the patch
and give the user an opportunity to edit the patch before
applying it."
  (interactive "P")
  (if arg
      (pop-to-buffer (gpb-git:create-patch-buffer))
    (let ((buf (gpb-git:create-patch-buffer)))
      (gpb-git:apply-patch buf))))


(defun gpb-git:create-patch-buffer (&optional buf)
  "Create a patch from the changes in BUF.
BUF should be the second (i.e., \"changes to commited\") buffer
create by `gpb-git:stage-hunks'."
  (let ((buf (or buf (current buf)))
        (patch-buf (get-buffer-create gpb-git:patch-buffer-name))
        filename text hunks hunk-diff)
    (with-current-buffer buf

      ;; Reset the patch buffer
      (let ((dir default-dir))
        (with-current-buffer patch-buf
          (setq default-directory dir)
          (erase-buffer)))

      ;; Write selected hunks to the patch buffer.
      (dolist (filename-text file-section-header-alist)
        (setq filename (car filename-text)
              text (cdr filename-text)
              hunks (gpb-git:get-visible-hunks filename))
        (when hunks
          (with-current-buffer patch-buf (insert text))
          (dolist (hunk hunks)
            (setq hunk-text (buffer-substring-no-properties
                             (overlay-start hunk) (overlay-end hunk)))
            (with-current-buffer patch-buf (insert hunk-text)))))

      ;; Apply text styling to the patch buffer and add a keymap.  This
      ;; will only be used if the user decides they want to edit the patch
      ;; before we apply it.
      (with-current-buffer patch-buf
        (gpb-git:put-text-faces)
        (goto-char (point-min))
        ;; We need a character to carry the following display text
        ;; property.
        (insert "\n")
        (let ((msg (concat "#\n#  Use C-c C-c to apply this patch to "
                           "the Git index.\n#\n")))
          (set-text-properties 1 2 `(display ,msg)))
        (use-local-map gpb-git:patch-keymap)))

    patch-buf))


(defun gpb-git:apply-patch (&optional buf repo-dir)
  "Apply the patch in BUF to the Git index.
BUF defaults to the current directory and REPO-DIR defaults to the
`default-directory' in the BUF."
  (interactive)
  (let* ((buf (or buf (current-buffer)))
         (proc-output-buf gpb-git:process-output-buffer-name)
         (default-directory (or repo-dir (with-current-buffer buf
                                           default-directory)))
         (tempfile (make-nearby-temp-file "git-" nil ".patch"))
         retvalue)
    (write-region (point-min) (point-max) tempfile)
    (setq retvalue (process-file "git" nil proc-output-buf t
                                 "apply" "--cached" tempfile))
    (if (= retvalue 0)
        ;; We succefully applied the patch.
        (progn
          (kill-buffer buf)
          (kill-buffer proc-output-buf)
          (delete-file tempfile))
      ;; If the application failed, we pop to the process output.
      (pop-to-buffer proc-output-buf))))


(defun gpb-git:kill-buffer-hook ()
  "Kill both linked buffers when one buffer is killed."
  (when (and gpb-git:other-buffer
             (buffer-live-p gpb-git:other-buffer))
    ;; Delete the link in the other buffer so we don't get into an infinite
    ;; loop.
    (with-current-buffer gpb-git:other-buffer
      (setq-local gpb-git:other-buffer nil))
    ;; Now kill the other buffer.
    (kill-buffer gpb-git:other-buffer)))


(defun gpb-get:get-hunks (&optional repo-root)
  "Get the current hunks.

Returns a structure of the form:

((filename1 (:header \"git --diff a/...\")
            (:hunks ((line-number1 ((:text . \"@@...@@\\n...\")
                                    (:status . status))
                     (line-number2 ((:text . \"@@...@@\\n...\")
                                    (:status . status)))
                     ...)))
 (filename2 (:header \"git --diff a/...\")
            (:hunks ((line-number1 ((:text . \"@@...@@\\n...\")
                                    (:status . status)))
                     (line-number2 ((:text . \"@@...@@\\n...\")
                                    (:status . status)))
                     ...)))
  ...)

where `status' is :staged or :unstaged."
  (let* ((repo-root (or repo-root (gpb-git:find-repo-root)))
         (unstaged (gpb-git:get-diff-info repo-root "diff" "HEAD"))
         (staged (gpb-git:get-diff-info repo-root "diff" "--cached")))

    ;; Check that the file headers agree.
    (dolist (file-info staged)
      (let* ((filename (car file-info))
             (header1 (cl-reduce 'aget `(,unstaged ,filename :header)))
             (header2 (cl-reduce 'aget `(,staged ,filename :header))))
        (assert (string= header1 header2))))

    ;; Check that the hunks agree.
    (dolist (filename (mapcar 'car staged))
      (let ((hunks (cl-reduce 'aget `(,staged ,filename :hunks))))
        (dolist (linenum (mapcar 'car hunks))
          (let* ((hunk1 (cl-reduce 'aget `(,unstaged ,filename :hunks ,linenum)))
                 (hunk2 (cl-reduce 'aget `(,staged ,filename :hunks ,linenum))))
            (assert (stringp hunk1))
            (assert (stringp hunk2))
            (assert (gpb-git:hunks-agree-p hunk1 hunk2))))))

    ;; Now check which hunks are staged.
    (dolist (filename (mapcar 'car unstaged))
      (let ((hunks (cl-reduce 'aget `(,unstaged ,filename :hunks))))
        (dolist (linenum (mapcar 'car hunks))
          (let ((hunk-text (aget hunks linenum))
                (staged (cl-reduce 'aget `(,staged ,filename :hunks ,linenum))))
            (setcdr (assoc linenum hunks)
                    `((:text . ,hunk-text)
                      (:state . ,(if staged :staged :unstaged))))))))

    unstaged))


(defun gpb-git:get-diff-info (repo-dir &rest args)
  "Parse the output of `gitt diff` from a buffer.

Returns a nested structure of the form:

  ((filename1 (:header \"git --diff a/...\")
              (:hunks ((line-number1 \"@@...@@\\n...\")
                       (line-number2 \"@@...@@\\n...\")
                       ...)))
   (filename2 (:header \"git --diff a/...\")
              (:hunks ((line-number1 \"@@...@@\\n...\")
                       (line-number2 \"@@...@@\\n...\")
                       ...)))
    ...)

The first level is an alist from filenames to futher alists
with keys `:text' and `:hunks'.  The values associated with the
`:hunks' are again alists with keys which are line numbers and
values which are the hunk text."
  (with-temp-buffer
    (setq default-directory repo-dir)
    (unless (= (apply 'process-file "git" nil t nil args) 0)
      (error "`git diff` failed"))

    (save-excursion
      (save-match-data
        (let ((diff-info (list :diff-info)) hunk-list)
          (goto-char (point-min))
          (while (< (point) (point-max))
            (when (> (point) 82300)
              (if t t))
            (cond
             ((looking-at "^diff --git [ab]/\\(.*\\) [ab]/")
              (let ((header-start (point))
                    (filename (substring-no-properties
                               (or (match-string 1)
                                   (error "Assertion error"))))
                    text)
                ;; Remove all the non-essential info.
                (forward-line 1)
                (delete-region (point)
                               (progn
                                 (re-search-forward "^\\(---\\|+++\\)")
                                 (match-beginning 0)))

                (setq text (buffer-substring-no-properties
                            header-start
                            (progn (re-search-forward "^@@")
                                   (goto-char (match-beginning 0))
                                   (point)))
                      hunk-list (list :hunks))

                (nconc diff-info `((,filename (:header . ,text) ,hunk-list)))))

             ((looking-at "^@@ +-\\([0-9]+\\),.*@@")
              (let ((line (string-to-number (match-string 1)))
                    (text (buffer-substring-no-properties
                           (point)
                           (progn
                             (forward-line 1)
                             (or (and
                                  (re-search-forward "^@@\\|^diff --git " nil t)
                                  (goto-char (match-beginning 0)))
                                 (goto-char (point-max)))
                             (point)))))
                (nconc hunk-list `((,line . ,text)))))
             (t (forward-line 1))))
          (cdr diff-info))))))


(defun gpb-git:create-hunk-buffer (diff-info staged)
  "Create a buffer that displays in the hunks in DIFF-INFO.

DIFF-INFO is should be a nested alist with the structure:

  ((filename1 (:header \"git --diff a/...\")
              (:hunks ((line-number1 \"@@...@@\\n...\")
                       (line-number2 \"@@...@@\\n...\")
                       ...)))
   (filename2 (:header \"git --diff a/...\")
              (:hunks ((line-number1 \"@@...@@\\n...\")
                       (line-number2 \"@@...@@\\n...\")
                       ...)))
    ...)

The first level is an alist from filenames to futher alists
with keys `:text' and `:hunks'.  The values associated with the
`:hunks' are again alists with keys which are line numbers and
values which are the hunk text."
  (let* ((dir default-directory)
         (name (if staged "*staged changes*" "*unstaged changes*"))
         (buf (get-buffer-create name))
         (filename-overlays (list :file-name-overlay-alist))
         (hunk-overlays (list :file-hunk-overlays-alist))
         ov)

    (with-current-buffer buf
      (kill-all-local-variables)
      (erase-buffer)
      (setq-local gpb-git:currently-highlighted-hunk nil)
      (setq-local header-line-format '(:eval (gpb-git:compute-header)))
      (setq-local diff-info diff-info)
      (setq-local default-directory dir)

      (add-hook 'kill-buffer-hook 'gpb-git:kill-buffer-hook nil t)
      (add-hook 'post-command-hook 'gpb-git:post-command-hook nil t)

      (use-local-map gpb-git:selected-hunks-keymap)

      (dolist (filename (mapcar 'car diff-info))
        (let ((hunks (cl-reduce 'aget `(,diff-info ,filename :hunks)))
              (file-hunk-overlays (list :stub))
              (file-start (point))
              (filename-overlay)
              (i 0))
          (insert "\n")
          (put-text-property (point) (progn (insert filename)
                                            (insert "\n")
                                            (point))
                             'face 'gpb-git:file-name)
          (setq filename-overlay (make-overlay file-start (point)))
          (overlay-put filename-overlay 'filename-p t)
          (nconc filename-overlays `((,filename . ,filename-overlay)))
          (dolist (hunk-info hunks)
            (let* ((line (car hunk-info))
                   (text (or (aget (cdr hunk-info) :text)
                             (error "Assertion error")))
                   (hunk-staged (or (aget (cdr hunk-info) :state)
                                    (error "Assertion error"))))
              (setq ov (make-overlay (point) (progn (insert text) (point))))
              (overlay-put ov 'hunk t)
              (overlay-put ov 'hunk-number i)
              (incf i)
              (overlay-put ov 'filename-overlay filename-overlay)
              (nconc file-hunk-overlays `(,ov))
              (gpb-git:put-text-faces (overlay-start ov)
                                      (overlay-end ov))
              (unless (eq (eq hunk-staged :staged) staged)
                (overlay-put ov 'invisible t))))

          (put-text-property file-start (point) 'filename filename)
          (nconc hunk-overlays `((,filename . ,(cdr file-hunk-overlays))))))

      (goto-char (point-min))
      (setq-local file-name-overlay-alist (cdr filename-overlays))
      (setq-local file-hunk-overlays-alist (cdr hunk-overlays))
      (gpb-git:update-filename-visibility)

      ;; Throw some instructions in the top of the buffer.
      (save-excursion
        (let ((msg (propertize
                    (concat "#\n#  Use C-c C-c to apply these changes "
                            "the Git index.\n#  Add a prefix argument "
                            "to edit the patch before applying it.\n#\n")
                    'face 'gpb-git:comment))
              (inhibit-read-only t))
          (insert "\n")
          (set-text-properties 1 2 `(display ,msg))))

      (setq buffer-read-only t))

    buf))


(defun gpb-git:hunks-agree-p (text1 text2)
  "Compare two hunks.

We consider two hunks to agree if they describe the same change
to be applied to the source file.  In particular, the target
files may be different, so the second range of lines may be
different in the hunk header.  We also ignore everything after
the second @@ in the hunk header as this is purely
informational."
  (let ((text1 (with-temp-buffer
                 (insert text1)
                 (goto-char (point-min))
                 (re-search-forward "^@@ -[0-9]+,")
                 (delete-region (point) (progn (end-of-line) (point)))))
        (text2 (with-temp-buffer
                 (insert text2)
                 (goto-char (point-min))
                 (re-search-forward "^@@ -[0-9]+,")
                 (delete-region (point) (progn (end-of-line) (point))))))
    (string= text1 text2)))


(global-set-key "\C-cs" 'gpb-git:update-index)
