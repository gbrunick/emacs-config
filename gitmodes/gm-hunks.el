;;
;;  Hunk and diff related functions and modes.
;;

(require 'gm-util)

(defvar gpb-git:hunk-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'gpb-git:forward-command)
    (define-key map [(backtab)] 'gpb-git:backward-command)
    (define-key map "p" 'gpb-git:backward-command)
    (define-key map "P" 'gpb-git:backward-file-command)
    (define-key map "n" 'gpb-git:forward-command)
    (define-key map "N" 'gpb-git:forward-file-command)
    (define-key map (kbd "RET") 'gpb-git:goto-line)
    (define-key map "g" 'gpb-git:refresh-buffer)
    (fset 'gpb-git:hunk-view-mode-map map)
    map)
  "Base keymap for hunk viewing.")

(defvar gpb-git:hunk-selection-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "m" 'gpb-git:mark-hunk-command)
    (define-key map "M" 'gpb-git:mark-file-command)
    (define-key map "r" 'gpb-git:mark-as-rename)
    (define-key map "u" 'gpb-git:unmark-hunk-command)
    (define-key map "U" 'gpb-git:unmark-file-command)
    (set-keymap-parent map 'gpb-git:hunk-view-mode-map)
    (fset 'gpb-git:hunk-selection-mode-map map)
    map)
  "Base keymap for hunk viewing and selection.")


(defvar gpb-git:unstaged-changes-mode-map
  (let ((map (make-sparse-keymap)))
    ;; "add" new files to the index.
    (define-key map "a" 'gpb-git:stage-hunks)
    ;; "delete" changes.
    (define-key map "d" 'gpb-git:revert-marked-hunks)
    (set-keymap-parent map 'gpb-git:hunk-selection-mode-map)
    (fset 'gpb-git:unstaged-changes-mode-map map)
    map)
  "The keymap used for unstaged hunks.")


(defvar gpb-git:staged-changes-mode-map
  (let ((map (make-sparse-keymap)))
    ;; "reset" marked hunks
    (define-key map "r" 'gpb-git:unstage-hunks)
    (set-keymap-parent map 'gpb-git:hunk-selection-mode-map)
    (fset 'gpb-git:staged-changes-mode-map map)
    map)
  "The keymap used for staged hunks.")



(define-derived-mode gpb-git:hunk-view-mode special-mode
  "Hunk Buffer"
  "\nBase mode for buffers showing hunks."
  (setq-local header-line-format '(:eval (gpb-git--compute-hunk-buffer-header)))
  (setq-local buffer-read-only t)
  (setq-local tab-width 4))

(define-derived-mode gpb-git:hunk-selection-mode gpb-git:hunk-view-mode
  "Hunk Buffer"
  "\nBase mode for buffers showing hunks."
  (add-hook 'post-command-hook 'gpb-git--post-command-hook))


(define-derived-mode gpb-git:unstaged-changes-mode gpb-git:hunk-selection-mode
  "Unstaged Changes"
  "\nMode for selecting unstages changes to be added to the index.

\\{gpb-git:unstaged-changes-mode-map}\n"
  (setq-local staged-changes-buffer nil))


(define-derived-mode gpb-git:staged-changes-mode gpb-git:hunk-selection-mode
  "Staged Changes"
  "\nMode for selecting stages changes to be removed from the index.

\\{gpb-git:staged-changes-mode-map}\n"
  (setq-local staged-changes-buffer t))


(defun gpb-git:show-unstaged-changes (&optional repo-dir)
  (interactive (list (gpb-git--read-repo-dir)))
  (let ((buf (get-buffer-create gpb-git:unstaged-buffer-name)))
    (with-current-buffer buf
      (gpb-git--refresh-unstaged-changes repo-dir)
      ;; For `gpb-git:refresh-buffer'
      (setq-local refresh-cmd `(gpb-git--refresh-unstaged-changes
                                ,default-directory)))
    (switch-to-buffer buf)))


(defun gpb-git:show-staged-changes (&optional repo-dir)
  (interactive (list (gpb-git--read-repo-dir)))
  (let ((buf (get-buffer-create gpb-git:staged-buffer-name)))
    (with-current-buffer buf
      (gpb-git--refresh-staged-changes repo-dir)
      ;; For `gpb-git:refresh-buffer'
      (setq-local refresh-cmd `(gpb-git--refresh-staged-changes
                                ,default-directory)))
    (switch-to-buffer buf)))


(defun gpb-git:show-commit-diff (hash1 hash2 &optional repo-dir)
  (let ((buf (get-buffer-create (format "*%s...%s*" hash1 hash2))))
    (with-current-buffer buf
      (gpb-git--refresh-commit-diff hash1 hash2 repo-dir)
      ;; For `gpb-git:refresh-buffer'
      (setq-local refresh-cmd `(gpb-git--refresh-commit-diff
                                ,hash1 ,hash2 ,default-directory))
      (switch-to-buffer buf))))


(defun gpb-git--refresh-unstaged-changes (&optional repo-dir cmd callback)
  "Display the differences between the index and HEAD.

Overwrites the current buffer and sets the mode to
`gpb-git:unstaged-changes-mode'."
  (interactive)
  (let ((cmd (or cmd '("git" "diff" "--histogram" "--find-renames")))
        (inhibit-read-only t))
    (gpb-git--refresh-changes cmd repo-dir callback)
    (gpb-git:unstaged-changes-mode)
    (goto-char (point-min))
    (insert (format "\nUnstaged changes in %s\n\n"
                    (gpb-git--abbreviate-file-name default-directory)))
    (insert (format "%s\n\n" (mapconcat 'identity cmd " ")))
    (goto-char (point-min))
    ;; This didn't seem to stick until I moved it here?
    (setq-local refresh-cmd `(gpb-git--refresh-unstaged-changes
                              ,default-directory ',cmd))))


(defun gpb-git--refresh-staged-changes (&optional repo-dir cmd callback)
  "Display the differences between the index and HEAD.

Overwrites the current buffer and sets the mode to
`gpb-git:staged-changes-mode'."
  (interactive)
  (let ((cmd (or cmd '("git" "diff" "--cached" "--histogram" "--find-renames")))
        (inhibit-read-only t))
    (gpb-git--refresh-changes cmd repo-dir callback)
    (gpb-git:staged-changes-mode)
    (goto-char (point-min))
    (insert (format "\nStaged changes in %s\n\n"
                    (gpb-git--abbreviate-file-name default-directory)))
    (insert (format "%s\n\n" (mapconcat 'identity cmd " ")))
    (setq-local refresh-cmd `(gpb-git--refresh-staged-changes
                              ,default-directory ',cmd))
    (goto-char (point-min))))


(defun gpb-git--refresh-commit-diff (hash1 hash2 &optional repo-dir callback)
  "Display the differences between the index and HEAD.

Overwrites the current buffer and sets the mode to
`gpb-git:staged-changes-mode'."
  (interactive)
  (let ((cmd `("git" "diff" "--stat" "--patch" "--histogram" "--find-renames"
               ,hash1 ,hash2 "--"))
        (inhibit-read-only t)
        (repo-dir (or repo-dir default-directory)))
    (gpb-git--refresh-changes cmd repo-dir callback)
    (gpb-git:hunk-view-mode)
    (goto-char (point-min))
    (insert (format "\nChanges from %s to %s\n\n" hash1 hash2))
    (insert (format "%s\n\n" (mapconcat 'identity cmd " ")))
    (setq-local refresh-cmd `(gpb-git--refresh-commit-diff
                              ,hash1 ,hash2 ,repo-dir))
    (goto-char (point-min))))


(defun gpb-git--refresh-changes (cmd &optional repo-dir callback)
  "Update the diff hunks in a buffer.

Executes `cmd', parses the result, erases the current buffer,
sets the major mode to `major-mode', writes the parsed hunks into
the buffer and adds overlays.  `cmd' is a list of strings.  If
`callback' is non-nil, we call this function when the buffer has
been updated (i.e., asyncronously)."
  (interactive)
  (let ((repo-dir (or repo-dir default-directory))
        (inhibit-read-only t))
    ;; Delete any existing hunk overlays in the buffer.
    (dolist (ov (gpb-git--get-hunk-overlays)) (delete-overlay ov))
    (erase-buffer)
    (setq default-directory repo-dir)
    ;; We save the command for `gpb-git--refresh-changes'.
    (setq current-command cmd)
    (gpb-git:insert-placeholder "Loading hunks ")
  (gpb-git:exec-async cmd repo-dir #'gpb-git--refresh-changes-1 callback)))


(defun gpb-git--refresh-changes-1 (buf callback)
"Implementation detail of `gpb-git--refresh-changes'."
  (let ((hunks (with-current-buffer buf (gpb-git--parse-diff)))
        (inhibit-read-only t))
    (goto-char (point-min))
    (gpb-git:delete-placeholder "Loading hunks")
    (cond
     (hunks
      ;; Add a text button for each filename
      (insert "Files: ")
      (let* ((filenames (mapcar (lambda (hunk) (aget hunk :filename1 t))
                                hunks))
             (max-length (apply 'max (mapcar 'length filenames))))
        (dolist (filename (sort (delete-dups (delq nil filenames)) 'string<))
          (make-text-button (point)
                            (progn (insert filename) (point))
                            'action 'gpb-git:jump-to-file-hunks
                            'filename filename)
          (insert (make-string (max (- max-length (length filename)) 0)
                               ?\ ))
          (let ((file-hunks (cl-remove-if-not
                             (lambda (h) (equal (aget h :filename1 t) filename))
                             hunks)))
            (insert (cond
                     ((= (length file-hunks) 1) " (1 hunk)")
                     (t (format " (%s hunks)"
                                (length file-hunks))))))
          (insert "\n       ")))
      (forward-line 0)
      (kill-line)
      (insert "\n\n")
      (gpb-git--insert-hunks hunks))
     (t
      (insert "No changes")))
    (goto-char (point-min))
    (when callback (funcall callback))))


(defun gpb-git--decorate-hunk (hunk &optional focused)
  "Apply faces to hunk text.
HUNK is an overlay with properties summarized at the top of this
file.  If FOCUSED is non-nil, we use alternative faces."
  (assert (and (overlayp hunk) (overlay-buffer hunk)))
  (with-current-buffer (overlay-buffer hunk)
    (let* ((beg (overlay-start hunk))
           (end (copy-marker (overlay-end hunk)))
           (marked (gpb-git--marked-p hunk))
           (inhibit-read-only t) (i 0) marked-lines line-marked)

      (save-mark-and-excursion
        (goto-char beg)

        ;; Update the hunk header.  We only delete it if it has changed as
        ;; deleting text and adding it back can interact poorly with
        ;; isearch.
        (let ((current-header (and (get-text-property (point) :hunk-header)
                                   (buffer-substring-no-properties
                                    (point) (save-excursion
                                              (forward-line 1) (point)))))
              (new-header (gpb-git:get-hunk-header hunk)))
          (unless (equal current-header new-header)
            (when current-header
              (delete-region (point) (save-excursion (forward-line 1)
                                                     (point))))
            (save-excursion (insert new-header))))

        ;; Update the text properites
        (add-text-properties
         (point) (progn (forward-line 1) (point))
         (list :hunk-header t
               'face (cond
                      ((and marked focused)
                       'gpb-git:focused-and-marked-hunk-header)
                      (marked 'gpb-git:marked-hunk-header)
                      (focused 'gpb-git:focused-hunk-header)
                      (t 'gpb-git:hunk-header))))

        ;; We wait until we have written the header to call
        ;; `gpb-git--get-marked-lines' as this function assumes that the
        ;; header line has been written.
        (setq marked-lines (gpb-git--get-marked-lines hunk))

        (while (< (point) end)
          (setq line-marked (aref marked-lines i))
          (cond
           ((looking-at-p "^+")
            (put-text-property (point) (progn (forward-line 1) (point))
                               'face (cond
                                      ((and line-marked focused)
                                       'gpb-git:focused-and-marked-added-line)
                                      (line-marked 'gpb-git:marked-added-line)
                                      (focused 'gpb-git:focused-added-line)
                                      (t 'gpb-git:added-line))))
           ((looking-at-p "^-")
            (put-text-property (point) (progn (forward-line 1) (point))
                               'face (cond
                                      ((and line-marked focused)
                                       'gpb-git:focused-and-marked-deleted-line)
                                      (line-marked 'gpb-git:marked-deleted-line)
                                      (focused 'gpb-git:focused-deleted-line)
                                      (t 'gpb-git:deleted-line))))
           (t
            (put-text-property (point) (progn (forward-line 1) (point))
                               'face (cond
                                      ((and line-marked focused)
                                       'gpb-git:focused-and-marked-context-line)
                                      (line-marked 'gpb-git:marked-context-line)
                                      (focused 'gpb-git:focused-context-line)
                                      (t 'gpb-git:context-line)))))
          (incf i))))))


(defun gpb-git:forward-hunk ()
  "Move point forward to the next hunk."
  (interactive)
  (when (eobp) (error "End of buffer"))
  (let ((pt (point))
        (end (window-end))
        (hunk (gpb-git:get-current-hunk)))
    ;; Move the end of the current hunk.
    (when hunk (goto-char (overlay-end hunk)))
    ;; Now move forward until you are inside a hunk.  This currently
    ;; doesn't happen, as the hunks are contiguous.
    (while (not (or (eobp) (gpb-git:get-current-hunk))) (forward-line 1))
    ;; If we moved to the end of the buffer, there is not next hunk.
    (when (eobp) (goto-char pt) (error "Last hunk")))
    (point))


(defun gpb-git:forward-command ()
  "Move forward to the next hunk or button and scroll window."
  (interactive)
  (unless (ignore-errors (forward-button 1))
    (gpb-git:forward-hunk)
    (let ((ov (gpb-git:get-current-hunk))
          (win (selected-window)))
      (when (and ov (overlay-buffer ov) win)
        (assert (eq (window-buffer win) (current-buffer)))
        ;; Ensure that the full hunk is visible when possible.
        (save-excursion
          (save-match-data
            (goto-char (overlay-start ov))
            (recenter 0)))))))


(defun gpb-git:backward-hunk ()
  "Move back to the start of the current hunk.
Move back to the start of the previous hunk, if we are already at
the start of the current hunk."
  (interactive)
  (when (= (line-number-at-pos) (point-min)) (error "Beginning of buffer"))
  (let ((pt (point)) hunk)
    (forward-line -1)
    (while (and (not (gpb-git:get-current-hunk)) (not (bobp)))
      (forward-line -1))
    (when (bobp)
      (goto-char pt)
      (error "No earlier hunks"))
    (setq hunk (gpb-git:get-current-hunk))
    (assert hunk)
    (goto-char (overlay-start hunk))
    (point)))


(defun gpb-git:forward-file-command ()
  "Move forward to first hunk of the next file."
  (interactive)
  (let* ((get-current-file (lambda () (ignore-errors
                                        (overlay-get (gpb-git:get-current-hunk)
                                                     :filename1))))
         (current-file (funcall get-current-file))
         new-point)
    (save-excursion
      (while (equal (funcall get-current-file) current-file)
        ;; `gpb-git:forward-hunk-command' signals an error when it fails.
        (or (ignore-errors (gpb-git:forward-hunk))
            (error "Last file")))
      (setq new-point (point)))
    (goto-char new-point)
    (recenter 0)))


(defun gpb-git:backward-file-command ()
  "Move backwards to first hunk of the previous file."
  (interactive)
  (let* ((get-file-at-point (lambda () (ignore-errors
                                         (overlay-get (gpb-git:get-current-hunk)
                                                      :filename1))))
         filename new-point first-hunk)

    (save-excursion
      (gpb-git:backward-hunk)
      (setq filename (funcall get-file-at-point))

      ;; `gpb-git:backward-hunk' signals an error when it fails.
      (ignore-errors
        (while (equal (funcall get-file-at-point) filename)
          (gpb-git:backward-hunk)))
      (assert (not (null (funcall get-file-at-point))))

      ;; Unless we are on the first hunk, we moved back one-to-far.
      (if (equal (funcall get-file-at-point) filename)
          (setq first-hunk t)
        (gpb-git:forward-command))
      (setq new-point (point)))

    (goto-char new-point)
    (when first-hunk
      (recenter (line-number-at-pos)))))



(defun gpb-git:backward-command ()
  "Move back to the start of the previous hunk or button."
  (interactive)
  (or (ignore-errors (gpb-git:backward-hunk))
      (backward-button 1)))


(defun gpb-git:mark-hunk-command ()
  "Mark the current hunk."
  (interactive)
  (let ((region-active (region-active-p)))
    (gpb-git:mark-hunk)
    (unless region-active (gpb-git:forward-command))))


(defun gpb-git:mark-hunk (&optional unmark new-name)
  "Mark the hunk at the current point.
If a region is active, we only mark the lines of the hunk that
intersect the region.  If UNMARK is non-nil we unmark the hunk or
lines within the hunk and ignore NEW-NAME.  If NEW-NAME is
non-nil, we mark the entire hunk as a rename.  This is only
allowed when the hunk corresponds to a file deletion and the
region is not active."
  (interactive)
  (let* ((hunk (or (gpb-git:get-current-hunk)
                   (user-error "Point is not in a hunk")))
         (filename1 (overlay-get hunk :filename1))
         ;; Number of lines in the hunk not counting the header line.
         (nlines (- (line-number-at-pos (overlay-end hunk))
                    (1+ (line-number-at-pos (overlay-start hunk)))))
         (inhibit-read-only t))

    (when new-name
      (unless (overlay-get hunk :deletion)
        (user-error "Only deleted files may be marked as renames."))
      (when (use-region-p)
        (user-error "Renames may only be applied to a full hunk.")))

    (cond
     ;; Mark or unmark a subset of the current hunk.
     ((use-region-p)
      (let ((beg (region-beginning))
            (end (region-end))
            (line-is-marked (gpb-git--get-marked-lines hunk))
            (i 0))
        ;; Ensure `beg' lies at the start of the line.
        (setq beg (save-excursion (goto-char beg) (forward-line 0) (point)))
        (save-excursion
          (goto-char (overlay-start hunk))
          ;;Skip past the header line.
          (forward-line 1)
          (while (< (point) (overlay-end hunk))
            (when (and (>= (point) beg) (< (point) end))
              (aset line-is-marked i (not unmark)))
            (forward-line 1)
            (incf i)))
        (if (cl-some (lambda (x) x) line-is-marked)
            (overlay-put hunk :marked `(:partial . ,line-is-marked))
          (overlay-put hunk :marked nil))
        (deactivate-mark t)))

     (unmark (overlay-put hunk :marked nil))
     (new-name (overlay-put hunk :marked `(:rename . ,new-name)))
     (t (overlay-put hunk :marked t)))

    ;; Update the text properties to reflect the new state of the hunk.
    (gpb-git--decorate-hunk hunk)
    (setq gpb-git:currently-focused-hunk nil)))


(defun gpb-git:unmark-hunk (&optional unmark new-name)
  "Unmark the current hunk."
  (interactive)
  (gpb-git:mark-hunk 'unmark))


(defun gpb-git:mark-file-command (&optional unmark)
  "Mark all hunks in the current file.
If UNMARK is non-nil we unmark all hunks in the current file."
  (interactive)
  (let* ((hunk (or (gpb-git:get-current-hunk)
                   (user-error "Point is not in a hunk")))
         (filename1 (overlay-get hunk :filename1))
         (pt 0))
    (dolist (hunk (gpb-git--get-hunk-overlays filename1))
      (goto-char (overlay-start hunk))
      (setq pt (max pt (point)))
      (gpb-git:mark-hunk unmark))
    (goto-char pt)
    (gpb-git:forward-command)))


(defun gpb-git:unmark-file-command ()
  "Unmark all hunks in the current file."
  (interactive)
  (gpb-git:mark-file-command t))


(defun gpb-git:mark-as-rename (new-name)
  "Mark file deletion hunk as a rename.

Prompts the user for the new file name and marks the file as a
rename.  When staged, the current filename will be removed from
the index, but the contents will be added under the new name.

This can be useful when the file has been renamed and modified,
but you only want to commit the rename.  Any changes to contents
of the file will then show up as unstaged changes to the new file
name."
  (interactive
   (list (progn
           (let ((hunk (gpb-git:get-current-hunk)))
             (unless hunk (user-error "Point is not in a hunk renames"))
             (unless (overlay-get hunk :deletion)
               (user-error "Only deleted hunks can be marked as a rename")))
           (file-relative-name (read-file-name "New name: ")))))
  (gpb-git:mark-hunk nil new-name))


(defun gpb-git:unmark-hunk-command ()
  "Mark the current hunk."
  (interactive)
  (gpb-git:unmark-hunk)
  (gpb-git:forward-command))


(defun gpb-git:get-marked-hunks (&optional buf)
  "Return the list of marked hunks in `buf'"
  (with-current-buffer (or buf (current-buffer))
    (sort
     (remove-if-not 'gpb-git--marked-p
                    (overlays-in (point-min) (point-max)))
     (lambda (ov1 ov2) (< (overlay-start ov1) (overlay-start ov2))))))


(defun gpb-git--update-highlights (&optional buf)
  "Updates hunk highlighting.

Implementation detail of `gpb-git--post-command-hook'.  The
function adds additional highlighting to the hunk that currently
contains the point and removes any highlighting from the
previously highlighted hunk."
  (with-current-buffer (window-buffer (selected-window))
    (let* ((prev-hunk (default-value 'gpb-git:currently-focused-hunk))
           (new-hunk  (gpb-git:get-current-hunk))
           ;; We don't want the changes below to deactivate the mark.
           deactivate-mark)
      (when (not (eq new-hunk prev-hunk))

        (when (and prev-hunk (overlay-buffer prev-hunk))
          (gpb-git--decorate-hunk prev-hunk))

        (when (and new-hunk (overlay-buffer new-hunk))
          (gpb-git--decorate-hunk new-hunk t))

        (set-default 'gpb-git:currently-focused-hunk new-hunk)))))


(defun gpb-git--compute-hunk-buffer-header ()
  "Construct a description string for the buffer header line."
  (let* ((ov (gpb-git:get-current-hunk))
         (window-start (window-start))
         (window-width (window-width))
         (face '((:height 160) diff-file-header diff-header))
         (files (gpb-git:get-hunk-filenames))
         (hunk-overlays (gpb-git--get-hunk-overlays)))
    (cond
     (ov
      (let* ((current-file (overlay-get ov :filename1))
             (file-hunk-overlays (gpb-git--get-hunk-overlays current-file))
             (hunks-before-pt (cl-remove-if
                               (lambda (ov)
                                 (> (overlay-start ov) (point)))
                               hunk-overlays))
             (file-hunks-before-pt (cl-remove-if
                               (lambda (ov)
                                 (> (overlay-start ov) (point)))
                               file-hunk-overlays))
             (files-before-pt (cl-remove-if (lambda (fn)
                                              (string< current-file fn))
                                            files)))
        (gpb-git--center-string
         (format "hunk %s/%s in %s  (file %s/%s, hunk %s/%s)"
                 (length file-hunks-before-pt)
                 (length file-hunk-overlays)
                 current-file
                 (length files-before-pt)
                 (length files)
                 (length hunks-before-pt)
                 (length hunk-overlays)))))
     (t (gpb-git--center-string
         (format "%s file%s, %s hunks"
                 (length files)
                 (if (> (length files) 1) "s" "")
                 (length hunk-overlays)))))))



(defun gpb-git:stage-hunks ()
  "Apply some hunks to the index."
  (interactive)
  (when (region-active-p) (gpb-git:mark-hunk))
  (let ((marked-hunks (or (gpb-git:get-marked-hunks)
                          ;; If nothing is marked, mark the current hunk
                          ;; and proceed.
                          (progn (gpb-git:mark-hunk)
                                 (gpb-git:get-marked-hunks)))))

    (let ((patch-file (gpb-git--apply-hunks marked-hunks
                                            "apply" "--cached")))
      (delete-file patch-file))

    (gpb-git:refresh-status)
    (cond
     ((> (length marked-hunks) 1)
      (quit-window t))
     (t
      (gpb-git--refresh-unstaged-changes)))))


(defun gpb-git:unstage-hunks ()
  "Remove some hunks from the index."
  (interactive)
  (when (region-active-p) (gpb-git:mark-hunk))
  (let ((marked-hunks (or (gpb-git:get-marked-hunks)
                          ;; If nothing is marked, mark the current hunk
                          ;; and proceed.
                          (progn (gpb-git:mark-hunk)
                                 (gpb-git:get-marked-hunks)))))

    (let ((patch-file (gpb-git--apply-hunks marked-hunks
                                            "apply" "--cached" "-R")))
      (delete-file patch-file))
    (gpb-git:refresh-status)
    (cond
     ((> (length marked-hunks) 1)
      (quit-window t))
     (t
      (gpb-git--refresh-unstaged-changes)))))


(defun gpb-git:revert-marked-hunks ()
  "Revert the marked changes in the working directory.

Prints a message giving the name of the patch file that was
applied in reverse.  If you make a mistake and remove change you
wanted from the working tree, you can revert this revert by
applying this patch file to the working directory."
  (interactive)
  (when (y-or-n-p "Revert marked changes in the working directory? ")
    (let ((patch-file (gpb-git--apply-hunks "apply" "-R")))
      ;; This is a potentially destructive operation, so we leave the patch
      ;; file intact and let the user know it exists.
      (message "Successfully applied %s" patch-file))))


(defun gpb-git--apply-hunks (hunks &rest args)
  "Apply marked hunks using ARGS.

Creates a new patch file in `default-directory' and returns the
name of this file."
  (interactive)
  (let* ((patch-file (make-temp-file (concat default-directory "gpb-git-")
                                     nil ".patch"))
         (localname (file-relative-name patch-file))
         (proc-output-buf gpb-git:process-output-buffer-name)
         (args (append args `(,localname))))

    (with-current-buffer (gpb-git:make-patch hunks (member "-R" args))
      (let ((coding-system-for-write 'unix))
        (write-region (point-min) (point-max) patch-file)))

    (with-current-buffer (get-buffer-create proc-output-buf)
      (erase-buffer)
      (insert (format "git %s\n" (mapconcat 'identity args " "))))

    (setq retvalue (apply 'process-file "git" nil proc-output-buf t args))

    ;; If patch application failed, we pop to the process output.
    (unless (= retvalue 0) (pop-to-buffer proc-output-buf))
    patch-file))


(defun gpb-git--get-hunk-overlays (&optional filename)
  "Get a list of hunk overlays.

If an optional FILENAME is given, only return the hunks that
modify the given file."
  (let ((hunks (cl-remove-if-not (lambda (ov) (overlay-get ov :is-hunk))
                                 (overlays-in (point-min) (point-max)))))
    (if filename
        ;; If a filename is given, further filter the hunks.
        (cl-remove-if-not (lambda (ov) (string= (overlay-get ov :filename1)
                                                filename))
                          hunks)
      hunks)))


(defun gpb-git:get-current-hunk (&optional pos)
  "Return the hunk overlay at POS.
If POS is omitted, we return the hunk at the current point or the
start of the reigon, if the region is active.
"
  (let ((pos (or pos (and (region-active-p) (region-beginning)) (point))))
    (cdr (get-char-property-and-overlay pos :is-hunk))))


(defun gpb-git:get-hunk-filenames ()
  "Get a list of hunk filenames."
  (sort (delete-dups
         (mapcar (lambda (ov) (overlay-get ov :filename1))
                 (gpb-git--get-hunk-overlays)))
        'string<))


(defun gpb-git:get-hunk-header (hunk)
  "Create the string which appears at the start of a hunk."
  (let* ((staged (with-current-buffer (overlay-buffer hunk)
                   (and (boundp 'staged-changes-buffer)
                        staged-changes-buffer)))
         (filename1 (overlay-get hunk :filename1))
         (filename2 (overlay-get hunk :filename2)))

    (cond
     ;; A deletion that has been marked as a rename
     ((gpb-git--marked-as-rename-p hunk)
      (let* ((filename (overlay-get hunk :filename1))
             (new-name (gpb-git--get-new-name hunk)))
        (format "%s: delete (marked as rename to %s)\n" filename new-name)))

     ;; New file
     ((overlay-get hunk :insertion)
      (assert (string= filename1 filename2))
      (format "%s: %s\n" filename1 (if staged "added" "add")))

     ;; Deleted file
     ((overlay-get hunk :deletion)
      (assert (string= filename1 filename2))
      (format "%s: %s\n" filename1 (if staged "deleted" "delete")))

     ;; A standard hunk with diff lines.
     ((ignore-errors (overlay-get hunk :diff))
      (let* ((file1-start (overlay-get hunk :file1-start))
             (file2-start (overlay-get hunk :file2-start))
             (file1-len (overlay-get hunk :file1-len))
             (file2-len (overlay-get hunk :file2-len))
             (file1-end (1- (+ file1-start file1-len)))
             (file2-end (1- (+ file2-start file2-len)))
             (line-range (format "%s-%s -> %s-%s"
                                 file1-start file1-end
                                 file2-start file2-end)))
        (if (string= filename1 filename2)
            (format "%s: %s\n" filename1 line-range)
          (format "%s -> %s: %s\n" filename1 filename2 line-range))))

     ;; A rename with no other changes to the file.
     ((overlay-get hunk :rename)
      (format "%s -> %s\n" filename1 filename2))

     ((overlay-get hunk :binary-info)
      (format "%s\n" filename1))

     (t
      (error "Assertion error: unhandled hunk %S" hunk)))))


(defun gpb-git:make-patch (hunks &optional reverse)
  "Construct a patch from the list of hunks.

If REVERSE is non-nil, we construct the patch so that it can be
applied in reverse.  This is used when removing changes from the
index.

Returns a buffer whose name is determined by
`gpb-git:patch-buffer-name' that contains the patch."
  (when (= (length hunks) 0)
    (error "No hunks have been marked"))

  (let* ((patch-buf (get-buffer-create gpb-git:patch-buffer-name))
         ;; These are used in `gpb-git:make-patch-1'
         (output-offset 0)
         current-output-file)
    (with-current-buffer patch-buf (erase-buffer))
    (dolist (hunk hunks)
      (cond
       ;; A deletion that has been marked as a rename.
       ((gpb-git--marked-as-rename-p hunk)
        (let* ((filename1 (overlay-get hunk :filename1))
               (filename2 (overlay-get hunk :filename2))
               (new-name (gpb-git--get-new-name hunk)))
          (assert (string= filename1 filename2))
          (with-current-buffer patch-buf
            (insert (format
                     (concat "diff --git a/%s b/%s\n"
                             "rename from %s\n"
                             "rename to %s\n")
                     filename1 new-name filename1 new-name)))))
       ;; If the hunk is marked and there are no diff lines, insert the
       ;; header.
       ((and (gpb-git--marked-p hunk)
             (not (overlay-get hunk :diff)))
        (with-current-buffer patch-buf
          (insert (overlay-get hunk :header))))
       ;; Otherwise we use the primary algorithm in `gpb-git:make-patch-1'.
       (t
        (gpb-git:make-patch-1 hunk patch-buf reverse))))

    patch-buf))


(defun gpb-git:make-patch-1 (hunk patch-buf reverse)
  "Write patch content for HUNK into PATCH-BUF.

This function is an implemenation detail of `gpb-git:make-patch'."
  (let* ((header (overlay-get hunk :header))
         (input-file (if reverse (overlay-get hunk :filename2)
                       (overlay-get hunk :filename1)))
         (output-file (if reverse (overlay-get hunk :filename1)
                        (overlay-get hunk :filename2)))
         (input-start (if reverse (overlay-get hunk :file2-start)
                        (overlay-get hunk :file1-start)))
         (new-file (if reverse (overlay-get hunk :deletion)
                     (overlay-get hunk :insertion)))
         (marked-lines (gpb-git--get-marked-lines hunk))
         (patch-start (with-current-buffer patch-buf (point)))
         (has-no-changes t)
         (input-lines 0)
         (output-lines 0)
         (i 0)
         diff-line first-char)

    ;; If we have changed files, return the offset between hunks in the
    ;; input file and hunks in the output file.
    (unless (equal output-file current-output-file)
      (setq current-output-file output-file
            output-offset 0))

    (save-excursion
      (goto-char (overlay-start hunk))
      ;; Move past the hunk header to the first line of the diff
      (forward-line 1)
      (while (< (point) (overlay-end hunk))
        (setq diff-line (buffer-substring-no-properties
                         (point)
                         (progn (forward-line 1) (point)))
              include-line (aref marked-lines i)
              first-char (substring diff-line 0 1)
              i (1+ i))

        (when reverse
          (setq first-char (aget `((" " . " ") ("+" . "-") ("-" . "+"))
                                 first-char)))

        ;; We have a removed line in the diff that we don't want to
        ;; include, so we convert it to a context line.
        (when (and (not include-line) (equal first-char "-"))
          (aset diff-line 0 ?\ )
          (setq first-char " "))

        (cond
         ;; We always keep context lines. They appear in both the
         ;; input and ouput files.
         ((equal first-char " ")
          (with-current-buffer patch-buf (insert diff-line))
          (incf input-lines)
          (incf output-lines))

         ;; We have an added or removed line that we want to include.
         (include-line
          (with-current-buffer patch-buf (insert diff-line))
          ;; A deleted line appears in the input, but not the output.
          (when (equal first-char "-") (incf input-lines))
          ;; An added line appears in the output, but not the input.
          (when (equal first-char "+") (incf output-lines))
          (setq has-no-changes nil)))))

    (with-current-buffer patch-buf
      (if has-no-changes
          ;; There were no changes included in the patch so we have
          ;; only context lines.  Git treats such a hunk as an error,
          ;; so we delete it.
          (delete-region patch-start (point))
        (save-excursion
          (goto-char patch-start)
          (cond
           ;; If we have staged a file that was renamed and modified, and
           ;; we have selected a subset of these changes, we only want roll
           ;; back the changes, not the the rename.  Recall that
           ;; `input-file' is the second file in the hunk header in this
           ;; case because we are operating in reverse.
           ((and reverse marked-lines (overlay-get hunk :rename))
            (insert (format "diff --git a/%s b/%s\n--- a/%s\n+++ b/%s\n"
                            input-file input-file input-file input-file)))
           ;; Otherwise, insert the header as is.
           (t (insert header)))

          (if reverse
              (insert (format "@@ -%s,%s +%s,%s @@\n"
                              (if new-file 1 (+ input-start output-offset))
                              output-lines
                              input-start input-lines))
            (insert (format "@@ -%s,%s +%s,%s @@\n"
                            input-start input-lines
                            (if new-file 1 (+ input-start output-offset))
                            output-lines))))))

    ;; If there is a difference between the number of input and output
    ;; lines, the next hunk for this file will start at different lines
    ;; in the input and output files.
    (setq output-offset (+ output-offset (- output-lines input-lines)))))




(defun gpb-git--marked-p (hunk)
  (not (null (overlay-get hunk :marked))))

(defun gpb-git--marked-as-rename-p (hunk)
    (eq (car-safe (overlay-get hunk :marked)) :rename))

(defun gpb-git--get-new-name (hunk)
  (let ((marked (overlay-get hunk :marked)))
    (when (eq (car-safe marked) :rename)
      (cdr marked))))



(defun gpb-git--get-marked-lines (hunk)
  "Return the set of line in `hunk' that are marked.
`hunk' is an overlay with associated data.  Returns an array of
bools in which the i-th entry is true if i-th line in the hunk is
marked."
  (let ((marked (overlay-get hunk :marked))
        ;; (1+ (line-number-at-pos (overlay-start hunk))) is the first diff
        ;; line and (line-number-at-pos (overlay-end hunk)) is the line after
        ;; the last diff line.
        (nlines (- (line-number-at-pos (overlay-end hunk))
                   (1+ (line-number-at-pos (overlay-start hunk))))))
    (cond
     ((eq (car-safe marked) :partial)
      (cdr marked))
     (marked
      (make-vector nlines t))
     (t
      (make-vector nlines nil)))))


(defun gpb-git:jump-to-file-hunks (obj)
  "Goto the first hunk associated with the given file.

OBJ is a string giving a filename or a button with a filename
property."
  (setq obj (or (button-get obj 'filename) obj))
  (let ((hunks (gpb-git--get-hunk-overlays obj)))
    (push-mark)
    ;; Scroll the end of the last hunk into the window.
    (goto-char (overlay-end (car (last hunks))))
    (forward-line -1)
    (recenter -1)
    ;; Then jump back to the start of the first hunk.
    (goto-char (overlay-start (car hunks)))))



(defun gpb-git:goto-line (&optional hunk)
  (interactive)
  (let* ((pt (point))
         (hunk (or hunk (gpb-git:get-current-hunk)))
         (filename (overlay-get hunk :filename2))
         (line-number (overlay-get hunk :file2-start)))

    (save-excursion
      (goto-char (overlay-start hunk))
      (forward-line 1)
      (while (< (point) pt)
        (when (looking-at-p "^[ +]") (incf line-number))
        (forward-line 1)))

    (find-file-other-window filename)
    (goto-line line-number)
    (set-window-point (selected-window) (point))))


(defun gpb-git--show-hunk-faces ()
  "Show all the faces in a test buffer."
  (interactive)
  (let ((text (concat " Some context\n-Removed line\n"
                      "+Added line\n Some more context\n"))
        (props '((:filename1 . "gpb-misc.el")
                 (:file1-start . 7)
                 (:file1-len . 6)
                 (:filename2 . "gpb-misc.el")
                 (:file2-start . 7)
                 (:file2-len . 9)
                 (:header . "")
                 (:diff . "stuff")
                 (:rename . nil)
                 (:deletion . nil)
                 (:insertion . nil)))
        ov1 ov2 ov3 ov4)
  (with-current-buffer (get-buffer-create "*gpb-git faces*")
    (erase-buffer)
    (setq-local staged-changes-buffer nil)
    (setq-local gpb-git:show-faces-buffer t)

    (insert "Faces used to display hunks\n")
    (insert "===========================\n\n")

    (insert "Base:\n")
    (setq ov1 (make-overlay (point) (progn (insert text) (point))))
    (dolist (key-val props) (overlay-put ov1 (car key-val) (cdr key-val)))
    (gpb-git--decorate-hunk ov1)

    (insert "\nFocused:\n")

    (setq ov2 (make-overlay (point) (progn (insert text) (point))))
    (dolist (key-val props) (overlay-put ov2 (car key-val) (cdr key-val)))
    (gpb-git--decorate-hunk ov2 t)

    (insert "\nMarked:\n")

    (setq ov3 (make-overlay (point) (progn (insert text) (point))))
    (dolist (key-val props) (overlay-put ov3 (car key-val) (cdr key-val)))
    (overlay-put ov3 :marked t)
    (gpb-git--decorate-hunk ov3)

    (insert "\nFocused and Marked:\n")

    (setq ov4 (make-overlay (point) (progn (insert text) (point))))
    (dolist (key-val props) (overlay-put ov4 (car key-val) (cdr key-val)))
    (overlay-put ov4 :marked t)
    (gpb-git--decorate-hunk ov4 t)

    (pop-to-buffer (current-buffer)))))


(defun gpb-git--insert-hunks (diff-hunks)
  ;; Insert the hunks
  (save-excursion
    (let* ((inhibit-read-only t))
      (dolist (diff-hunk diff-hunks)
        (let* ((filename1 (aget diff-hunk :filename1 t))
               (filename2 (aget diff-hunk :filename2 t)))
          (setq ov (make-overlay (point)
                                 (progn
                                   (insert (or (aget diff-hunk :diff t)
                                               (aget diff-hunk :binary-info t)
                                               " No differences\n"))
                                   (point))))
          (dolist (key-val diff-hunk)
            (overlay-put ov (car key-val) (cdr key-val)))
          (overlay-put ov :is-hunk t)
          (overlay-put ov :marked nil)
          (gpb-git--decorate-hunk ov))))
    (setq-local hunks-available t)))


(defun gpb-git--parse-diff (&optional beg end)
  "Parse Git diff output.

Returns a list of hunk alists.  See the comments at the top of
the file for the structure of these alists."
  (let* ((region-beg (or beg (point-min)))
         (region-end (or end (point-max)))
         (hunk-list (list :stub))
         ;; Some hunks consist of a file header have no trailing diff
         ;; sections (e.g., renames with 100% similarity and empty files
         ;; staged with --intent-to-add).  We set add-diffless-hunk to t
         ;; when we see a file header, and set it back to nil when we see
         ;; the first diff section.  If we see a file header and
         ;; add-diffless-hunk is t, we know that the previous file header
         ;; had no diff sections.
         (add-diffless-hunk nil)
         beg end insertion deletion filename1 filename2 header binary-info)
    (save-restriction
      (narrow-to-region region-beg region-end)
      (save-excursion
        (save-match-data
          (goto-char region-beg)
          (if (re-search-forward "^diff --git" nil t)
              (goto-char (match-beginning 0))
            (goto-char (point-max)))
          (while (< (point) region-end)
            (cond
             ((looking-at "^diff --git a/\\(.*\\) b/\\(.*\\)")
              (when add-diffless-hunk
                ;; The previous file header had no trailing diff sections,
                ;; so we need to record it as a diff-less hunk.
                (nconc hunk-list `(((:filename1 . ,filename1)
                                    (:filename2 . ,filename2)
                                    (:file1-start . 0)
                                    (:file1-len . 0)
                                    (:file2-start . 0)
                                    (:file2-len . 0)
                                    (:header . ,header)
                                    (:insertion . ,(when insertion t))
                                    (:deletion . ,(when deletion t))
                                    (:binary-info . ,binary-info)
                                    (:rename . ,(not (string= filename1
                                                              filename2)))))))

              (setq filename1 (substring-no-properties
                               (or (match-string 1) (error "Assertion error")))
                    filename2 (substring-no-properties
                               (or (match-string 2) (error "Assertion error")))
                    beg (point)
                    end (progn
                          (forward-line 1)
                          (or (and
                               (re-search-forward "^\\(@@\\|diff --git\\)"
                                                  nil t)
                               (goto-char (match-beginning 0)))
                              (end-of-buffer))
                          (point))
                    insertion (save-excursion
                                (or (re-search-backward "^--- /dev/null" beg t)
                                    (re-search-backward "^new file" beg t)))
                    deletion (save-excursion
                               (or (re-search-backward "^+++ /dev/null" beg t)
                                   (re-search-backward "^deleted file" beg t)))
                    header (buffer-substring-no-properties beg end)
                    binary-info (save-excursion
                                  (when (re-search-backward "^Binary files"
                                                            beg t)
                                    (buffer-substring-no-properties
                                     (point) (progn (forward-line 1) (point)))))
                    add-diffless-hunk t))

             ((looking-at "^@@ -\\([0-9,]+\\) \\+\\([0-9,]+\\) @@")
              (let* ((range1 (save-match-data
                               (split-string (match-string 1) ",")))
                     (range2 (save-match-data
                               (split-string (match-string 2) ",")))
                     (file1-start (string-to-number (first range1)))
                     (file1-len (string-to-number (or (second range1) "1")))
                     (file2-start (string-to-number (first range2)))
                     (file2-len (string-to-number (or (second range2) "1")))
                     (diff (buffer-substring-no-properties
                            (progn (forward-line 1) (point))
                            (progn
                              (or (and
                                   (re-search-forward "^@@\\|^diff --git " nil t)
                                   (goto-char (match-beginning 0)))
                                  (goto-char (point-max)))
                              (point)))))
                (setq add-diffless-hunk nil)
                (nconc hunk-list `(((:filename1 . ,filename1)
                                    (:file1-start . ,file1-start)
                                    (:file1-len . ,file1-len)
                                    (:filename2 . ,filename2)
                                    (:file2-start . ,file2-start)
                                    (:file2-len . ,file2-len)
                                    (:header . ,header)
                                    (:insertion . ,(when insertion t))
                                    (:deletion . ,(when deletion t))
                                    (:rename . ,(not (string= filename1
                                                              filename2)))
                                    (:diff . ,diff))))))

             (t (error "Assertion error"))))))

      (when add-diffless-hunk
        ;; The last file header had no trailing diff sections.
        (nconc hunk-list `(((:filename1 . ,filename1)
                            (:filename2 . ,filename2)
                            (:file1-start . 0)
                            (:file1-len . 0)
                            (:file2-start . 0)
                            (:file2-len . 0)
                            (:header . ,header)
                            (:insertion . ,(when insertion t))
                            (:deletion . ,(when deletion t))
                            (:binary-info . ,binary-info)
                            (:rename . ,(not (string= filename1
                                                      filename2)))))))

      (cdr hunk-list))))



(provide 'gm-hunks)
