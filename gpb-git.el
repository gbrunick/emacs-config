;;
;;  Tools for staging and unstaging hunks to the Git index
;;
;;  This package provides the command `gpb-git:update-index' which opens
;;  two side-by-side buffers for selecting unstaged changes that should be
;;  applied to the Git index and staged changes to remove.
;;
;;  This package does not attempt to provide a full Git porcelain.  In
;;  particular, you will still need to use the command line (or `vc' or
;;  `magit') to commit the changes after they are staged.
;;
;;  Implementation overview:
;;
;;    We call `git diff HEAD` to find the changes in the working directory
;;    and then call `git diff --cached` to see which of these changes have
;;    already been staged.  If the set of hunks returned by `git diff
;;    --cached` is not a subset of the hunks returned by `git diff HEAD` we
;;    give up; the current GUI doesn't make sense for this case.
;;
;;    We insert the hunks from `git diff HEAD` into two buffers and cover
;;    each hunk with an overlay so we can easily toggle the visibility of
;;    each hunk ensuring that each hunk is always visible in one and only
;;    one of the buffers.  Initially, the first buffer shows the unstaged
;;    changes and the second buffer shows the changes that have been
;;    staged, but the user may then move hunks back and forth between the
;;    buffers.
;;
;;    Once we are done choosing hunks, we then reset the index to HEAD,
;;    write all the hunks in the right buffer to a patch, and apply the
;;    patch to the index via `git apply --cached TEMPFILE`.

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

(defvar-local gpb-git:diff-file-header-alist nil
  "Mapping from filenames to diff section header strings.
These strings are removed from the hunk selection buffers and
stored here so we can add them back when we produce the patch
file for `git apply`.")

(defvar-local gpb-git:file-name-overlay-alist nil
  "Mapping from filenames to overlays on the text showing the filename.
We use these overlays to hide a filename when all of its hunks
are hidden.")

(defvar-local file-hunk-overlays-alist nil
  "Mapping from filenames to lists of overlays that cover hunk diffs")

(defvar-local gpb-git:other-buffer nil
  "The linked buffer that is used for selecting hunks")

(defvar-local gpb-git:unstaged-buffer nil
  "The linked buffer that displays hunks which could be added to the index")

(defvar-local gpb-git:staged-buffer nil
  "The linked buffer that contains hunks to add to the index.")

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


(defun gpb-git:put-text-faces (&optional beg end highlighted)
  "Apply faces to a diff output.
If HIGHLIGHTED is non-nil, we apply a darker of of faces that
are used to the emphasize the hunk that contains the point."
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
          ;; This only happens in a patch buffer.  In the working
          ;; buffers, we replace the differ header with the file name in
          ;; a larger font.
          (re-search-forward "^@@")
          (goto-char (match-beginning 0)))
         (t
          (put-text-property (point) (progn (forward-line 1) (point))
                             'face context-face)))))))


(defun gpb-git:hide-hunk (ov &optional move-pt)
  (with-current-buffer (overlay-buffer ov)
    (overlay-put ov 'invisible t)
    (gpb-git:update-visible-files)))


(defun gpb-git:show-hunk (ov &optional move-pt)
  (with-current-buffer (overlay-buffer ov)
    (overlay-put ov 'invisible nil)
    (gpb-git:update-visible-files)))


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


(defun gpb-git:get-visible-file-overlays ()
  "Find the list of visible file overlays."
  (cl-remove-if (lambda (ov) (overlay-get ov 'invisible))
                (mapcar 'cdr gpb-git:file-name-overlay-alist)))


(defun gpb-git:get-visible-files ()
  "Find the list of visible files in the current buffer."
  (mapcar (lambda (ov) (get-text-property (overlay-start ov) 'filename))
          (gpb-git:get-visible-file-overlays)))


(defun gpb-git:get-visible-hunks (&optional filename)
  "Find the list of visible hunk overlays.
If FILENAME if given, only return hunks that change FILENAME."
  (let ((hunks (if filename
                   (aget file-hunk-overlays-alist filename nil)
                 (apply 'append (mapcar 'cdr file-hunk-overlays-alist)))))
    (cl-remove-if (lambda (ov) (overlay-get ov 'invisible)) hunks)))


(defun gpb-git:update-visible-files (&optional filename)
  "Align the set of visible filenames with the visible hunks.
Checks the `invisible' property on each hunk overlay, and then
updates the `invisible' propertes on the filename overlays."
  (dolist (filename-hunks file-hunk-overlays-alist)
    (let* ((filename (car filename-hunks))
           (filename-ov (aget gpb-git:file-name-overlay-alist filename))
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
  (dolist (filename-overlay gpb-git:file-name-overlay-alist)
    (let ((filename (car filename-overlay))
          (ov (cdr filename-overlay)))
      (overlay-put ov 'invisible (null (gpb-git:get-visible-hunks filename))))))


(defun gpb-git:post-command-hook ()
  "Updates hunk highlighting after each user command."
  (gpb-git:update-highlights)
  (gpb-git:update-highlights gpb-git:other-buffer))


(defun gpb-git:update-highlights (&optional buf)
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
      (let* ((visible-files (gpb-git:get-visible-file-overlays))
             (visible-files-before-pt (cl-remove-if
                                       (lambda (ov)
                                         (> (overlay-start ov) (point)))
                                       visible-files))
             (file-header-ov (aget gpb-git:file-name-overlay-alist filename))
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
                 (length (gpb-git:get-visible-file-overlays))
                 (length (gpb-git:get-visible-hunks))))))))


(defun gpb-git:apply-changes-command (arg)
  "Apply the changes in the current buffer to the Git index.
With a prefix argument, we pop to a buffer containing the patch
and give the user an opportunity to edit the patch before
applying it."
  (interactive "P")
  (with-current-buffer gpb-git:staged-buffer
    (if arg
        (pop-to-buffer (gpb-git:create-patch-buffer))
      (let ((buf (gpb-git:create-patch-buffer)))
        (gpb-git:apply-patch buf)
        (kill-buffer)))))


(defun gpb-git:create-patch-buffer (&optional buf)
  "Create a patch from the hunks visible in BUF.
BUF should be the second (i.e., \"changes to commited\") buffer
create by `gpb-git:stage-hunks'."
  (let ((buf (or buf (current-buffer)))
        (patch-buf (get-buffer-create gpb-git:patch-buffer-name))
        filename text hunks hunk-diff)
    (with-current-buffer buf

      ;; Reset the patch buffer
      (let ((dir default-directory))
        (with-current-buffer patch-buf
          (setq default-directory dir)
          (erase-buffer)))

      ;; Write selected hunks to the patch buffer.
      (dolist (filename (gpb-git:get-visible-files))
        (setq file-header (or (aget gpb-git:diff-file-header-alist filename)
                              (error "Assertion error"))
              hunks (gpb-git:get-visible-hunks filename))
        (with-current-buffer patch-buf (insert file-header))
        (dolist (hunk hunks)
          (setq hunk-text (buffer-substring-no-properties
                           (overlay-start hunk) (overlay-end hunk)))
          (with-current-buffer patch-buf (insert hunk-text))))

      ;; Apply text styling to the patch buffer and add a keymap.  This
      ;; will only be used if the user decides they want to edit the patch
      ;; before we apply it.
      (with-current-buffer patch-buf
        (gpb-git:put-text-faces)
        (goto-char (point-min))
        ;; We need a character to carry the following display text
        ;; property.
        (insert "\n")
        (let ((msg (propertize
                      (concat "#\n#  Use C-c C-c to apply these changes "
                              "the Git index.\n#  Add a prefix argument "
                              "to edit the patch before applying it.\n#\n")
                      'face 'gpb-git:comment)))
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
    (with-current-buffer buf
      (write-region (point-min) (point-max) tempfile))
    (unless (= (process-file "git" nil nil nil "reset") 0)
      (error "`git reset` failed"))
    (with-current-buffer (get-buffer-create proc-output-buf)
      (erase-buffer))
    (setq retvalue (process-file "git" nil proc-output-buf t
                                 "apply" "--cached" tempfile))
    (if (= retvalue 0)
        ;; We successfully applied the patch.
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

where `line-number1' is the first line that the hunk applies to
(i.e., the first number between the @@s) and `status' is :staged
or :unstaged."
  (let* ((repo-root (or repo-root (gpb-git:find-repo-root)))
         (unstaged (gpb-git:parse-diff-output repo-root "diff" "HEAD"))
         (staged (gpb-git:parse-diff-output repo-root "diff" "--cached")))

    ;; Check that the file headers agree.
    (dolist (file-info staged)
      (let* ((filename (car file-info))
             (header1 (gpb-git:get unstaged filename :header))
             (header2 (gpb-git:get staged filename :header)))
        (assert (string= header1 header2))))

    ;; Check that the hunks agree.
    (dolist (filename (mapcar 'car staged))
      (let ((hunks (gpb-git:get staged filename :hunks)))
        (dolist (linenum (mapcar 'car hunks))
          (let* ((hunk1 (gpb-git:get unstaged filename :hunks linenum))
                 (hunk2 (gpb-git:get staged filename :hunks linenum)))
            (assert (stringp hunk1))
            (assert (stringp hunk2))
            (unless (gpb-git:hunks-agree-p hunk1 hunk2)
              (let ((buf (get-buffer-create "*Hunk conflict*")))
                (with-current-buffer buf
                  (erase-buffer)
                  (insert "Hunk conflict:\n\n")
                  (insert hunk1)
                  (insert "\n")
                  (insert hunk2)
                  (goto-char (point-min)))
                (pop-to-buffer buf)
                (user-error "Hunk conflict")))))))

    ;; Now check which hunks are staged.
    (dolist (filename (mapcar 'car unstaged))
      (let ((hunks (gpb-git:get unstaged filename :hunks)))
        (dolist (linenum (mapcar 'car hunks))
          (let ((hunk-text (aget hunks linenum))
                (staged (staged filename :hunks linenum)))
            (setcdr (assoc linenum hunks)
                    `((:text . ,hunk-text)
                      (:state . ,(if staged :staged :unstaged))))))))

    unstaged))


(defun gpb-git:parse-diff-output (repo-dir &rest args)
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


(defun gpb-get:describe-conflicts (&optional repo-root)
  "Describe the changes in the working copy that conflict with staged changes.

For the primary GUI that this package provieds to make sense, the
set hunks that describe the changes between HEAD and the index
must be a subset of the set of hunks that describe changes
between HEAD and the working copy.  This will not be the case if
we stage changes to a file and then edit that file introducing
new changes that overload the staged changes.

This function produces a buffer which identifies such conflicting
changes.

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

where `line-number1' is the first line that the hunk applies to
(i.e., the first number between the @@s) and `status' is :staged
or :unstaged."
  (let* ((repo-root (or repo-root (gpb-git:find-repo-root)))
         (unstaged (gpb-git:parse-diff-output repo-root "diff" "HEAD"))
         (staged (gpb-git:parse-diff-output repo-root "diff" "--cached")))

    ;; Check that the file headers agree.
    (dolist (file-info staged)
      (let* ((filename (car file-info))
             (header1 (gpb-git:get unstaged filename :header))
             (header2 (gpb-git:get staged filename :header)))
        (assert (string= header1 header2))))

    ;; Check that the hunks agree.
    (dolist (filename (mapcar 'car staged))
      (let ((hunks (gpb-git:get staged filename :hunks)))
        (dolist (linenum (mapcar 'car hunks))
          (let* ((hunk1 (gpb-git:get unstaged filename :hunks linenum))
                 (hunk2 (gpb-git:get staged filename :hunks linenum)))
            (assert (stringp hunk1))
            (assert (stringp hunk2))
            (unless (gpb-git:hunks-agree-p hunk1 hunk2)
              (let ((buf (get-buffer-create "*Hunk conflict*")))
                (with-current-buffer buf
                  (erase-buffer)
                  (insert "Hunk conflict:\n\n")
                  (insert hunk1)
                  (insert "\n")
                  (insert hunk2)
                  (goto-char (point-min)))
                (pop-to-buffer buf)
                (user-error "Hunk conflict")))))))

    ;; Now check which hunks are staged.
    (dolist (filename (mapcar 'car unstaged))
      (let ((hunks (gpb-git:get unstaged filename :hunks)))
        (dolist (linenum (mapcar 'car hunks))
          (let ((hunk-text (aget hunks linenum))
                (staged (staged filename :hunks linenum)))
            (setcdr (assoc linenum hunks)
                    `((:text . ,hunk-text)
                      (:state . ,(if staged :staged :unstaged))))))))

    unstaged))


(defun gpb-git:parse-diff-output (repo-dir &rest args)
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
         (filename-headers (list :diff-file-header-alist))
         (filename-overlays (list :file-name-overlay-alist))
         (hunk-overlays (list :file-hunk-overlays-alist))
         (inhibit-read-only t)
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
        (let ((file-header (gpb-git:get diff-info filename :header)))
          (nconc filename-headers `((,filename . ,file-header))))
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
          (nconc filename-overlays `((,filename . ,filename-overlay)))
          (overlay-put filename-overlay 'filename-p t)
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
      (setq-local gpb-git:diff-file-header-alist (cdr filename-headers))
      (setq-local gpb-git:file-name-overlay-alist (cdr filename-overlays))
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
          (insert-before-markers "\n")
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
                 (delete-region (point) (progn (end-of-line) (point)))
                 (buffer-substring (point-min) (point-max))))
        (text2 (with-temp-buffer
                 (insert text2)
                 (goto-char (point-min))
                 (re-search-forward "^@@ -[0-9]+,")
                 (delete-region (point) (progn (end-of-line) (point)))
                 (buffer-substring (point-min) (point-max)))))
    (string= text1 text2)))


(defun gpb-git:get (nested-alists &rest keys)
  "Reccursively look up KEYS in nested alists."
  (while keys
    (let ((key (car keys)))
      (setq nested-alists (or (cdr (assoc key nested-alists))
                              (error (format "Missing key: %s" key)))
            keys (cdr keys))))
  nested-alists)


(global-set-key "\C-cs" 'gpb-git:update-index)
