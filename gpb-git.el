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
;;    We call `git diff` to find the changes in the working directory
;;    relative to the index and `git diff --cached` to see the
;;    changes in the index relative to HEAD.
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

(defvar gpb-git:unstaged-buffer-name "*unstaged changes*"
  "The name of the buffer used to show staged changes.")

(defvar gpb-git:staged-buffer-name "*staged changes*"
  "The name of the buffer used to show unstaged changes.")

(defvar gpb-git:patch-buffer-name "*git patch*"
  "The name of the temporary buffer used to formulate patches.")

(defvar gpb-git:process-output-buffer-name "*git apply*"
  "The name of the buffer used to display Git output.
If the patch cannot be applied, this is the buffer that will be
used to show the errors to the user.")

(defvar gpb-git:currently-highlighted-hunk nil
  "Used to track the currently highlighted hunk.")


;;
;;  Local variables used in the various editing buffers.
;;

(defvar-local gpb-git:hunk-overlays nil
  "List of overlays that cover the hunks.")

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

(defvar gpb-git:unstaged-hunks-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'gpb-git:forward-hunk)
    (define-key map [(backtab)] 'gpb-git:backward-hunk)
    (define-key map "\r" 'gpb-git:stage-hunk)
    (define-key map "g" 'gpb-git:refresh-hunk-buffers)
    (fset 'gpb-git:unstaged-hunks-keymap map)
    map)
  "The keymap used for choosing hunks.")

(defvar gpb-git:staged-hunks-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'gpb-git:forward-hunk)
    (define-key map [(backtab)] 'gpb-git:backward-hunk)
    (define-key map "\r" 'gpb-git:unstage-hunk)
    (define-key map "g" 'gpb-git:refresh-hunk-buffers)
    (fset 'gpb-git:staged-hunks-keymap map)
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
  (let* ((dir (or repo-root default-directory))
         (unstaged-buf (gpb-git:refresh-unstaged-hunk-buffer dir))
         (staged-buf (gpb-git:refresh-staged-hunk-buffer dir)))

    ;; Show the buffers in two side-by-side windows in the current frame.
    (delete-other-windows)
    (set-window-buffer (selected-window) unstaged-buf)
    (let ((win2 (split-window-horizontally)))
      (set-window-buffer win2 staged-buf))))


(defun gpb-git:put-text-faces (&optional beg end highlighted)
  "Apply faces to diff output.
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
  (let ((pos (or pos (and (region-active-p) (region-beginning)) (point))))
    (cdr (get-char-property-and-overlay pos 'hunk-info))))


(defun gpb-git:get-current-hunk-info (&optional prop)
  "Return the 'hunk-info property of the hunk overlay at POS."
  (let* ((ov (gpb-git:get-current-hunk))
         (hunk-info (and ov (overlay-get ov 'hunk-info))))
    (if prop
        (gpb-git:get hunk-info prop)
      hunk-info)))


(defun gpb-git:forward-hunk ()
  "Move forward to the next hunk."
  (interactive)
  (when (eobp) (user-error "End of buffer"))
  (let ((pt (point)) (end (window-end)))

    (condition-case exc
        (progn (forward-line 1)
               (re-search-forward "^@@.*@@")
               (goto-char (match-beginning 0)))
      (search-failed
       (goto-char pt)
       (user-error "Last hunk")))

    ;; If we scrolled all the way out of the initial window, display the
    ;; hunk on the first or second line so we can see as much as possible.
    (when (and (> (point) end) (not (eobp)))
      (if (gpb-git:get-current-hunk-info :first-hunk)
          (recenter 1)
        (recenter 0)))

    (point)))


(defun gpb-git:backward-hunk ()
  "Move back to the start of the previous hunk."
  (interactive)
  (when (bobp) (user-error "Beginning of buffer"))
  (let ((start (window-start)))

    (condition-case exc
        (re-search-backward "^@@.*@@")
      (search-failed
       (user-error "First hunk")))

    ;; If we had to scroll the window to make the hunk visible and the hunk
    ;; is the first hunk for a file, scroll to include the filename in the
    ;; current window.
    (when (< (point) start)
      (if (gpb-git:get-current-hunk-info :first-hunk)
          (recenter 1)
        (recenter 0)))

    (point)))


(defun gpb-git:post-command-hook ()
  "Updates hunk highlighting after each user command."
  ;; If the mark will be deactivated before the next command, we want to
  ;; consider it to already be deactivated when we compute the highlights
  ;; to avoid flicker.
  (let ((mark-active (and mark-active (not deactivate-mark))))
    (gpb-git:update-highlights)))


(defun gpb-git:update-highlights (&optional buf)
  "Updates hunk highlighting."
  (with-current-buffer (window-buffer (selected-window))
    (let* ((prev-hunk (default-value 'gpb-git:currently-highlighted-hunk))
           (new-hunk  (gpb-git:get-current-hunk))
           ;; We don't want the changes below to deactivate the mark.
           deactivate-mark)

      (when (not (eq new-hunk prev-hunk))
        (when (and prev-hunk (overlay-buffer prev-hunk))
          (with-current-buffer (overlay-buffer prev-hunk)
            (gpb-git:put-text-faces (overlay-start prev-hunk)
                                    (overlay-end prev-hunk)
                                    nil)))
        (when (and new-hunk (overlay-buffer new-hunk))
          (with-current-buffer (overlay-buffer new-hunk)
            (gpb-git:put-text-faces (overlay-start new-hunk)
                                    (overlay-end new-hunk)
                                    t)))

        (set-default 'gpb-git:currently-highlighted-hunk new-hunk)))))


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
         (files (gpb-git:get-hunk-filenames))
         (hunk-overlays (gpb-git:get-hunk-overlays)))
    (cond
     (ov
      (let* ((current-file (gpb-git:get-current-hunk-info :filename1))
             (file-hunk-overlays (gpb-git:get-hunk-overlays current-file))
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
        (gpb-git:center-string
         (format "hunk %s/%s in %s  (file %s/%s, hunk %s/%s)"
                 (length file-hunks-before-pt)
                 (length file-hunk-overlays)
                 current-file
                 (length files-before-pt)
                 (length files)
                 (length hunks-before-pt)
                 (length hunk-overlays)))))
     (t (gpb-git:center-string
         (format "%s files, %s hunks"
                 (length files)
                 (length hunk-overlays)))))))


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



(defun gpb-git:diff (repo-dir &rest args)
  "Run a git diff command and parse the result.

Passes ARGS to the git command.  The first element of ARGS should
probably be \"diff\".  Returns a nested structure of the form:

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

          (sort (cdr diff-info)
                (lambda (x y) (string< (car x) (car y)))))))))


(defun gpb-git:compute-diff (&rest args)
  "Run a git diff command and parse the result.

Passes any additional ARGS to the `git diff` command.  Returns a
list of hunks, where each hunk is an alist with the keys
:filename1, :filename2, :file1-start, :file1-len, :file2-start,
:file2-len, and :diff."
  (let ((dir default-directory)
        (hunk-list (list :stub))
        filename1 filename2)

    (with-temp-buffer
      (setq default-directory dir)
      (unless (= (apply 'process-file "git" nil t nil "diff" args) 0)
        (error "`git diff` failed"))

      (save-excursion
        (save-match-data
          (goto-char (point-min))
          (re-search-forward "^diff --git" nil t)
          (goto-char (match-beginning 0))
          (while (< (point) (point-max))
            (cond
             ((looking-at "^diff --git a/\\(.*\\) b/\\(.*\\)")
              (setq filename1 (substring-no-properties
                               (or (match-string 1) (error "Assertion error")))
                    filename2 (substring-no-properties
                               (or (match-string 2) (error "Assertion error"))))
              (re-search-forward "^@@")
              (goto-char (match-beginning 0)))
             ((looking-at (concat "^@@ -\\([0-9]+\\),\\([0-9]+\\) +"
                                  "\\+\\([0-9]+\\),\\([0-9]+\\) @@"))
              (let ((file1-start (string-to-number (match-string 1)))
                    (file1-len (string-to-number (match-string 2)))
                    (file2-start (string-to-number (match-string 3)))
                    (file2-len (string-to-number (match-string 4)))
                    (diff (buffer-substring-no-properties
                           (progn (forward-line 1) (point))
                           (progn
                             (or (and
                                  (re-search-forward "^@@\\|^diff --git " nil t)
                                  (goto-char (match-beginning 0)))
                                 (goto-char (point-max)))
                             (point)))))
                (nconc hunk-list `(((:filename1 . ,filename1)
                                    (:file1-start . ,file1-start)
                                    (:file1-len . ,file1-len)
                                    (:filename2 . ,filename2)
                                    (:file2-start . ,file2-start)
                                    (:file2-len . ,file2-len)
                                    (:diff . ,diff))))))
             (t (error "Assertion error")))))))

    (sort (cdr hunk-list)
          (lambda (x y)
            (or (string< (aget x :filename1) (aget y :filename1))
                (and (string= (aget x :filename1) (aget y :filename1))
                     (< (aget x :file1-start) (aget y :file1-start))))))))


(defun gpb-git:refresh-unstaged-hunk-buffer (&optional repo-dir)
  "Create or refresh the buffer that displays unstaged changes."
  (let* ((repo-dir (or repo-dir default-directory))
         (buf (get-buffer-create gpb-git:unstaged-buffer-name))
         (inhibit-read-only t)
         (msg (propertize (concat "#\n#  Hit enter on a hunk to add it "
                                  "to the index.\n#\n")
                          'face 'gpb-git:comment)))

    (with-current-buffer buf
      (setq default-directory repo-dir)
      (gpb-git:refresh-hunks)
      (use-local-map gpb-git:unstaged-hunks-keymap)

      ;; Throw some instructions in the top of the buffer.
      (save-excursion
        (goto-char (point-min))
        (insert "\n")
        (set-text-properties 1 2 `(display ,msg))))

    buf))


(defun gpb-git:refresh-staged-hunk-buffer (&optional repo-dir hunk-info)
  "Create or refresh the buffer that displays staged changes."
  (let* ((repo-dir (or repo-dir default-directory))
         (buf (get-buffer-create gpb-git:staged-buffer-name))
         (inhibit-read-only t)
         (msg (propertize (concat "#\n#  Hit enter on a hunk to remove it "
                                  "from the index.\n#\n")
                          'face 'gpb-git:comment)))

    (with-current-buffer buf
      (setq default-directory repo-dir)
      (gpb-git:refresh-hunks "--cached")
      (use-local-map gpb-git:staged-hunks-keymap)

      ;; Throw some instructions in the top of the buffer.
      (save-excursion
        (goto-char (point-min))
        (insert "\n")
        (set-text-properties 1 2 `(display ,msg))))

    buf))


(defun gpb-git:refresh-hunks (&rest args)
  "Write the hunks in DIFF-INFO to the current buffer.

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
         (diff-hunks (apply 'gpb-git:compute-diff args))
         (hunk-overlays (list :stub))
         (inhibit-read-only t)
         ;; We attempt preserve the current location.
         (current-hunk-info
          (or (gpb-git:get-current-hunk-info)
              (and (ignore-errors (gpb-git:forward-hunk))
                   (gpb-git:get-current-hunk-info))
              (and (ignore-errors (gpb-git:backward-hunk))
                   (let* ((hunk-info2 (gpb-git:get-current-hunk-info))
                          (filename1 (gpb-git:get hunk-info2 :filename1))
                          (file1-start (gpb-git:get hunk-info2 :file1-start)))
                     `((:filename1 . ,filename1)
                       (:file1-start . ,(1+ file1-start)))))))
         current-file ov)

    (kill-all-local-variables)
    (erase-buffer)
    (gpb-git:delete-hunk-overlays)
    (setq-local header-line-format '(:eval (gpb-git:compute-header)))
    (setq-local default-directory dir)

    ;; (add-hook 'kill-buffer-hook 'gpb-git:kill-buffer-hook nil t)
    (add-hook 'post-command-hook 'gpb-git:post-command-hook)

    ;; (use-local-map gpb-git:selected-hunks-keymap)

    (dolist (diff-hunk diff-hunks)
      (let* ((filename1 (gpb-git:get diff-hunk :filename1))
            (filename2 (gpb-git:get diff-hunk :filename2))
            (first-hunk (not (equal current-file `(,filename1 . ,filename2)))))
        (setq current-file `(,filename1 . ,filename2))
        (insert "\n")
        (when first-hunk
          (if (string= filename1 filename2)
              (put-text-property (point) (progn (insert filename1)
                                                (insert "\n")
                                                (point))
                                 'face 'gpb-git:file-name)
            (put-text-property (point) (progn (insert filename1)
                                              (insert " -> ")
                                              (insert filename2)
                                              (insert "\n")
                                              (point))
                               'face 'gpb-git:file-name)))

        (setq ov (make-overlay (point)
                               (progn
                                 (insert (gpb-git:get-hunk-header diff-hunk))
                                 (insert (gpb-git:get diff-hunk :diff))
                                 (point))))
        (overlay-put ov 'hunk-info diff-hunk)
        (gpb-git:put-text-faces (overlay-start ov) (overlay-end ov))
        (nconc hunk-overlays (list ov))))

    (goto-char (point-min))
    (setq-local buffer-read-only t)
    (setq-local gpb-git:hunk-overlays (cdr hunk-overlays))
    (when current-hunk-info
      (gpb-git:jump-to-hunk (gpb-git:get current-hunk-info :filename1)
                            (gpb-git:get current-hunk-info :file1-start)))))


(defun gpb-git:get (nested-alists &rest keys)
  "Recursively look up KEYS in nested alists.
If NESTED-ALISTS is an overlay, we use its 'hunk-info property."
  (when (overlayp nested-alists)
    (setq nested-alists (overlay-get nested-alists 'hunk-info)))
  (while keys
    (let ((key (car keys)))
      (setq nested-alists (cdr (or (assoc key nested-alists)
                                   (error (format "Missing key: %s" key))))
            keys (cdr keys))))
  nested-alists)


(defun gpb-git:stage-hunk (&optional unstage)
  "Apply the hunk at the point to the Git index.
If UNSTAGE is non-nil, we apply the hunk in reverse."
  (interactive)
  (let* ((hunk-info (or (gpb-git:get-current-hunk-info)
                        (user-error "No hunk at point")))
         (tempfile (make-nearby-temp-file "git-" nil ".patch"))
         (proc-output-buf gpb-git:process-output-buffer-name)
         (args (if unstage `("apply" "--cached" "-R" ,tempfile)
                 `("apply" "--cached" ,tempfile))))

    (with-current-buffer (gpb-git:get-patch unstage)
      (write-region (point-min) (point-max) tempfile))

    (with-current-buffer (get-buffer-create proc-output-buf)
      (erase-buffer)
      (insert (format "git %s\n" (mapconcat 'identity args " ")))
      (save-excursion
        (insert "\n\nPatch contents:\n\n")
        (insert-file-contents tempfile)))

    (setq retvalue (apply 'process-file "git" nil proc-output-buf t args))

    (if (= retvalue 0)
        ;; We successfully applied the patch.
        (progn
          (kill-buffer proc-output-buf)
          (delete-file tempfile)
          (gpb-git:refresh-unstaged-hunk-buffer)
          (gpb-git:refresh-staged-hunk-buffer))
      ;; If the application failed, we pop to the process output.
      (pop-to-buffer proc-output-buf))))


(defun gpb-git:unstage-hunk ()
  "Toggle the selection of the hunk at the point."
  (interactive)
  (gpb-git:stage-hunk t))


(defun gpb-git:refresh-hunk-buffers ()
  "Refresh the buffers displaying staged and unstaged changes."
  (interactive)
  (gpb-git:refresh-unstaged-hunk-buffer)
  (gpb-git:refresh-staged-hunk-buffer))


;; (defun gpb-git:find-hunk (filename line-number)
;;   "Find the overlay for the hunk in FILENAME at LINE-NUMBER."
;;   (catch 'found-overlay
;;     (dolist (ov (overlays-in (point-min) (point-max)))
;;       (let ((hunk-info (overlay-get ov 'hunk-info)))
;;         (when (and hunk-info
;;                    (string= (gpb-git:get hunk-info :filename) filename)
;;                    (= (gpb-git:get hunk-info :line-number) line-number))
;;           (throw 'found-overlay ov))))
;;     (error "Cannot find overlay: %s line %s" filename line-number)))


;; (defun gpb-git:goto-hunk (filename line-number)
;;   "Move to the hunk starting at line LINE-NUMBER in FILENAME."
;;   (let ((ov (gpb-git:find-hunk filename line-number)))
;;     (goto-char (overlay-start ov))))


(defun gpb-git:jump-to-hunk (filename line-number)
  "Move to the hunk starting at line LINE-NUMBER in FILENAME.
If there is not a hunk that starts at line LINE-NUMBER in
FILENAME, we move to the end of the last hunk starting before
LINE-NUMBER in FILENAME, where the set of hunks are ordered by
filename and then line number.  If there are no hunks before
LINE-NUMBER in FILENAME, we move to the start of the first hunk
in the buffer."
  (goto-char (point-min))
  (ignore-errors (gpb-git:forward-hunk))
  (dolist (ov (overlays-in (point-min) (point-max)))
    (let* ((hunk-info (overlay-get ov 'hunk-info))
           (hunk-filename (and hunk-info
                               (gpb-git:get hunk-info :filename1)))
           (hunk-line-number (and hunk-info
                                  (gpb-git:get hunk-info :file1-start))))
      (when (and hunk-info
                 (>= (overlay-start ov) (point))
                 (or (string< hunk-filename filename)
                     (and (string= hunk-filename filename)
                          (<= hunk-line-number line-number))))
        (if (and (string= hunk-filename filename)
                 (= hunk-line-number line-number))
            (goto-char (overlay-start ov))
          (goto-char (overlay-end ov)))))))


(defun gpb-git:get-hunk-overlays (&optional filename)
  "Get a list of hunk overlays.

If an optional FILENAME is given, only return the hunks that
modify the given file."
  (let ((hunks (cl-remove-if-not (lambda (ov) (overlay-get ov 'hunk-info))
                                 (overlays-in (point-min) (point-max)))))
    (if filename
        ;; If a filename is given, further filter the hunks.
        (cl-remove-if-not (lambda (ov) (string= (gpb-git:get ov :filename1)
                                                filename))
                          hunks)
      hunks)))


(defun gpb-git:delete-hunk-overlays ()
  "Delete all hunk overlays in the current buffer."
  (dolist (ov (gpb-git:get-hunk-overlays)) (delete-overlay ov)))


(defun gpb-git:get-hunk-filenames ()
  "Get a list of hunk filenames."
  (sort (delete-dups
         (mapcar (lambda (ov) (gpb-git:get (overlay-get ov 'hunk-info)
                                           :filename1))
                 (gpb-git:get-hunk-overlays)))
        'string<))

(defun gpb-git:get-hunk-header (hunk-info)
  (format "@@ -%s,%s +%s,%s @@\n"
          (gpb-git:get hunk-info :file1-start)
          (gpb-git:get hunk-info :file1-len)
          (gpb-git:get hunk-info :file2-start)
          (gpb-git:get hunk-info :file2-len)))


(defun gpb-git:get-patch (&optional reverse)
  "Construct a patch corresponding to the currrent hunk.
This function should be called from a buffer that is displaying
hunks.  If the region is active, we construct a patch which
corresponds to the portion of the current hunk that is selected.
If the region is not active, we construct a patch that
corresponds to the entire hunk.  If REVERSE is non-nil, we
construct the patch so that it can be applied in reverse.
Returns a buffer with the name `gpb-git:patch-buffer-name' that
contains the patch."
  (let* ((hunk (or (gpb-git:get-current-hunk)
                   (user-error "No hunk at point")))
         (filename1 (gpb-git:get hunk :filename1))
         (file1-start (gpb-git:get hunk :file1-start))
         (file1-len (gpb-git:get hunk :file1-len))
         (filename2 (gpb-git:get hunk :filename2))
         (file2-start (gpb-git:get hunk :file2-start))
         (file2-len (gpb-git:get hunk :file2-len))
         (diff-beg (save-excursion
                     (goto-char (overlay-start hunk))
                     (forward-line 1)
                     (point)))
         (diff-end (overlay-end hunk))
         (beg (or (and (region-active-p)
                       (save-excursion
                         (goto-char (region-beginning))
                         (beginning-of-line)
                         (point)))
                  diff-beg))
         (end (or (and (region-active-p) (region-end)) (overlay-end hunk)))
         (patch-buf (get-buffer-create gpb-git:patch-buffer-name))
         (source-lines 0)
         (target-lines 0)
         diff-line)

    (when (or (< beg diff-beg) (> end diff-end))
      (user-error "Selection cannot extend outside of hunk"))

    (with-current-buffer patch-buf
      (erase-buffer))

    (save-excursion
      (goto-char diff-beg)

      ;; Changes before beg are not applied.
      (while (< (point) beg)
        (cond
         ;; Keep context lines
         ((looking-at-p "^ ")
          (setq diff-line (buffer-substring-no-properties
                           (point) (progn (forward-line 1) (point))))
          (with-current-buffer patch-buf (insert diff-line))
          (incf source-lines)
          (incf target-lines))

         ;; In the forward direction, removed lines become context lines.
         ((and (not reverse) (looking-at-p "^-"))
          (setq diff-line (buffer-substring-no-properties
                             (point) (progn (forward-line 1) (point))))
          (aset diff-line 0 ?\ )
          (with-current-buffer patch-buf (insert diff-line))
          (incf source-lines)
          (incf target-lines))
         ;; In the reverse direction, removed lines are discarded.
         ((and reverse (looking-at-p "^-"))
          (forward-line 1))

         ;; In the forward direction, added lines are ignored.
         ((and (not reverse) (looking-at-p "^+"))
          (forward-line 1))
         ;; In the reverse direction, added lines become context lines.
         ((and reverse (looking-at-p "^+"))
          (setq diff-line (buffer-substring-no-properties
                             (point) (progn (forward-line 1) (point))))
          (aset diff-line 0 ?\ )
          (with-current-buffer patch-buf (insert diff-line))
          (incf source-lines)
          (incf target-lines))

         (t (error "Assertion error"))))

      ;; Changes between beg and end are applied.
      (while (< (point) end)
        ;; Context lines and removed lines appear in the source file.
        (when (looking-at-p "^[ -]") (incf source-lines))
        ;; Context lines and new lines appear in the target file.
        (when (looking-at-p "^[ +]") (incf target-lines))
        (setq diff-line (buffer-substring-no-properties
                         (point)
                         (progn (forward-line 1) (point))))
        (with-current-buffer patch-buf (insert diff-line)))

      ;; The changes after end are not applied.
      (while (< (point) diff-end)
        (cond
         ;; Keep context lines
         ((looking-at-p "^ ")
          (setq diff-line (buffer-substring-no-properties
                           (point) (progn (forward-line 1) (point))))
          (with-current-buffer patch-buf (insert diff-line))
          (incf source-lines)
          (incf target-lines))

         ;; In the forward direction, removed lines become context lines.
         ((and (not reverse) (looking-at-p "^-"))
          (setq diff-line (buffer-substring-no-properties
                             (point) (progn (forward-line 1) (point))))
          (aset diff-line 0 ?\ )
          (with-current-buffer patch-buf (insert diff-line))
          (incf source-lines)
          (incf target-lines))
         ;; In the reverse direction, removed lines are discarded.
         ((and reverse (looking-at-p "^-"))
          (forward-line 1))

         ;; In the forward direction, added lines are ignored.
         ((and (not reverse) (looking-at-p "^+"))
          (forward-line 1))
         ;; In the reverse direction, added lines become context lines.
         ((and reverse (looking-at-p "^+"))
          (setq diff-line (buffer-substring-no-properties
                             (point) (progn (forward-line 1) (point))))
          (aset diff-line 0 ?\ )
          (with-current-buffer patch-buf (insert diff-line))
          (incf source-lines)
          (incf target-lines))

         (t (error "Assertion error"))))

      (when (or (not reverse) (and (= beg diff-beg) (- end diff-end)))
        (assert (= file1-len source-lines)))
      (when (or reverse (and (= beg diff-beg) (- end diff-end)))
        (assert (= file2-len target-lines))))

    (with-current-buffer patch-buf
      (goto-char (point-min))
      (save-excursion
        (insert (format "diff --git a/%s b/%s\n" filename1 filename2))
        (insert (format "--- a/%s\n" filename1))
        (insert (format "+++ b/%s\n" filename2))
        (insert (format "@@ -%s,%s +%s,%s @@\n"
                        file1-start source-lines
                        file2-start target-lines))))

    patch-buf))


(global-set-key "\C-cs" 'gpb-git:update-index)
