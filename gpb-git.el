;;
;;  Tools for staging and unstaging hunks to the Git index
;;
;;  This package provides the command `gpb-git:stage-changes' which opens
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
;;
;;  Hunk properties:
;;
;;    :marked This property is non-nil if the entire hunk is marked.
;;      Typically t when the hunk is marked, but may be set to a cons cell
;;      of the form (:rename . filename) when the hunk is a file deletion
;;      that has been marked as a rename (see `gpb-git:mark-as-rename').
;;    :marked-lines A vector of bools or nil.  If a vector, there is one
;;      component for each line in the hunk and at least one entry is t.
;;
;;  The functions `gpb-git:get', `gpb-git:put', and `gpb-git:del' should be
;;  used to modify hunk properties.
;;

(defvar gpb-git:unstaged-buffer-name "*unstaged changes*"
  "The name of the buffer used to show staged changes.")

(defvar gpb-git:staged-buffer-name "*staged changes*"
  "The name of the buffer used to show unstaged changes.")

(defvar gpb-git:patch-buffer-name "*git patch*"
  "The name of the temporary buffer used to formulate patches.")

(defvar gpb-git:process-output-buffer-name "*git output*"
  "The name of the buffer used to display Git output.
If the patch cannot be applied, this is the buffer that will be
used to show the errors to the user.")

(defvar gpb-git:currently-focused-hunk nil
  "Used to track the currently focused hunk.")

(defvar gpb-git:currently-flashed-hunk nil
  "Used to track the currently flashed hunk.")

(defvar gpb-git:saved-window-configuration nil)


(defvar gpb-git:commit-message-buffer-name "*commit message*"
  "The name of the temporary buffer used to edit commit messages.")

(defvar gpb-git:commit-buffer-name "*git commit*"
  "The name of the temporary buffer used to edit commit messages.")

(defvar gpb-git:commit-messages nil
  "We save all commit messages so they can be recovered.")

;;
;;  Local variables used in the various editing buffers.
;;

(defvar-local gpb-git:hunk-overlays nil
  "List of overlays that cover the hunks.")

;;
;;  Faces
;;

(defun gpb-git:blend-colors (c1 c2 &optional alpha1 alpha2)
  "Blend the two colors C1 and C2 with ALPHA."
  (let ((alpha1 (or alpha1 0.5))
        (alpha2 (or alpha2 (- 1 alpha1))))
    (apply 'format "#%02x%02x%02x"
           (cl-mapcar (lambda (x y) (* 256 (+ (* alpha1 x) (* alpha2 y))))
                      (color-name-to-rgb c1)
                      (color-name-to-rgb c2)))))

(defface gpb-git:title
  '((t :background "#eeeeee" :inherit default))
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
  '((t :background "gray95"))
  "Face used for context lines in a hunk")

(defface gpb-git:deleted-line
  `((t :foreground "#550000"
       :background ,(gpb-git:blend-colors "#f0c0c0" "white" 0.6)))
  "Face used for the deleted lines in a hunk")

(defface gpb-git:added-line
  `((t :foreground "#004400"
       :background ,(gpb-git:blend-colors "#b8e0b8" "white" 0.6)))
  "Face used for the added lines in a hunk")

;; Focused faces

(defface gpb-git:focused-hunk-header
  '((t :background "#9999ff"))
  "Face used for context lines in the focused hunk")

(defface gpb-git:focused-context-line
  '((t :background "#e0e0ff"))
  "Face used for context lines in the focused hunk")

(defface gpb-git:focused-added-line
  '((t :foreground "#005500"
       :background "#b8e0b8"))
  "Face used for the added lines in the focused hunk.")

(defface gpb-git:focused-deleted-line
  '((t :foreground "#880000"
       :background "#f0c0c0"))
  "Face used for deleted lines in the focused hunk.")

;; Marked faces

(defface gpb-git:marked-hunk-header
  `((t :background ,(gpb-git:blend-colors "khaki4" "white" 0.6)))
  "Face used for context lines in a marked hunk")

(defface gpb-git:marked-context-line
  `((t :background ,(gpb-git:blend-colors "khaki2" "white" 0.6)))
  "Face used for context lines in the marked hunk")

(defface gpb-git:marked-added-line
  `((t :background ,(gpb-git:blend-colors "khaki2" "white" 0.8)))
  "Face used for the added lines in the marked hunk.")

(defface gpb-git:marked-deleted-line
  `((t :background ,(gpb-git:blend-colors "khaki3" "white" 0.6)))
  "Face used for deleted lines in the marked hunk.")

;; Focused and marked faces

(defface gpb-git:focused-and-marked-hunk-header
  '((t :background "khaki4"))
  "Face used for context lines in a marked hunk")

(defface gpb-git:focused-and-marked-context-line
  `((t :background ,(gpb-git:blend-colors "khaki2" "white" 0.72 0.22)))
  "Face used for context lines in the marked hunk")

(defface gpb-git:focused-and-marked-added-line
  `((t ;; :foreground "#003000"
       :background ,(gpb-git:blend-colors "khaki2" "black" 0.95)))
  "Face used for the added lines in the marked hunk.")

(defface gpb-git:focused-and-marked-deleted-line
  `((t ;; :foreground "#660000"
       :background ,(gpb-git:blend-colors "#f0c0c0" "khaki3" 0)))
  "Face used for deleted lines in the marked hunk.")

;;
;;  Keymaps
;;

(defvar gpb-git:unstaged-changes-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'gpb-git:forward-hunk-command)
    (define-key map [(backtab)] 'gpb-git:backward-hunk-command)
    (define-key map "x" 'gpb-git:stage-marked-hunks)
    (define-key map "g" 'gpb-git:refresh-hunk-buffers)
    (define-key map "m" 'gpb-git:mark-hunk-command)
    (define-key map "u" 'gpb-git:unmark-hunk-command)
    (define-key map "r" 'gpb-git:mark-as-rename)
    (define-key map "\C-c\C-c" 'gpb-git:complete-commit)
    (fset 'gpb-git:unstaged-changes-mode-map map)
    map)
  "The keymap used for choosing hunks.")


(define-derived-mode gpb-git:unstaged-changes-mode special-mode
  "Unstaged Changes"
  "\nMode for selecting unstages changes to be added to the index.

\\{gpb-git:unstaged-changes-mode-map}\n")


(defvar gpb-git:staged-changes-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'gpb-git:forward-hunk-command)
    (define-key map [(backtab)] 'gpb-git:backward-hunk-command)
    (define-key map "x" 'gpb-git:unstage-marked-hunks)
    (define-key map "g" 'gpb-git:refresh-hunk-buffers)
    (define-key map "m" 'gpb-git:mark-hunk-command)
    (define-key map "u" 'gpb-git:unmark-hunk-command)
    (define-key map "\C-c\C-c" 'gpb-git:commit)
    (fset 'gpb-git:staged-changes-mode-map map)
    map)
  "The keymap used for removing and staging hunks.")


(define-derived-mode gpb-git:staged-changes-mode special-mode
  "Staged Changes"
  "\nMode for selecting stages changes to be removed from the index.

\\{gpb-git:staged-changes-mode-map}\n")


;;
;;  Functions
;;

(defun gpb-git:stage-changes (&optional repo-root)
  "Show the user two buffers for staging and unstaging hunks selection."
  (interactive)
  (let* ((dir (or repo-root (gpb-git:find-repo-root)))
         (unstaged-buf (gpb-git:refresh-unstaged-hunk-buffer dir))
         (staged-buf (gpb-git:refresh-staged-hunk-buffer dir)))

    ;; Show the buffers in two side-by-side windows in the current frame.
    (setq gpb-git:saved-window-configuration (current-window-configuration))
    (delete-other-windows)
    (set-window-buffer (selected-window) unstaged-buf)
    (let ((win2 (split-window-horizontally)))
      (set-window-buffer win2 staged-buf))))


(defun gpb-git:decorate-hunk (hunk &optional focused)
  "Apply faces to hunk text.
HUNK is an overlay, or a cons cell of the form (beg . end).  If
FOCUSED is non-nil, we used alternative faces."
  ;; FIXME: Handle the (beg . end) case again.
  (assert (and (overlayp hunk) (overlay-buffer hunk)))
  (with-current-buffer (overlay-buffer hunk)
    (let* ((beg (or (and (overlayp hunk) (overlay-start hunk)) (car hunk)))
           (end (copy-marker (or (and (overlayp hunk) (overlay-end hunk))
                                 (cdr hunk))))
           (marked (gpb-git:get hunk :marked))
           (marked-lines (gpb-git--get-marked-lines hunk))
           (inhibit-read-only t) (i 0) line-marked)

      (save-excursion
        (goto-char beg)
        (put-text-property (point) (progn (forward-line 1) (point))
                           'face (cond
                                  ((and marked focused)
                                   'gpb-git:focused-and-marked-hunk-header)
                                  (marked 'gpb-git:marked-hunk-header)
                                  (focused 'gpb-git:focused-hunk-header)
                                  (t 'gpb-git:hunk-header)))
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


(defun gpb-git:find-repo-root ()
  "Find the root of the Git repository.
Looks for the .git directory rather than calling Git."
  (let ((dir default-directory))
    (while (and dir (not (file-exists-p (concat dir ".git"))))
      (setq dir (file-name-directory
                 (directory-file-name dir))))
    dir))


(defun gpb-git:forward-hunk ()
  "Move point forward to the next hunk."
  (interactive)
  (when (eobp) (user-error "End of buffer"))
  (let ((pt (point))
        (end (window-end))
        (hunk (gpb-git:get-current-hunk)))
    ;; Move the end of the current hunk.
    (when hunk (goto-char (overlay-end hunk)))
    ;; Now move forward until you are inside a hunk.  This currently
    ;; doesn't happen, as the hunks are contiguous.
    (while (not (or (eobp) (gpb-git:get-current-hunk))) (forward-line 1))
    ;; If we moved to the end of the buffer, there is not next hunk.
    (when (eobp) (goto-char pt) (user-error "Last hunk")))
    (point))


(defun gpb-git:forward-hunk-command ()
  "Move forward to the next hunk and scroll window."
  (interactive)
  (gpb-git:forward-hunk)
  (let ((ov (gpb-git:get-current-hunk))
        (win (selected-window)))
    (when (and ov (overlay-buffer ov) win)
      (assert (eq (window-buffer win) (current-buffer)))
      ;; Ensure that the full hunk is visible when possible.
      (save-excursion
        (save-match-data
          (goto-char (overlay-end ov))
          (when (looking-back "\n") (backward-char))
          (set-window-point win (point))
          (redisplay t)))
      ;; Ensure that file name is visible when possible.
      (when (gpb-git:get ov :first-file-hunk)
        (save-excursion
          (forward-line -1)
          (set-window-point win (point))
          (redisplay t)))))

  (point))


(defun gpb-git:backward-hunk ()
  "Move back to the start of the current hunk."
  (interactive)
  (when (bobp) (user-error "Beginning of buffer"))
  (let ((pt (point))
        (hunk (gpb-git:get-current-hunk)))
    (while (and (not hunk) (not (bobp)))
      ;; This happens when the point is at the end of the buffer.
      (forward-line -1)
      (setq hunk (gpb-git:get-current-hunk)))
    (assert hunk)
    (if (< (overlay-start hunk) pt)
        ;; We are strictly inside the hunk, so we move back the start.
        (goto-char (overlay-start hunk))
      ;; We are at the start of the hunk.
      (forward-line -1)
      (gpb-git:backward-hunk)))
  (point))


(defun gpb-git:backward-hunk-command ()
  "Move back to the start of the previous hunk and scroll window."
  (interactive)
  (gpb-git:backward-hunk)
  ;; If we had to scroll the window to make the hunk visible and the hunk
  ;; is the first hunk for a file, scroll to include the filename in the
  ;; current window.
  (let ((win (selected-window)))
    (assert (eq (window-buffer win) (current-buffer)))
    ;; Ensure that file name is visible when possible.
    (when (gpb-git:get-current-hunk-info :first-file-hunk)
      (save-excursion
        (forward-line -1)
        (set-window-point win (point))
        (redisplay t)))))


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
    (let* ((prev-hunk (default-value 'gpb-git:currently-focused-hunk))
           (new-hunk  (gpb-git:get-current-hunk))
           ;; We don't want the changes below to deactivate the mark.
           deactivate-mark)
      (when (not (eq new-hunk prev-hunk))

        (when (and prev-hunk (overlay-buffer prev-hunk))
          (gpb-git:decorate-hunk prev-hunk))

        (when (and new-hunk (overlay-buffer new-hunk))
          (gpb-git:decorate-hunk new-hunk t))

        (set-default 'gpb-git:currently-focused-hunk new-hunk)))))


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


(defun gpb-git:kill-buffer-hook ()
  "Kill both linked buffers when one buffer is killed."
  (with-current-buffer gpb-git:staged-buffer-name
    (remove-hook 'kill-buffer-hook 'gpb-git:kill-buffer-hook t))
  (with-current-buffer gpb-git:unstaged-buffer-name
    (remove-hook 'kill-buffer-hook 'gpb-git:kill-buffer-hook t))
  (kill-buffer gpb-git:staged-buffer-name)
  (kill-buffer gpb-git:unstaged-buffer-name)
  (remove-hook 'post-command-hook 'gpb-git:post-command-hook)
  (when gpb-git:saved-window-configuration
    (set-window-configuration gpb-git:saved-window-configuration)
    (setq gpb-git:saved-window-configuration nil)))


(defun gpb-git:compute-diff (&optional staged dir)
  "Compute differences between working dir and index or index and HEAD.

Returns a list of hunks, where each hunk is an alist with the
keys :filename1, :filename2, :file1-start, :file1-len,
:file2-start, :file2-len, :first-file-hunk, :insertion, :deletion,
:rename and :diff."
  (let* ((dir (or dir default-directory))
         (hunk-list (list :stub))
         (args (if staged
                   '("diff-index" "--cached" "--find-renames" "--patch" "HEAD")
                 '("diff-files" "--patch" "--find-renames")))
         (cmd-string (mapconcat 'identity (cons "git" args) " "))
         (buf-name (concat "*" cmd-string "*"))
         ;; Some hunks consist of a file header have no trailing diff
         ;; sections (e.g., renames with 100% similarity and empty files
         ;; staged with --intent-to-add).  We set add-diffless-hunk to t
         ;; when we see a file header, and set it back to nil when we see
         ;; the first diff section.  If we see a file header and
         ;; add-diffless-hunk is t, we know that the previous file header
         ;; had no diff sections.
         (add-diffless-hunk nil)
         beg end insertion deletion filename1 filename2 header binary-info)

    (with-current-buffer (get-buffer-create buf-name)
      (erase-buffer)
      (setq default-directory dir)

      (setq beg (point))
      (unless (<= (apply 'process-file "git" nil t nil args) 1)
        (error (buffer-substring beg (point))))

      ;; Include unstaged files when computing the diff between working
      ;; directory and the index.
      (unless staged
        (dolist (filename (gpb-git:list-unstaged-files))
          (setq beg (point))
          (unless (<= (process-file "git" nil t nil "diff" "--no-index"
                                    "--" "/dev/null" filename)
                      1)
            (error (buffer-substring beg (point))))))

      (save-excursion
        (save-match-data
          (goto-char (point-min))
          (re-search-forward "^diff --git" nil t)
          (goto-char (match-beginning 0))
          (while (< (point) (point-max))
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
                               (re-search-forward "^\\(@@\\|diff --git\\)" nil t)
                               (goto-char (match-beginning 0)))
                              (end-of-buffer))
                          (point))
                    insertion (save-excursion
                                (or (re-search-backward "^--- /dev/null" beg t)
                                    (re-search-backward "^new file" beg t)))
                    deletion (save-excursion
                               (re-search-backward "^+++ /dev/null" beg t))
                    header (buffer-substring-no-properties beg end)
                    binary-info (save-excursion
                                  (when (re-search-backward "^Binary files" beg t)
                                    (buffer-substring-no-properties
                                     (point) (progn (end-of-line) (point)))))
                    add-diffless-hunk t))

             ((looking-at "^@@ -\\([0-9,]+\\) \\+\\([0-9,]+\\) @@")
              (let* ((range1 (save-match-data (split-string (match-string 1) ",")))
                     (range2 (save-match-data (split-string (match-string 2) ",")))
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

             (t (error "Assertion error")))))))

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

    (setq hunk-list (sort (cdr hunk-list)
                          (lambda (x y)
                            (or (string< (aget x :filename1)
                                         (aget y :filename1))
                                (and (string= (aget x :filename1)
                                              (aget y :filename1))
                                     (< (aget x :file1-start)
                                        (aget y :file1-start)))))))

    (let (current-file)
      (dolist (hunk hunk-list)
        (let* ((filename1 (gpb-git:get hunk :filename1))
               (filename2 (gpb-git:get hunk :filename2))
               (first-hunk (not (equal current-file
                                       `(,filename1 . ,filename2)))))
          (setq current-file `(,filename1 . ,filename2))
          (nconc hunk `((:first-file-hunk . ,first-hunk))))))

    hunk-list))


(defun gpb-git:refresh-unstaged-hunk-buffer (repo-dir)
  "Create or refresh the buffer that displays unstaged changes."
  (let* ((buf (get-buffer-create gpb-git:unstaged-buffer-name))
         (inhibit-read-only t)
         (title (propertize (format "\nUnstaged changes in %s\n\n" repo-dir)
                            'face 'gpb-git:title)))

    (with-current-buffer buf
      (setq default-directory repo-dir)
      (setq-local staged-changes-buffer nil)
      (put 'staged-changes-buffer 'permanent-local t)
      (gpb-git:refresh-hunks)
      (gpb-git:unstaged-changes-mode)
      (add-hook 'post-command-hook 'gpb-git--mark-full-lines nil t)

      (dolist (ov (overlays-in (point-min) (point-min)))
        (when (overlay-get ov 'title)
          (delete-overlay ov)))
      (let ((ov (make-overlay (point-min) (point-min))))
        (overlay-put ov 'before-string title)
        (overlay-put ov 'title t)))

    buf))


(defun gpb-git:refresh-staged-hunk-buffer (repo-dir)
  "Create or refresh the buffer that displays staged changes."
  (let* ((buf (get-buffer-create gpb-git:staged-buffer-name))
         (inhibit-read-only t)
         (title (propertize (format "\nStaged changes in %s\n\n" repo-dir)
                            'face 'gpb-git:title)))

    (with-current-buffer buf
      (setq default-directory repo-dir)
      (setq-local staged-changes-buffer t)
      (put 'staged-changes-buffer 'permanent-local t)
      (gpb-git:refresh-hunks t)
      (gpb-git:staged-changes-mode)
      (add-hook 'post-command-hook 'gpb-git--mark-full-lines nil t)

      (dolist (ov (overlays-in (point-min) (point-min)))
        (when (overlay-get ov 'title)
          (delete-overlay ov)))
      (let ((ov (make-overlay (point-min) (point-min))))
        (overlay-put ov 'before-string title)
        (overlay-put ov 'title t)))

    buf))


(defun gpb-git:refresh-hunks (&optional staged)
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
         (diff-hunks (gpb-git:compute-diff staged))
         (hunk-overlays (list :stub))
         (inhibit-read-only t)
         ;; We attempt preserve the current location.
         (current-hunk-info
          (or (gpb-git:get-current-hunk-info)
              (and (ignore-errors (gpb-git:forward-hunk))
                   (gpb-git:get-current-hunk-info))
              (and (ignore-errors (gpb-git:backward-hunk))
                   (let* ((filename1 (gpb-git:get-current-hunk-info
                                      :filename1))
                          (file1-start (gpb-git:get-current-hunk-info
                                        :file1-start)))
                     `((:filename1 . ,filename1)
                       (:file1-start . ,(1+ file1-start)))))))
         ov)

    (kill-all-local-variables)
    (erase-buffer)
    (gpb-git:delete-hunk-overlays)
    (setq-local header-line-format '(:eval (gpb-git:compute-header)))
    (setq-local default-directory dir)

    (add-hook 'kill-buffer-hook 'gpb-git:kill-buffer-hook nil t)
    (add-hook 'post-command-hook 'gpb-git:post-command-hook)

    (dolist (diff-hunk diff-hunks)
      (let* ((filename1 (gpb-git:get diff-hunk :filename1))
             (filename2 (gpb-git:get diff-hunk :filename2)))
        (setq ov (make-overlay (point)
                               (progn
                                 (insert (or (aget diff-hunk :diff t)
                                             (aget diff-hunk :binary-info t)
                                             " No differences\n"))
                                 (point))))
        (overlay-put ov 'hunk-info diff-hunk)
        (gpb-git:put ov :marked nil)
        (gpb-git:update-hunk-header ov staged)
        (gpb-git:decorate-hunk ov)
        (nconc hunk-overlays (list ov))))

    (goto-char (point-min))
    (setq-local buffer-read-only t)
    (setq-local gpb-git:hunk-overlays (cdr hunk-overlays))
    (when current-hunk-info
      (gpb-git:show-hunk (gpb-git:get current-hunk-info :filename1)
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


(defun gpb-git:put (hunk key value)
  "Set KEY to VALUE in alist associated with HUNK."
   (assert (overlayp hunk))
   (let* ((alist (overlay-get hunk 'hunk-info))
          (key-val (assoc key alist)))
     (if key-val
         (setcdr key-val value)
       (push `(,key . ,value) alist))
     (overlay-put hunk 'hunk-info alist)))


(defun gpb-git:del (hunk key)
  "Remove KEY from the alist associated with HUNK."
  (assert (overlayp hunk))
  (let* ((alist (overlay-get hunk 'hunk-info)))
    (overlay-put hunk 'hunk-info
                 (cl-remove-if (lambda (key-val) (equal (car key-val) key))
                               alist))))


(defun gpb-git:stage-marked-hunks (&optional unstage)
  "Apply the marked hunka to the Git index.
If UNSTAGE is non-nil, we apply the marked hunks in reverse."
  (interactive)
  (let* ((tempfile (make-nearby-temp-file "git-" nil ".patch"))
         (localname (or (file-remote-p tempfile 'localname) tempfile))
         (proc-output-buf gpb-git:process-output-buffer-name)
         (args (if unstage `("apply" "--cached" "-R" ,localname)
                 `("apply" "--cached" ,localname))))

    (with-current-buffer (gpb-git:make-patch unstage)
      (let ((coding-system-for-write 'unix))
        (write-region (point-min) (point-max) tempfile)))

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
          ;; (kill-buffer proc-output-buf)
          (delete-file tempfile)
          (gpb-git:refresh-unstaged-hunk-buffer default-directory)
          (gpb-git:refresh-staged-hunk-buffer default-directory)
          (pop-to-buffer gpb-git:staged-buffer-name))

      ;; If the application failed, we pop to the process output.
      (pop-to-buffer proc-output-buf))))


(defun gpb-git:unstage-marked-hunks ()
  "Toggle the selection of the hunk at the point."
  (interactive)
  (gpb-git:stage-marked-hunks t))


(defun gpb-git:refresh-hunk-buffers ()
  "Refresh the buffers displaying staged and unstaged changes."
  (interactive)
  (gpb-git:refresh-unstaged-hunk-buffer default-directory)
  (gpb-git:refresh-staged-hunk-buffer  default-directory))


(defun gpb-git:show-hunk (filename line-number &optional after)
  "Move to the hunk starting at line LINE-NUMBER in FILENAME.
If AFTER is non-nil, we look at the second filename and line
number; otherwise, we use the first filename and line number.  If
there is not a hunk that starts at line LINE-NUMBER in FILENAME,
we move to the end of the last hunk starting before LINE-NUMBER
in FILENAME, where the set of hunks are ordered by filename and
then line number.  If there are no hunks before LINE-NUMBER in
FILENAME, we move to the start of the first hunk in the buffer."
  (goto-char (point-min))
  (ignore-errors (gpb-git:forward-hunk))
  (dolist (ov (overlays-in (point-min) (point-max)))
    (let* ((hunk-info (overlay-get ov 'hunk-info))
           (hunk-filename (and hunk-info
                               (if after
                                   (gpb-git:get hunk-info :filename2)
                                 (gpb-git:get hunk-info :filename1))))
           (hunk-line-number (and hunk-info
                                  (if after
                                      (gpb-git:get hunk-info :file2-start)
                                    (gpb-git:get hunk-info :file1-start)))))
      (when (and hunk-info
                 (>= (overlay-start ov) (point))
                 (or (string< hunk-filename filename)
                     (and (string= hunk-filename filename)
                          (<= hunk-line-number line-number))))
        (if (and (string= hunk-filename filename)
                 (= hunk-line-number line-number))
            (goto-char (overlay-start ov))
          (goto-char (overlay-end ov))))))

  ;; If the window is not selected, we need to move the window point as
  ;; well.
  (let ((ov (gpb-git:get-current-hunk)))
    (dolist (win (get-buffer-window-list))
      ;; Ensure that the full hunk is visible when possible.
      (when (and ov (overlay-buffer ov))
        (save-excursion
          (save-match-data
            (goto-char (overlay-end ov))
            (when (looking-back "\n") (backward-char))
            (set-window-point win (point))
            (redisplay t))))
      (set-window-point win (point)))
    (when ov (gpb-git:flash-hunk ov))))


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

(defun gpb-git:update-hunk-header (hunk &optional staged)
  "Create the string which appears at the start of a hunk."
  (let ((text (gpb-git:get-hunk-header hunk))
        (inhibit-read-only t))
    (with-current-buffer (overlay-buffer hunk)
      (save-excursion
        (goto-char (overlay-start hunk))
        ;; There is alread a hunk header, delete it.
        (when (get-text-property (point) :hunk-header)
          (delete-region (point) (progn (forward-line 1) (point))))
        (insert (propertize text :hunk-header t))))))


(defun gpb-git:get-hunk-header (hunk)
  "Create the string which appears at the start of a hunk."
  (let* ((staged (with-current-buffer (overlay-buffer hunk)
                   staged-changes-buffer))
         (filename1 (gpb-git:get hunk :filename1))
         (filename2 (gpb-git:get hunk :filename2)))

    (cond
     ;; A deletion that has been marked as a rename
     ((eq (car-safe (gpb-git:get hunk :marked)) :rename)
      (let* ((filename (gpb-git:get hunk :filename1))
             (new-name (cdr (gpb-git:get hunk :marked))))
        (format "%s: delete (marked as rename to %s)\n" filename new-name)))

     ;; New file
     ((gpb-git:get hunk :insertion)
      (assert (string= filename1 filename2))
      (format "%s: %s\n" filename1 (if staged "added" "add")))

     ;; Deleted file
     ((gpb-git:get hunk :deletion)
      (assert (string= filename1 filename2))
      (format "%s: %s\n" filename1 (if staged "deleted" "delete")))

     ;; A standard hunk with diff lines.
     ((ignore-errors (gpb-git:get hunk :diff))
      (let* ((file1-start (gpb-git:get hunk :file1-start))
             (file2-start (gpb-git:get hunk :file2-start))
             (file1-len (gpb-git:get hunk :file1-len))
             (file2-len (gpb-git:get hunk :file2-len))
             (file1-end (1- (+ file1-start file1-len)))
             (file2-end (1- (+ file2-start file2-len)))
             (line-range (format "%s-%s -> %s-%s"
                                 file1-start file1-end
                                 file2-start file2-end)))
        (if (string= filename1 filename2)
            (format "%s: %s\n" filename1 line-range)
          (format "%s -> %s: %s\n" filename1 filename2 line-range))))

     ;; A rename with no other changes to the file.
     ((gpb-git:get hunk :rename)
      (format "%s -> %s\n" filename1 filename2))

     (t (error "Assertion error: unhandled hunk %S" hunk)))))


(defun gpb-git:make-patch (&optional reverse)
  "Construct a patch from the currently marked hunks.

This function should be called from a buffer that is displaying
hunks.  If the region is active, we construct a patch which
corresponds to the portion of the current hunk that is selected.
If the region is not active, we construct a patch that
corresponds to the entire hunk.

If REVERSE is non-nil, we construct the patch so that it can be
applied in reverse.  This is used when removing changes from the
index.

Returns a buffer with the name `gpb-git:patch-buffer-name' that
contains the patch."
  (let* ((patch-buf (get-buffer-create gpb-git:patch-buffer-name))
         (output-offset 0)
         current-output-file)

    (with-current-buffer patch-buf (erase-buffer))

    (dolist (hunk (gpb-git:get-marked-hunks))
      (cond
       ;; A deletion that has been marked as a rename.
       ((eq (car-safe (gpb-git:get hunk :marked)) :rename)
        (let* ((filename1 (gpb-git:get hunk :filename1))
               (filename2 (gpb-git:get hunk :filename2))
               (new-name (cdr (gpb-git:get hunk :marked))))
          (assert (string= filename1 filename2))
          (with-current-buffer patch-buf
            (insert (format
                     (concat "diff --git a/%s b/%s\n"
                             "rename from %s\n"
                             "rename to %s\n")
                     filename1 new-name filename1 new-name)))))
       ;; If the hunk is marked and there are no diff lines, insert the
       ;; header.
       ((and (gpb-git:get hunk :marked)
             (not (ignore-errors (gpb-git:get hunk :diff))))
        (with-current-buffer patch-buf
          (insert (gpb-git:get hunk :header))))
       ;; Otherwise we use the primary algorithm in `gpb-git:make-patch-1'.
       (t
        (gpb-git:make-patch-1 hunk patch-buf reverse))))

    patch-buf))


(defun gpb-git:make-patch-1 (hunk patch-buf reverse)
  "Write patch content for HUNK into PATCH-BUF.

This function is an implemenation detail of `gpb-git:make-patch'."
  (let* ((header (gpb-git:get hunk :header))
         (input-file (if reverse (gpb-git:get hunk :filename2)
                       (gpb-git:get hunk :filename1)))
         (output-file (if reverse (gpb-git:get hunk :filename1)
                        (gpb-git:get hunk :filename2)))
         (input-start (if reverse (gpb-git:get hunk :file2-start)
                        (gpb-git:get hunk :file1-start)))
         (new-file (if reverse (gpb-git:get hunk :deletion)
                     (gpb-git:get hunk :insertion)))
         (marked-lines (gpb-git--get-marked-lines hunk))
         (patch-start (with-current-buffer patch-buf (point)))
         (has-no-changes t)
         (input-lines 0)
         (output-lines 0)
         (i 0)
         diff-line first-char)

    ;; If have changed files, return the offset between hunks in the
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
           ((and reverse marked-lines (gpb-git:get hunk :rename))
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


(defun gpb-git:flash-hunk (ov)
  ;; If we are flashing another region, reset it now.
  (gpb-git:unflash-hunk)
  (with-current-buffer (overlay-buffer ov)
    (gpb-git:decorate-hunk ov :focused))
  (setq gpb-git:currently-flashed-hunk ov)
  (run-at-time 0.5 nil 'gpb-git:unflash-hunk))


(defun gpb-git:unflash-hunk ()
  (let ((ov gpb-git:currently-flashed-hunk))
    (when (and ov (overlay-buffer ov))
      (with-current-buffer (overlay-buffer ov)
        (gpb-git:decorate-hunk ov)))
    (setq gpb-git:currently-flashed-hunk nil)))


(defun gpb-git:mark-hunk-command ()
  "Mark the current hunk."
  (interactive)
  (gpb-git:mark-hunk)
  (gpb-git:forward-hunk-command))


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
         (filename1 (gpb-git:get hunk :filename1))
         ;; Number of lines in the hunk not counting the header line.
         (nlines (- (line-number-at-pos (overlay-end hunk))
                    (1+ (line-number-at-pos (overlay-start hunk)))))
         (inhibit-read-only t))

    (when new-name
      (unless (gpb-git:get hunk :deletion)
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
            (gpb-git:put hunk :marked `(:partial . ,line-is-marked))
          (gpb-git:put hunk :marked nil))
        (deactivate-mark t)))

     (unmark (gpb-git:put hunk :marked nil))
     (new-name (gpb-git:put hunk :marked `(:rename . ,new-name)))
     (t (gpb-git:put hunk :marked t)))

    ;; Update the text properties to reflect the new state of the hunk.
    (gpb-git:update-hunk-header hunk)
    (gpb-git:decorate-hunk hunk)
    (setq gpb-git:currently-focused-hunk nil)))


(defun gpb-git:unmark-hunk (&optional unmark new-name)
  "Unmark the current hunk."
  (interactive)
  (gpb-git:mark-hunk 'unmark))


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
             (unless (gpb-git:get hunk :deletion)
               (user-error "Only deleted hunks can be marked as a rename")))
           (file-relative-name (read-file-name "New name: ")))))
  (gpb-git:mark-hunk nil new-name))


(defun gpb-git:unmark-hunk-command ()
  "Mark the current hunk."
  (interactive)
  (gpb-git:unmark-hunk)
  (gpb-git:forward-hunk-command))


(defun gpb-git:list-unstaged-files (&optional dir)
  "List unstaged, non-ignored, files in DIR.
DIR defaults to `default-directory' if omitted."
  (let ((dir (or dir default-directory)))
    (with-temp-buffer
      (setq default-directory dir)
      (unless (= (process-file "git" nil t nil "ls-files"
                               "--others" "--exclude-standard")
                 0)
        (error (buffer-substring (point-min) (point-max))))
      (split-string (buffer-substring (point-min) (point-max)) "\n" t))))


(defun gpb-git:get-marked-hunks (&optional buf)
  "Return the list of marked hunks in `buf'"
  (with-current-buffer (or buf (current-buffer))
    (sort
     (remove-if-not
      (lambda (ov)
        (let ((hunk-is-marked (ignore-errors (gpb-git:get ov :marked)))
              (line-is-marked (ignore-errors (gpb-git:get ov :marked-lines))))
          (or hunk-is-marked
              (and line-is-marked (cl-some (lambda (x) x) line-is-marked)))))
      (overlays-in (point-min) (point-max)))
     (lambda (ov1 ov2) (< (overlay-start ov1) (overlay-start ov2))))))


(define-derived-mode gpb-git:commit-message-mode diff-mode "Git Commit"
  "Mode for editing a Git commit message and completing the commit."
  (local-set-key "\C-c\C-c" 'gpb-git:complete-commit))


(defun gpb-git:commit ()
  (interactive)
  (let ((dir default-directory)
        (buf (get-buffer-create gpb-git:commit-message-buffer-name))
        filename)
    (with-current-buffer buf
      (erase-buffer)
      (setq default-directory dir)
      ;; Set the editor to echo so it will write the name of the commit
      ;; message file into the buffer, but git will abort the commit as the
      ;; message file was not modified.
      (process-file "git" nil t nil "-c" "core.editor=echo" "commit" "-v")
      (goto-char (point-min))
      (while (not (or (eobp) (looking-at-p ".*COMMIT_EDITMSG$")))
        (forward-line 1))
      (when (eobp)
        (pop-to-buffer buf)
        (error "Couldn't find commit message file"))
      (setq filename (concat
                      (or (file-remote-p default-directory) "")
                      (buffer-substring-no-properties
                       (point) (progn (end-of-line) (point)))))
      (erase-buffer)
      (insert "\n")
      (insert-file-contents filename)
      (goto-char (point-min))
      (gpb-git:commit-message-mode)
      (setq-local commit-message-file filename))

    (switch-to-buffer buf)))


(defun gpb-git:complete-commit (arg)
  "Complete the current commit.
With a prefix argument, prompt the user for the commit command."
  (interactive "P")
  (let* ((dir default-directory)
         ;; The buffer we use to write the cleaned commit message.
         (clean-buf (get-buffer-create "*clean Git commit message*"))
         ;; The buffer we use to show the output of the commit command.
         (proc-buf (get-buffer-create gpb-git:commit-buffer-name))
         ;; `commit-message-file' was set by `gpb-git:commit'.
         (filename commit-message-file)
         (localname (file-relative-name commit-message-file))
         (coding-system-for-write 'unix)
         (cmd (split-string-and-unquote
               (if arg (read-string "Commit command: "
                                    (format "git commit -F \"%s\"" localname))
                 (format "git commit -F \"%s\"\n" localname))))
         text)
    (goto-char (point-min))

    ;; It appears that --cleanup is ignored when using -F, so we have to
    ;; trim the buffer outselves.  Boo.
    (with-current-buffer clean-buf (erase-buffer))
    (while (not (eobp))
      (cond
       ;; Snip everything after this.
       ((looking-at-p "^# +-+ +>8 +-+")
        (goto-char (point-max)))
       ;; Skip commentary lines.
       ((looking-at-p "^#")
        (forward-line 1))
       ;; Otherwise, keep the line.
       (t
        (setq text (buffer-substring-no-properties
                    (point) (progn (forward-line 1) (point))))
        (with-current-buffer clean-buf (insert text)))))

    ;; Save the current commit message.
    (with-current-buffer clean-buf
      (setq gpb-git:commit-messages (cons (buffer-substring-no-properties
                                           (point-min) (point-max))
                                          gpb-git:commit-messages))

      (write-region (point-min) (point-max) filename))

    ;; Now call git commit ...
    (with-current-buffer proc-buf
      (erase-buffer)
      (setq default-directory dir)
      (insert (combine-and-quote-strings cmd) "\n")
      (when (= 0 (apply 'process-file (car cmd) nil t nil (cdr cmd)))
        (kill-buffer gpb-git:commit-message-buffer-name))
      (goto-char (point-min)))

    (switch-to-buffer proc-buf)))


(defun gpb-git--show-faces ()
  (interactive)
  (let ((text (concat "@@ -89,21 +89,59 @@\n Some context\n-(defface "
                      "gpb-git:highlighted-hunk-header\n+;; Focused "
                      "faces\n '((t :background \"#9999ff\"))\n-  "
                      "\"Face used for context lines in the highlighted "
                      "hunk\")\n+  \"Face used for context lines in the "
                      "focused hunk\")\n \n")))
  (with-current-buffer (get-buffer-create "*show faces*")
    (erase-buffer)
    (dolist (modifier '(nil :focused :marked :focused-and-marked))
      (gpb-git:decorate-hunk `(,(point) . ,(progn (insert text) (point)))
                              modifier))
    (pop-to-buffer (current-buffer)))))


(defvar-local gpb-git--mark-full-lines-overlay nil
  "The overlay used to visually extend the region to full lines.")

(defun gpb-git--mark-full-lines ()
  "Unsure that the visual indication of the region includes full lines."
  (let* ((ov gpb-git--mark-full-lines-overlay)
         (region-active (region-active-p))
         (beg (when region-active
                (save-excursion (goto-char (region-beginning))
                                (forward-line 0)
                                (point))))
         (end (when region-active
                (save-excursion (goto-char (region-end))
                                (unless (bolp) (forward-line 1))
                                (point)))))
    (cond
     ;; The region is active and the overlay already exists, so we move it.
     ((and region-active ov)
      (move-overlay ov beg end))
     ;; The region is active and the overlay doesn't exists, so we create
     ;; it.
     (region-active
      (setq ov (make-overlay beg end))
      (overlay-put ov 'face 'region)
      (setq gpb-git--mark-full-lines-overlay ov))
     ;; The region is not active but an overlay exists, so we delete it.
     (ov
      (delete-overlay ov)
      (setq gpb-git--mark-full-lines-overlay nil)))))


(defun gpb-git--get-marked-lines (hunk)
  "Return the set of line in `hunk' that are marked.
`hunk' is an overlay with associated data.  Returns an array of
bools in which the i-th entry is true if i-th line in the hunk is
marked."
  (let ((marked (gpb-git:get hunk :marked))
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


(global-set-key "\C-cs" 'gpb-git:stage-changes)
