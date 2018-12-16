;;  Tools for staging and committing hunks in Git
;;  We use git command `diff-files` to find the changes in the working
;;  directory relative to the index and the command `diff-index` to see the
;;  changes in the index relative to HEAD.
;;  We parse the values returned by these functions to identify the hunks,
;;  and then insert the hunks into buffers whose names are given by
;;  `gpb-git:unstaged-buffer-name' and `gpb-git:staged-buffer-name' and
;;  place an onverlay on each hunk.  We then give the user an opportunity
;;  to mark hunks in these buffers and we record the marked hunks by
;;  useing the :marked property on the hunk overlays.
;;  Applying marked hunks in `gpb-git:unstaged-buffer-name' applies them to
;;  the index; applying marked hunks in `gpb-git:staged-buffer-name'
;;  removes them from the index.
;;  cover each hunk with an overlay so we can easily toggle the visibility
;;  of each hunk ensuring that each hunk is always visible in one and only
;;  one of the buffers.  Initially, the first buffer shows the unstaged
;;  changes and the second buffer shows the changes that have been staged,
;;  but the user may then move hunks back and forth between the buffers.
;;  Once we are done choosing hunks, we then reset the index to HEAD,
;;  write all the hunks in the right buffer to a patch, and apply the
;;  patch to the index via `git apply --cached TEMPFILE`.
;;  The primary data structure we use is a hunk alist that contains
;;  information about a given unit of change.  The function
;;  `gpb-git:compute-diff' returns a list of such alists, and these alists
;;  are stored in the 'hunk-info property of the overlays that are created
;;  by `gpb-git:insert-hunks' when this function writes the hunks into a
;;  buffer.
;;
;;  These alists have the following entries:
;;
;;    :filename1 A string giving the first filename in the diff header.
;;    :filename2 A string giving the second filename in the diff header.
;;    :file1-start An integer giving the first line to which the hunk
;;        applies in the before state.  The first line of the file is
;;        1, but this value may be zero when creating a new file.
;;    :file1-len The number of lines in the input file to which the
;;        hunk applies.
;;    :file2-start An integer giving the first line in the second file
;;        to which the hunk applied.  The first line of the file is 1.
;;    :file2-len The number of lines in the ouput file that application
;;        of the hunk produces.
;;    :header A string giving the diff header.  These are lines
;;        starting with "diff --git ..." and ending at the first diff
;;        lines header (i.e., lines of the form "@@ ... @@").
;;    :binary-info A string or nil.  This value is set when the hunk
;;        correponds to a change to a binary file.  In this case, it
;;        contains a string describing the change.
;;    :insertion bool which is true when the hunk correspond to the
;;        creation of new file.
;;    :deletion bool which is true when the hunk correspond to the
;;        deletion of a new file.
;;    :rename bool which is true when the hunk correspond to a file
;;        rename.
;;
;;  In addition to those listed above, the hunk overlay also have
;;  the following property:
;;
;;    :is-hunk t
;;    :marked This property is non-nil if the hunk is marked.  If the
;;      entire hunk is marked, this property is t.  If only some lines are
;;      marked, the value is a cons cell of the form
;;      (:partial. line-is-marked) where line-is-marked is a bool vector
;;      with one entry for each line in the hunk.  If a deleted file has
;;      been marked as a rename (see `gpb-git:mark-as-rename'), the value
;;      is a cons cell of the form (:rename . filename).
(defun gpb-git--blend-colors (c1 c2 &optional alpha1 alpha2)
       :background ,(gpb-git--blend-colors "#f0c0c0" "white" 0.6)))
       :background ,(gpb-git--blend-colors "#b8e0b8" "white" 0.6)))
  `((t :background ,(gpb-git--blend-colors "khaki4" "white" 0.6)))
  `((t :background ,(gpb-git--blend-colors "khaki2" "white" 0.6)))
  `((t :background ,(gpb-git--blend-colors "khaki2" "white" 0.8)))
  `((t :background ,(gpb-git--blend-colors "khaki3" "white" 0.6)))
  `((t :background ,(gpb-git--blend-colors "khaki2" "white" 0.72 0.22)))
       :background ,(gpb-git--blend-colors "khaki2" "black" 0.95)))
       :background ,(gpb-git--blend-colors "#f0c0c0" "khaki3" 0)))
    (define-key map "m" 'gpb-git:mark-hunk-command)
    (define-key map "M" 'gpb-git:mark-file-command)
    (define-key map "u" 'gpb-git:unmark-hunk-command)
    (define-key map "U" 'gpb-git:unmark-file-command)
    (define-key map "m" 'gpb-git:mark-hunk-command)
    (define-key map "M" 'gpb-git:mark-file-command)
    (define-key map "u" 'gpb-git:unmark-hunk-command)
    (define-key map "U" 'gpb-git:unmark-file-command)
;;
;;  Hunk buffer modes
;;

(define-derived-mode gpb-git:hunk-buffer-mode special-mode
  "Hunk Buffer"
  "\nMode for buffers showing hunks."
  (setq-local header-line-format '(:eval (gpb-git:compute-header)))
  (setq-local buffer-read-only t)
  (setq-local tab-width 4)
  (add-hook 'post-command-hook 'gpb-git:post-command-hook)
  (add-hook 'post-command-hook 'gpb-git--mark-full-lines nil t)
  (add-hook 'kill-buffer-hook 'gpb-git:kill-buffer-hook nil t))


(define-derived-mode gpb-git:unstaged-changes-mode gpb-git:hunk-buffer-mode
  "Unstaged Changes"
  "\nMode for selecting unstages changes to be added to the index.

\\{gpb-git:unstaged-changes-mode-map}\n"
  (setq-local staged-changes-buffer nil))


(define-derived-mode gpb-git:staged-changes-mode gpb-git:hunk-buffer-mode
\\{gpb-git:staged-changes-mode-map}\n"
  (setq-local staged-changes-buffer t))
         (unstaged-buf (gpb-git:get-unstaged-changes-buffer dir))
         (staged-buf (gpb-git:get-staged-changes-buffer dir)))
HUNK is an overlay with properties summarized at the top of this
file.  If FOCUSED is non-nil, we use alternative faces."
  (assert (and (overlayp hunk) (overlay-buffer hunk)))
  (with-current-buffer (overlay-buffer hunk)
    (let* ((beg (overlay-start hunk))
           (end (copy-marker (overlay-end hunk)))
           (marked (gpb-git--marked-p hunk))
           (inhibit-read-only t) (i 0) marked-lines line-marked)
      (save-excursion
        (goto-char beg)

        ;; If there is already a hunk header, delete it.
        (when (get-text-property (point) :hunk-header)
          (delete-region (point) (progn (forward-line 1) (point))))
        (insert (propertize
                 (gpb-git:get-hunk-header hunk)
                 :hunk-header t
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
          ;; Temporarily move the point and force redisplay to scroll the
          ;; window.
          (redisplay t))))))
  "Move back to the start of the current hunk.
Move back to the start of the previous hunk, if we are already at
the start of the current hunk."
  (when (= (line-number-at-pos) (point-min)) (user-error "Beginning of buffer"))
  (let ((pt (point)) hunk)
    (forward-line -1)
    (while (and (not (gpb-git:get-current-hunk)) (not (bobp)))
      (forward-line -1))
    (when (bobp)
      (goto-char pt)
      (user-error "No earlier hunks"))
    (setq hunk (gpb-git:get-current-hunk))
    (goto-char (overlay-start hunk))
    (point)))
    (assert (eq (window-buffer win) (current-buffer)))))
    (gpb-git--update-highlights)))
(defun gpb-git--update-highlights (&optional buf)
  "Updates hunk highlighting.
Implementation detail of `gpb-git:post-command-hook'."

          (gpb-git:decorate-hunk prev-hunk))

          (gpb-git:decorate-hunk new-hunk t))
         (hunk-overlays (gpb-git--get-hunk-overlays)))
      (let* ((current-file (overlay-get ov :filename1))
             (file-hunk-overlays (gpb-git--get-hunk-overlays current-file))
keys described in the comments at the top of this file."
         beg end insertion deletion filename1 filename2 header binary-info)
                                    (:binary-info . ,binary-info)
                                (or (re-search-backward "^--- /dev/null" beg t)
                                    (re-search-backward "^new file" beg t)))
                               (re-search-backward "^+++ /dev/null" beg t))
                    binary-info (save-excursion
                                  (when (re-search-backward "^Binary files"
                                                            beg t)
                                    (buffer-substring-no-properties
                                     (point) (progn (end-of-line) (point)))))
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
                          (:binary-info . ,binary-info)
(defun gpb-git:get-unstaged-changes-buffer (repo-dir)
  "A buffer that displaying unstaged changes."
  (let* ((buf (get-buffer-create gpb-git:unstaged-buffer-name)))
      (setq default-directory repo-dir)
      (gpb-git:update-hunks))
    buf))
;; (defun gpb-git:refresh-unstaged-changes-buffer ()
;;   "Refresh the buffer displaying unstaged changes."
;;   (let* ((buf (get-buffer gpb-git:unstaged-buffer-name))
;;          (inhibit-read-only t))
;;     (with-current-buffer buf
;;       (erase-buffer)
;;       (dolist (ov (gpb-git--get-hunk-overlays)) (delete-overlay ov))
;;       (insert (format "\nUnstaged changes in %s\n\n" repo-dir))
;;       (gpb-git:insert-hunks (gpb-git:compute-diff)))))
(defun gpb-git:get-staged-changes-buffer (repo-dir)
  (let* ((buf (get-buffer-create gpb-git:staged-buffer-name)))
      (setq default-directory repo-dir)
      (gpb-git:update-hunks))
;; (defun gpb-git:refresh-staged-changes-buffer ()
;;   "Create or refresh the buffer that displays staged changes."
;;   (let* ((buf (get-buffer-create gpb-git:staged-buffer-name))
;;          (inhibit-read-only t))
;;     (with-current-buffer buf
;;       (erase-buffer)
;;       (dolist (ov (gpb-git--get-hunk-overlays)) (delete-overlay ov))
;;       (insert (format "\nUnstaged changes in %s\n\n" repo-dir))
;;       (gpb-git:insert-hunks (gpb-git:compute-diff t)))))


;; (defun gpb-git:insert-hunks (diff-info)
;;   "Insert the hunks in DIFF-INFO after the point."
;;   (save-excursion
;;     (let* (ov)
;;       (dolist (diff-hunk diff-info)
;;         (let* ((filename1 (aget diff-hunk :filename1 t))
;;                (filename2 (aget diff-hunk :filename2 t)))
;;           (setq ov (make-overlay (point)
;;                                  (progn
;;                                    (insert (or (aget diff-hunk :diff t)
;;                                                (aget diff-hunk :binary-info t)
;;                                                " No differences\n"))
;;                                    (point))))
;;           (dolist (key-val diff-hunk)
;;             (overlay-put ov (car key-val) (cdr key-val)))
;;           (overlay-put ov :is-hunk t)
;;           (overlay-put ov :marked nil)
;;           (gpb-git:decorate-hunk ov))))))


(defun gpb-git:update-hunks ()
  "Insert the hunks in DIFF-INFO after the point."
  (let* ((inhibit-read-only t) ov)
    (dolist (ov (gpb-git--get-hunk-overlays)) (delete-overlay ov))
    (insert (format "\n%s changes in %s\n\n"
                    (if staged-changes-buffer "Staged" "Unstaged")
                    default-directory))
    ;; Get a status overview
    (process-file "git" nil t nil "-c" "advice.statusHints=false" "status")
    (save-excursion
      (if staged-changes-buffer
          ;; We are in the staged changes buffer.
          (progn
            (delete-region (re-search-backward "^On branch")
                           (progn (forward-line 2) (point)))
            (when (re-search-forward "^Changes not" nil t)
              (delete-region (match-beginning 0)
                             (progn
                               (forward-line 1)
                               (while (looking-at-p "^\t") (forward-line 1))
                               (forward-line 1)
                               (point))))
            (when (re-search-forward "^Untracked" nil t)
              (delete-region (match-beginning 0)
                             (progn
                               (forward-line 1)
                               (while (looking-at-p "^\t") (forward-line 1))
                               (forward-line 1)
                               (point)))))
        ;; We are in the unstaged changes buffer.
        (when (re-search-backward "^no changes added" nil t)
          (delete-region (match-beginning 0) (progn (forward-line 1) (point))))
        (when (re-search-backward "^Changes to" nil t)
          (delete-region (match-beginning 0)
                         (progn
                           (forward-line 1)
                           (while (looking-at-p "^\t") (forward-line 1))
                           (point))))))

    (save-excursion
      (dolist (diff-hunk (gpb-git:compute-diff staged-changes-buffer))
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
          (gpb-git:decorate-hunk ov))))))
          (gpb-git:refresh-hunk-buffers)
          (pop-to-buffer gpb-git:staged-buffer-name))
  (with-current-buffer gpb-git:unstaged-buffer-name
    (gpb-git:update-hunks))
  (with-current-buffer gpb-git:staged-buffer-name
    (gpb-git:update-hunks)))
(defun gpb-git--get-hunk-overlays (&optional filename)
  (let ((hunks (cl-remove-if-not (lambda (ov) (overlay-get ov :is-hunk))
        (cl-remove-if-not (lambda (ov) (string= (overlay-get ov :filename1)
  "Return the hunk overlay at POS.
If POS is omitted, we return the hunk at the current point or the
start of the reigon, if the region is active.
"
    (cdr (get-char-property-and-overlay pos :is-hunk))))
         (mapcar (lambda (ov) (overlay-get ov :filename1))
                 (gpb-git--get-hunk-overlays)))
         (filename1 (overlay-get hunk :filename1))
         (filename2 (overlay-get hunk :filename2)))
     ((gpb-git--marked-as-rename-p hunk)
      (let* ((filename (overlay-get hunk :filename1))
             (new-name (gpb-git--get-new-name hunk)))
     ((overlay-get hunk :insertion)
     ((overlay-get hunk :deletion)
     ((ignore-errors (overlay-get hunk :diff))
      (let* ((file1-start (overlay-get hunk :file1-start))
             (file2-start (overlay-get hunk :file2-start))
             (file1-len (overlay-get hunk :file1-len))
             (file2-len (overlay-get hunk :file2-len))
     ((overlay-get hunk :rename)
       ((gpb-git--marked-as-rename-p hunk)
        (let* ((filename1 (overlay-get hunk :filename1))
               (filename2 (overlay-get hunk :filename2))
               (new-name (gpb-git--get-new-name hunk)))
       ((and (gpb-git--marked-p hunk)
             (not (overlay-get hunk :diff)))
          (insert (overlay-get hunk :header))))
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
              include-line (aref marked-lines i)
           ((and reverse marked-lines (overlay-get hunk :rename))
  (let ((region-active (region-active-p)))
    (gpb-git:mark-hunk)
    (unless region-active (gpb-git:forward-hunk-command))))
  "Mark the hunk at the current point.
intersect the region.  If UNMARK is non-nil we unmark the hunk or
         (filename1 (overlay-get hunk :filename1))
         ;; Number of lines in the hunk not counting the header line.
      (unless (overlay-get hunk :deletion)
     ;; Mark or unmark a subset of the current hunk.
            (line-is-marked (gpb-git--get-marked-lines hunk))
            (overlay-put hunk :marked `(:partial . ,line-is-marked))
          (overlay-put hunk :marked nil))
        (deactivate-mark t)))

     (unmark (overlay-put hunk :marked nil))
     (new-name (overlay-put hunk :marked `(:rename . ,new-name)))
     (t (overlay-put hunk :marked t)))
(defun gpb-git:mark-file-command (&optional unmark)
  "Mark all hunks in the current file.
If UNMARK is non-nil we unmark all hunks in the current file."
  (interactive)
  (let* ((hunk (or (gpb-git:get-current-hunk)
                   (user-error "Point is not in a hunk")))
         (filename1 (overlay-get hunk :filename1))
         (pt 0))
    (dolist (hunk (gpb-git:get-hunk-overlays filename1))
      (goto-char (overlay-start hunk))
      (setq pt (max pt (point)))
      (gpb-git:mark-hunk unmark))
    (goto-char pt)
    (gpb-git:forward-hunk-command)))


(defun gpb-git:unmark-file-command ()
  "Unmark all hunks in the current file."
  (interactive)
  (gpb-git:mark-file-command t))


             (unless (overlay-get hunk :deletion)
  (gpb-git:forward-hunk-command))
     (remove-if-not 'gpb-git--marked-p
                    (overlays-in (point-min) (point-max)))
      (while (not (or (eobp) (looking-at-p ".*COMMIT_EDITMSG$")))
        (forward-line 1))
      (when (eobp)
        (pop-to-buffer buf)
        (error "Couldn't find commit message file"))
      (setq filename (concat
                      (or (file-remote-p default-directory) "")
                      (buffer-substring-no-properties
                       (point) (progn (end-of-line) (point)))))
         ;; `commit-message-file' was set by `gpb-git:commit'.
         (filename commit-message-file)
         (localname (file-relative-name commit-message-file))
                                    (format "git commit -F \"%s\"" localname))
                 (format "git commit -F \"%s\"\n" localname))))
    (gpb-git:refresh-hunk-buffers)
(defun gpb-git:show-faces ()
  "Show all the faces in a test buffer"
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
    (setq-local staged-changes-buffer nil)
    (setq-local gpb-git:show-faces-buffer t)

    (insert "Faces used to display hunks\n")
    (insert "===========================\n\n")

    (insert "Base:\n")
    (setq ov1 (make-overlay (point) (progn (insert text) (point))))
    (dolist (key-val props) (overlay-put ov1 (car key-val) (cdr key-val)))
    (gpb-git:decorate-hunk ov1)

    (insert "\nFocused:\n")

    (setq ov2 (make-overlay (point) (progn (insert text) (point))))
    (dolist (key-val props) (overlay-put ov2 (car key-val) (cdr key-val)))
    (gpb-git:decorate-hunk ov2 t)

    (insert "\nMarked:\n")

    (setq ov3 (make-overlay (point) (progn (insert text) (point))))
    (dolist (key-val props) (overlay-put ov3 (car key-val) (cdr key-val)))
    (overlay-put ov3 :marked t)
    (gpb-git:decorate-hunk ov3)

    (insert "\nFocused and Marked:\n")

    (setq ov4 (make-overlay (point) (progn (insert text) (point))))
    (dolist (key-val props) (overlay-put ov4 (car key-val) (cdr key-val)))
    (overlay-put ov4 :marked t)
    (gpb-git:decorate-hunk ov4 t)

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

