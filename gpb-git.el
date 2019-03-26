;;
;;  Tools for staging and committing hunks in Git
;;
;;  This package provides the command `gpb-git:stage-changes' which opens
;;  two side-by-side buffers for selecting unstaged changes that should be
;;  applied to the Git index, selecting staged changes to be removed, and
;;  committing the currently staged changes.  This is essentially a GUI
;;  interface to `git add --patch`, but the implementation makes no use of
;;  this command.
;;
;;  To get started, call `gpb-git:stage-changes' from a buffer whose
;;  `default-directory' lies inside a Git a repository and then call
;;  `describe-mode' in the buffers that appear to get help on the
;;  keybindings and workflow.
;;
;;  This package does not attempt to provide a full Git porcelain.  In
;;  particular, you will still need to use the command line (or `vc' or
;;  `magit') to view logs, merge branches, and perform other actions.
;;
;;  Due to a bug/missing feature in TRAMP, `magit' cannot commit hunks to a
;;  Git reposity on a remote machine over TRAMP from a Windows machine.
;;  This package works in this case because we write files rather than
;;  trying to pipe patchs into the stdin of a Git process.  See
;;  https://github.com/magit/magit/issues/3624 and
;;  http://lists.gnu.org/archive/html/tramp-devel/2018-11/msg00020.html
;;
;;  Implementation overview:
;;
;;  We use git command `diff-files` to find the changes in the working
;;  directory relative to the index and we insert these hunks into a buffer
;;  named `gpb-git:unstaged-buffer-name', placing an overlay on each hunk.
;;  We then give the user an opportunity to mark hunks and portions of
;;  hunks in this buffers, recording the marked hunks using properties on
;;  the hunk overlays.  Once the user has finished selecting the hunks, she
;;  calls `gpb-git:stage-marked-hunks' to construct a patch from the
;;  selected hunks and apply it to the Git index.
;;
;;  Similarly, we use `diff-index --cached` to find the changes in the
;;  index relative to HEAD, place these hunks in the buffer
;;  `gpb-git:staged-buffer-name', and give the user an opportunity mark
;;  hunks and portions of hunks there as well.  When done, the user may
;;  call `gpb-git:unstage-marked-hunks' to remove existing changes from the
;;  index.  In this case, we create the patch, but apply it in reverse.
;;
;;  The primary complex data structure used is a hunk alist that contains
;;  information from the Git diff output about a single unit of change.
;;  The function `gpb-git:compute-diff' returns a list of such alists.
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
;;    :diff A string or nil giving the changes to the file.  This is a
;;        series of line additions and deletions surrounded by context
;;        lines that describe the changes to be applied to the file.  Some
;;        hunks have not diff lines (e.g., a file rename with no changes,
;;        the addition of an empty file to the index using git add
;;        --intent-to-add, or a change to a binary file).  In these case,
;;        :diff is nil.
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
;;  The function `gpb-git:update-hunks' calls `gpb-git:compute-diff' to
;;  produce a list of changes and inserts these changes into a buffer,
;;  placing an overlay on the section of the buffer that correponds to each
;;  hunk.  These overlays have all of the properties listed above as well
;;  as the following properties (you can call `describe-text-properties' in
;;  a hunk buffer to see the overlay properties):
;;
;;    :is-hunk t
;;    :marked This property is non-nil if the hunk is marked.  If the
;;        entire hunk is marked, this property is t.  If only some lines
;;        are marked, the value is a cons cell of the form (:partial
;;        . line-is-marked) where line-is-marked is a bool vector with one
;;        entry for each line in the hunk.  If a deleted file has been
;;        marked as a rename (see `gpb-git:mark-as-rename'), the value is a
;;        cons cell of the form (:rename . filename).
;;

(defvar gpb-git:unstaged-buffer-name "*unstaged changes*"
  "The name of the buffer used to show staged changes.")

(defvar gpb-git:staged-buffer-name "*staged changes*"
  "The name of the buffer used to show unstaged changes.")

(defvar gpb-git:patch-buffer-name "*git patch*"
  "The name of the temporary buffer used to construct patches.")

(defvar gpb-git:process-output-buffer-name "*git output*"
  "The name of the buffer used to display Git output.")

(defvar gpb-git:commit-message-buffer-name "*commit message*"
  "The name of the temporary buffer used to edit commit messages.")


(defvar gpb-git:currently-focused-hunk nil
  "Tracks the currently focused hunk (see `gpb-git--update-highlights').")

(defvar gpb-git:saved-window-configuration nil)

(defvar gpb-git:commit-messages nil
  "We save all commit messages so they can be recovered.")

(defvar gpb-git:status-script-cache nil
  "An alist mapping from TRAMP remotes to filenames.

Each entry has a key corresponding to the TRAMP prefix string as
returned by `file-remote-p' and a value that is the local
filename of a script for producing a status update.")

;;
;;  Faces
;;
;;  The function `gpb-git:show-faces' can be used to view these faces in a
;;  buffer.
;;

(defun gpb-git--blend-colors (c1 c2 &optional alpha1 alpha2)
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
       :background ,(gpb-git--blend-colors "#f0c0c0" "white" 0.6)))
  "Face used for the deleted lines in a hunk")

(defface gpb-git:added-line
  `((t :foreground "#004400"
       :background ,(gpb-git--blend-colors "#b8e0b8" "white" 0.6)))
  "Face used for the added lines in a hunk")

;; Focused faces

(defface gpb-git:focused-hunk-header
  '((t :background "cornflower blue"))
  "Face used for context lines in the focused hunk")

(defface gpb-git:focused-context-line
  `((t :background ,(gpb-git--blend-colors "cornflower blue" "white" 0.25)))
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
  `((t :background ,(gpb-git--blend-colors "khaki4" "white" 0.6)))
  "Face used for context lines in a marked hunk")

(defface gpb-git:marked-context-line
  `((t :background ,(gpb-git--blend-colors "khaki2" "white" 0.6)))
  "Face used for context lines in the marked hunk")

(defface gpb-git:marked-added-line
  `((t :background ,(gpb-git--blend-colors "khaki2" "white" 0.8)))
  "Face used for the added lines in the marked hunk.")

(defface gpb-git:marked-deleted-line
  `((t :background ,(gpb-git--blend-colors "khaki3" "white" 0.6)))
  "Face used for deleted lines in the marked hunk.")

;; Focused and marked faces

(defface gpb-git:focused-and-marked-hunk-header
  '((t :background "khaki4"))
  "Face used for context lines in a marked hunk")

(defface gpb-git:focused-and-marked-context-line
  `((t :background ,(gpb-git--blend-colors "khaki2" "white" 0.72 0.22)))
  "Face used for context lines in the marked hunk")

(defface gpb-git:focused-and-marked-added-line
  `((t ;; :foreground "#003000"
       :background ,(gpb-git--blend-colors "khaki2" "black" 0.95)))
  "Face used for the added lines in the marked hunk.")

(defface gpb-git:focused-and-marked-deleted-line
  `((t ;; :foreground "#660000"
       :background ,(gpb-git--blend-colors "#f0c0c0" "khaki3" 0)))
  "Face used for deleted lines in the marked hunk.")

;;
;;  Keymaps
;;

(defvar gpb-git:user-command-prefix-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "s" 'gpb-git:stage-changes)
    (define-key map "i" 'gpb-git:unstage-changes)
    (define-key map "c" 'gpb-git:commit)
    (fset 'gpb-git:user-command-prefix-keymap map)
    map)
  "The prefix keymap for user commands.")


(defvar gpb-git:unstaged-changes-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'gpb-git:forward-command)
    (define-key map [(backtab)] 'gpb-git:backward-command)
    (define-key map "p" 'gpb-git:backward-command)
    (define-key map "n" 'gpb-git:forward-command)
    (define-key map "x" 'gpb-git:stage-marked-hunks)
    (define-key map "d" 'gpb-git:revert-marked-hunks)
    (define-key map "g" 'gpb-git:refresh-status)
    (define-key map "m" 'gpb-git:mark-hunk-command)
    (define-key map "M" 'gpb-git:mark-file-command)
    (define-key map "u" 'gpb-git:unmark-hunk-command)
    (define-key map "U" 'gpb-git:unmark-file-command)
    (define-key map "r" 'gpb-git:mark-as-rename)
    (define-key map "v" 'gpb-git:display-staged-changes-buffer)
    (define-key map "q" 'gpb-git:quit-staging-windows)
    (define-key map "\C-c\C-c" 'gpb-git:complete-commit)
    (define-key map (kbd "RET") 'gpb-git:goto-line)
    (fset 'gpb-git:unstaged-changes-mode-map map)
    map)
  "The keymap used for choosing hunks.")


(defvar gpb-git:staged-changes-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'gpb-git:forward-command)
    (define-key map [(backtab)] 'gpb-git:backward-command)
    (define-key map "p" 'gpb-git:backward-command)
    (define-key map "n" 'gpb-git:forward-command)
    (define-key map "x" 'gpb-git:unstage-marked-hunks)
    (define-key map "g" 'gpb-git:refresh-status)
    (define-key map "m" 'gpb-git:mark-hunk-command)
    (define-key map "M" 'gpb-git:mark-file-command)
    (define-key map "u" 'gpb-git:unmark-hunk-command)
    (define-key map "U" 'gpb-git:unmark-file-command)
    (define-key map "v" 'gpb-git:display-unstaged-changes-buffer)
    (define-key map "q" 'gpb-git:quit-staging-windows)
    (define-key map "\C-c\C-c" 'gpb-git:commit)
    (define-key map (kbd "RET") 'gpb-git:goto-line)
    (fset 'gpb-git:staged-changes-mode-map map)
    map)
  "The keymap used for removing and staging hunks.")


;;
;;  Hunk buffer modes
;;

(define-derived-mode gpb-git:hunk-buffer-mode special-mode
  "Hunk Buffer"
  "\nBase mode for buffers showing hunks."
  (setq-local header-line-format '(:eval (gpb-git:compute-header)))
  (setq-local buffer-read-only t)
  (setq-local tab-width 4)
  (add-hook 'post-command-hook 'gpb-git:post-command-hook)
  (add-hook 'kill-buffer-hook 'gpb-git:kill-buffer-hook nil t))


(define-derived-mode gpb-git:unstaged-changes-mode gpb-git:hunk-buffer-mode
  "Unstaged Changes"
  "\nMode for selecting unstages changes to be added to the index.

\\{gpb-git:unstaged-changes-mode-map}\n"
  (setq-local staged-changes-buffer nil))


(define-derived-mode gpb-git:staged-changes-mode gpb-git:hunk-buffer-mode
  "Staged Changes"
  "\nMode for selecting stages changes to be removed from the index.

\\{gpb-git:staged-changes-mode-map}\n"
  (setq-local staged-changes-buffer t))


;;
;;  Primary user functions
;;

(defvar gpb-git:repo-dir-history nil
  "This symbol is used to remember the history of repository roots.")

(defun gpb-git:repo-root-p (dir)
  (file-exists-p (concat (file-name-as-directory dir) ".git")))

(defun gpb-git:read-repo-dir ()
  "Prompt the user for a Git repository directory.

Maintains a separate history list from `read-directory-name'."
  (let* ((dir (or (gpb-git:find-repo-root default-directory)
                  default-directory))
         (file-name-history (copy-list gpb-git:repo-dir-history))
         (repo-dir (read-directory-name "Repo root: " dir dir nil "")))
    (unless (gpb-git:repo-root-p repo-dir)
      (user-error "Invalid Git repo dir: %s" repo-dir))
    (setq gpb-git:repo-dir-history file-name-history)
    repo-dir))


(defun gpb-git:stage-changes (&optional repo-root)
  "Show a buffer for staging changes to the Git index."
  (interactive (list (gpb-git:read-repo-dir)))
  (gpb-git:refresh-status (or repo-root default-directory))
  (gpb-git:switch-to-unstaged-changes-buffer)
  (gpb-git:display-staged-changes-buffer))


(defun gpb-git:unstage-changes (&optional repo-root)
  "Show a buffer for removing changes from the Git index."
  (interactive (list (gpb-git:read-repo-dir)))
  (gpb-git:refresh-status (or repo-root default-directory))
  (gpb-git:switch-to-staged-changes-buffer))


;;
;;  Functions
;;

(defun gpb-git:switch-to-unstaged-changes-buffer ()
  "Switch to the staged changes buffer."
  (interactive)
  (switch-to-buffer gpb-git:unstaged-buffer-name))

(defun gpb-git:display-unstaged-changes-buffer ()
  "Display the unstaged changes buffer in some window."
  (interactive)
  (display-buffer gpb-git:unstaged-buffer-name))

(defun gpb-git:switch-to-staged-changes-buffer ()
  "Switch to the unstaged changes buffer."
  (interactive)
  (switch-to-buffer gpb-git:staged-buffer-name))

(defun gpb-git:display-staged-changes-buffer ()
  "Display the staged changes buffer in some window."
  (interactive)
  (display-buffer gpb-git:staged-buffer-name))


(defun gpb-git:decorate-hunk (hunk &optional focused)
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


(defun gpb-git:find-repo-root (&optional dir)
  "Find the root of the Git repository.
Looks for the .git directory rather than calling Git."
  (let ((dir (file-name-as-directory (or dir default-directory))) next)
    (while (and dir (not (file-exists-p (concat dir ".git"))))
      (setq next (file-name-directory (directory-file-name dir))
            dir (if (string= next dir) nil next)))
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
  (when (= (line-number-at-pos) (point-min)) (user-error "Beginning of buffer"))
  (let ((pt (point)) hunk)
    (forward-line -1)
    (while (and (not (gpb-git:get-current-hunk)) (not (bobp)))
      (forward-line -1))
    (when (bobp)
      (goto-char pt)
      (user-error "No earlier hunks"))
    (setq hunk (gpb-git:get-current-hunk))
    (assert hunk)
    (goto-char (overlay-start hunk))
    (point)))


(defun gpb-git:backward-command ()
  "Move back to the start of the previous hunk or button."
  (interactive)
  (or (ignore-errors (gpb-git:backward-hunk))
      (backward-button 1)))


(defun gpb-git:post-command-hook ()
  "Updates hunk highlighting after each user command."
  ;; If the mark will be deactivated before the next command, we want to
  ;; consider it to already be deactivated when we compute the highlights
  ;; to avoid flicker.
  (let ((mark-active (and mark-active (not deactivate-mark))))
    (gpb-git--update-highlights)))


(defun gpb-git--update-highlights (&optional buf)
  "Updates hunk highlighting.
Implementation detail of `gpb-git:post-command-hook'.  The
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


(defun gpb-git:get-unstaged-changes-buffer (repo-dir)
  "A buffer that displaying unstaged changes."
  (let* ((buf (get-buffer-create gpb-git:unstaged-buffer-name)))
    (with-current-buffer buf
      (gpb-git:unstaged-changes-mode)
      (setq default-directory repo-dir)
      (gpb-git:update-hunks))
    buf))


(defun gpb-git:get-staged-changes-buffer (repo-dir)
  "Create or refresh the buffer that displays staged changes."
  (let* ((buf (get-buffer-create gpb-git:staged-buffer-name)))
    (with-current-buffer buf
      (gpb-git:staged-changes-mode)
      (setq default-directory repo-dir)
      (gpb-git:update-hunks))
    buf))


(defun gpb-git:stage-marked-hunks ()
  "Apply the marked hunks to the index."
  (interactive)
  (let ((patch-file (gpb-git--apply-hunks "apply" "--cached")))
    (delete-file patch-file)))


(defun gpb-git:unstage-marked-hunks ()
  "Remove the marked hunks from the index."
  (interactive)
  (let ((patch-file (gpb-git--apply-hunks "apply" "--cached" "-R")))
    (delete-file patch-file)))


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


(defun gpb-git--apply-hunks (&rest args)
  "Apply marked hunks using ARGS.

Creates a new patch file in `default-directory' and returns the
name of this file."
  (interactive)
  (let* ((patch-file (make-temp-file (concat default-directory "gpb-git-")
                                     nil ".patch"))
         (localname (file-relative-name patch-file))
         (proc-output-buf gpb-git:process-output-buffer-name)
         (args (append args `(,localname))))

    (with-current-buffer (gpb-git:make-patch (member "-R" args))
      (let ((coding-system-for-write 'unix))
        (write-region (point-min) (point-max) patch-file)))

    (with-current-buffer (get-buffer-create proc-output-buf)
      (erase-buffer)
      (insert (format "git %s\n" (mapconcat 'identity args " "))))

    (setq retvalue (apply 'process-file "git" nil proc-output-buf t args))

    (if (= retvalue 0)
        ;; We successfully applied the patch.
        (progn
          (gpb-git:refresh-status)
          (pop-to-buffer gpb-git:staged-buffer-name))

      ;; If the application failed, we pop to the process output.
      (pop-to-buffer proc-output-buf))
    patch-file))


(defun gpb-git:refresh-hunk-buffers ()
  "Refresh the buffers displaying staged and unstaged changes."
  (interactive)
  (with-current-buffer gpb-git:unstaged-buffer-name
    (gpb-git:update-hunks))
  (with-current-buffer gpb-git:staged-buffer-name
    (gpb-git:update-hunks)))


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
                   staged-changes-buffer))
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
         (marked-hunks (gpb-git:get-marked-hunks))
         current-output-file)

    (when (= (length marked-hunks) 0)
      (user-error "No hunks have been marked"))

    (with-current-buffer patch-buf (erase-buffer))
    (dolist (hunk marked-hunks)
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
    (gpb-git:decorate-hunk hunk)
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
         (proc-buf (get-buffer-create gpb-git:process-output-buffer-name))
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

    (gpb-git:refresh-status)
    (switch-to-buffer proc-buf)))


(defun gpb-git:show-faces ()
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

    (pop-to-buffer (current-buffer)))))


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


(defun gpb-git:exec-async (cmd dir callback &rest args)
  "Execute an asyncronous command and then call CALLBACK.

CMD is a list of strings that is passed through to
`start-file-process' to execute an asynchronous command and the
output is written into the new temporary buffer.

CALLBACK is a function that will (eventually) be called in the
buffer where this command was called when the asycnronous command
finishes.  If that buffer is no longer alive when the subprocess
exits, the CALLBACK will not be called.  The CALLBACK function is
passed the buffer that contains the process output followed by
any additional arguments in ARGS."
  (gpb-git--trace-funcall #'gpb-git:exec-async `(,cmd ,dir ,callback ,@args))
  (let ((buf (current-buffer)) proc)
    (with-current-buffer (gpb-git--get-new-buffer "*exec-async" "*")
      ;; Set variables for `gpb-git:exec-async--process-sentinel'.
      (setq-local default-directory dir)
      (setq-local callback-func callback)
      (setq-local callback-args args)
      (setq-local callback-buf buf)
      (setq proc (apply 'start-file-process (car cmd) (current-buffer) cmd))
      (set-process-sentinel proc 'gpb-git:exec-async--process-sentinel)
      (message "Started process %S in %S" cmd (current-buffer)))))


(defun gpb-git:exec-async--process-sentinel (proc change)
  "Process sentinel used handle asyncronous Git commands."
  (gpb-git--trace-funcall #'gpb-git:exec-async--process-sentinel
                          `(,proc ,change))
  (when (eq 'exit (process-status proc))
    (let (func args buf)
      (message "gpb-git:exec-async--process-sentinel(1) %S"
               (process-buffer proc))
      (with-current-buffer (process-buffer proc)
        (setq func callback-func
              args (cons (current-buffer) callback-args)
              buf callback-buf))
      ;; Evaluate `func' in the buffer where `gpb-git:exec-async' was
      ;; initially called.
      (message "gpb-git:exec-async--process-sentinel(2) %S" buf)
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (let ((debug-on-error t))
            (gpb-git--trace-funcall func args)
            (apply func args))))

      (kill-buffer (process-buffer proc)))))


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


(defun gpb-git--get-new-buffer (prefix suffix)
  "Get a new buffer whose name starts PREFIX with and ends with SUFFIX."
  (if (null (get-buffer (concat prefix suffix)))
      (get-buffer-create (concat prefix suffix))
    (let ((i 2))
      (while (get-buffer (concat prefix "<" (int-to-string i) ">" suffix))
        (incf i))
      (get-buffer-create (concat prefix "<" (int-to-string i) ">" suffix)))))


(defmacro gpb-git--with-blocking-callback (callback-symbol &rest body)
  "Run BLOCK and wait it for it to call CALLBACK-SYMBOL.

This macro defines a lambda function with one parameter and binds
it to CALLBACK-SYMBOL, executes BLOCK, and then performs a busy
wait until CALLBACK-SYMBOL has been called.  The form produced
returns the value passed to CALLBACK-SYMBOL.

BLOCK should arrange for CALLBACK-SYMBOL to be called at some
point in the future.

Example:

  (gpb-git--with-blocking-callback f
    (run-at-time 2 nil f \"test\"))

Notice that the function is stored in the variable slot of `f'."
  (declare (indent 1) (debug t))
  (let ((retval-symbol (gensym "retval-")))
    `(let ((,retval-symbol nil)
           (,callback-symbol (lambda (x)
                                (setq ,retval-symbol (list x)))))
       ,@body
       (while (null ,retval-symbol)
         (sit-for 0.3))
       (car ,retval-symbol))))


(defun gpb-git--trace-funcall (func args)
  "Write tracing output to buffer."
  (let ((buf (current-buffer))
        (bufname "*trace gpb-git:exec-async*"))
    (unless (get-buffer bufname)
      (with-current-buffer (get-buffer-create bufname)
        (setq truncate-lines t)))
    (with-current-buffer (get-buffer bufname)
      (let ((args-string (mapconcat (lambda (y)
                                      (truncate-string-to-width
                                       (prin1-to-string y) 1000 nil nil t))
                                    args "\n  ")))
        (save-excursion
          (goto-char (point-max))
          (unless (bobp) (insert "\n"))
          (insert (format "%S in %S:\n  %s\n" func buf args-string)))))))


(defun gpb-git:insert-spinner ()
  "Insert spinner at current point."
  (let ((m (copy-marker (point))))
    (set-marker-insertion-type m nil)
    (insert (propertize "|" 'spinner t 'sequence '("/" "-" "\\" "|")))
    (set-marker-insertion-type m t)
    (run-at-time 0.5 nil 'gpb-git:insert-spinner--spin m)))

(defun gpb-git:insert-spinner--spin (m)
  "Implementation detail of `gpb-git:insert-spinner'"
  (with-current-buffer (marker-buffer m)
    (when (ignore-errors (get-text-property m 'spinner))
      (let* ((seq (get-text-property m 'sequence))
             (next-seq (append (cdr seq) (list (car seq))))
             (inhibit-read-only t)
             props)
        (save-excursion
          (goto-char m)
          (setq props (text-properties-at m))
          (plist-put props 'sequence next-seq)
          (set-marker-insertion-type m nil)
          (insert (apply 'propertize (car seq) props))
          (set-marker-insertion-type m t)
          (delete-region (+ m 1) (+ m 2))))
      (run-at-time 0.5 nil 'gpb-git:insert-spinner--spin m))))


(defvar gpb-git:windows-status-script "

@echo off

git -c advice.statusHints=false status

echo.
echo ***unstaged changes***
echo.

git diff-files --patch --histogram --find-renames

for /f \"delims=\" %%p in ('git ls-files --others --exclude-standard') do git diff --no-index --histogram -- /dev/null \"%%p\"

echo.
echo ***staged changes***
echo.

git diff-index --cached --find-renames --patch --histogram HEAD
" "The status update script for use on Windows")


(defvar gpb-git:bash-status-script "

git -c advice.statusHints=false status

echo
echo \"***unstaged changes***\"
echo

git diff-files --patch --histogram --find-renames

for i in $( git ls-files --others --exclude-standard ); do
    git diff --no-index --histogram -- /dev/null $i
done

echo
echo \"***staged changes***\"
echo

git diff-index --cached --find-renames --patch --histogram HEAD

" "The status update script for use with bash")


(defun gpb-git:use-windows-script-p (&optional repo-dir)
  ;; Use the Windows script when working locally on a Windows machine.
  (and (null (file-remote-p (or repo-dir default-directory)))
       (eq system-type 'windows-nt)))


(defun gpb-git:get-status-script (&optional where force)
  "Get the name of the script used for producing status updates.

If FORCE is non-nil, we always write a new file."
  (let* ((default-directory (or where default-directory))
         (remote (file-remote-p default-directory))
         (cache-val (and (not force)
                         (cdr (assoc remote gpb-git:status-script-cache)))))
    (if cache-val
        cache-val
      (let* ((make-file (or (and (fboundp 'make-nearby-temp-file)
                                 'make-nearby-temp-file)
                            'make-temp-file))
             (filename (cond
                        ((gpb-git:use-windows-script-p)
                         (funcall make-file "git-status-" nil ".cmd"))
                        (t
                         (funcall make-file "git-status-" nil ".bash")))))
        (with-temp-file filename
          (cond
           ((gpb-git:use-windows-script-p)
            (insert gpb-git:windows-status-script))
           (t
            (setq coding-system-for-write 'unix)
            (insert gpb-git:bash-status-script)))
          (goto-char (point-min))
          (skip-chars-forward "\n ")
          (delete-region (point-min) (point)))
        (aput 'gpb-git:status-script-cache remote
              (or (file-remote-p filename 'localname) filename))
        filename))))


(defun gpb-git--parse-diff (&optional beg end)
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
          (gpb-git:decorate-hunk ov))))
    (setq-local hunks-available t)))


(defun gpb-git:refresh-status (&optional repo-dir)
  "Refresh the buffers showing staged and unstaged changes.
Updates the buffers `gpb-git:unstaged-buffer-name' and
`gpb-git:staged-buffer-name'."
  (interactive)
  (let* ((repo-dir (or repo-dir default-directory))
         (remote (file-remote-p repo-dir))
         (filename (gpb-git:get-status-script repo-dir))
         (unstaged-buf (get-buffer-create gpb-git:unstaged-buffer-name))
         (staged-buf (get-buffer-create gpb-git:staged-buffer-name))
         (cmd (if (gpb-git:use-windows-script-p repo-dir)
                  `("cmd" "/c" ,filename) `("bash" ,filename)))
         (inhibit-read-only t)
         status-text)

    (with-temp-buffer
      (setq default-directory repo-dir)
      (process-file "git" nil t nil "-c" "advice.statusHints=false" "status")
      (goto-char (point-max))
      (skip-chars-backward " \n")
      (setq status-text (buffer-substring (point-min) (point))))

    (with-current-buffer unstaged-buf
      (erase-buffer)
      (dolist (ov (gpb-git--get-hunk-overlays)) (delete-overlay ov))
      (gpb-git:unstaged-changes-mode)
      (setq default-directory repo-dir)
      (save-excursion
        (insert (format "\nUnstaged changes in %s\n\n"
                        (gpb-git:abbreviate-file-name default-directory)))
        (insert status-text)
        (insert "\n")
        (add-text-properties
         (point)
         (save-excursion
           (insert "Loading hunks ")
           (gpb-git:insert-spinner)
           (point))
         '(face (background-color . "light gray")))
        (insert "\n")
        (goto-char (point-min))
        (when (re-search-forward "^Changes to be committed:" nil t)
          (delete-region (match-beginning 0)
                         (progn
                           (forward-line 1)
                           (while (looking-at-p "^\t") (forward-line 1))
                           (skip-chars-forward " \t\n")
                           (point))))
        ;; Delete the sections related to staged changes.
        (when (re-search-forward "^no changes added" nil t)
          (delete-region (match-beginning 0)
                         (progn (forward-line 1) (point))))))


    (with-current-buffer staged-buf
      (erase-buffer)
      (dolist (ov (gpb-git--get-hunk-overlays)) (delete-overlay ov))
      (gpb-git:staged-changes-mode)
      (setq default-directory repo-dir)
      (save-excursion
        (insert (format "\nStaged changes in %s\n\n"
                        (gpb-git:abbreviate-file-name default-directory)))
        (insert status-text)
        (insert "\n\n")
        (unless (save-excursion
                  (re-search-backward "^no changes added to commit" nil t))
          (add-text-properties
           (point)
           (save-excursion
             (insert "Loading hunks ")
             (gpb-git:insert-spinner)
             (point))
           '(face (background-color . "light gray"))))
        (goto-char (point-min))
        (when (re-search-forward "^On branch")
          (delete-region (match-beginning 0)
                         (progn (forward-line 1)
                                (skip-chars-forward " \t\n")
                                (point))))
        (when (re-search-forward "^Your branch is ahead of" nil t)
          (delete-region (match-beginning 0)
                         (progn
                           (forward-line 1)
                           (while (looking-at-p "^$") (forward-line 1))
                           (point))))
        (when (re-search-forward "^Changes not staged for commit:" nil t)
          (delete-region (match-beginning 0)
                         (progn
                           (forward-line 1)
                           (while (looking-at-p "^\t") (forward-line 1))
                           (forward-line 1)
                           (point))))
        (when (re-search-forward "^Untracked files:$" nil t)
          (delete-region (match-beginning 0)
                         (progn
                           (forward-line 1)
                           (while (looking-at-p "^\t") (forward-line 1))
                           (forward-line 1)
                           (point))))))

    (gpb-git:exec-async cmd repo-dir #'gpb-git:refresh-status-1)))


(defun gpb-git:refresh-status-1 (buf)
  (let ((repo-dir (gpb-git:find-repo-root))
        (unstaged-buf (get-buffer-create gpb-git:unstaged-buffer-name))
        (staged-buf (get-buffer-create gpb-git:staged-buffer-name))
        (inhibit-read-only t)
        status-text unstaged-hunks staged-hunks)

    (with-current-buffer buf
      (goto-char (point-min))
      (when (re-search-forward "No such file or directory"
                               (save-excursion (forward-line 1) (point)) t)
        (let ((filename (gpb-git:get-status-script)))
          (if (null (file-exists-p (concat (or (file-remote-p default-directory) "")
                                           filename)))
              (progn
                ;; The remote status script has been deleted, so we write a
                ;; new remote status script and try again.
                (gpb-git:get-status-script nil t)
                (gpb-git:refresh-status)
                (error "Remote script was missing."))
            (pop-to-buffer buf)
            (error "Error running status script."))))

      (setq status-text (buffer-substring
                         (point-min)
                         (progn (re-search-forward
                                 "^\\*\\*\\*unstaged changes\\*\\*\\*$")
                                (match-beginning 0))))
      (forward-line 1)
      (setq unstaged-hunks (gpb-git--parse-diff
                            (point)
                            (progn (re-search-forward
                                    "^\\*\\*\\*staged changes\\*\\*\\*$")
                                   (match-beginning 0))))
      (forward-line 1)
      (setq staged-hunks (gpb-git--parse-diff (point))))

    (with-current-buffer unstaged-buf
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "Loading hunks" nil t)
          (delete-region (match-beginning 0)
                         (progn (forward-line 1) (point)))
          (gpb-git--insert-hunks unstaged-hunks)
          (gpb-git--add-unstaged-hunk-links))))

    (with-current-buffer staged-buf
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "Loading hunks" nil t)
          (delete-region (match-beginning 0) (progn (forward-line 1) (point)))
          (gpb-git--insert-hunks staged-hunks)
          (gpb-git--add-staged-hunk-links))))))


(defun gpb-git--add-unstaged-hunk-links ()
  (save-excursion
    (goto-char (point-min))

    ;; Add links from the modified files to the hunks
    (while (re-search-forward "^\t\\(modified:\\|deleted:\\)" nil t)
      (skip-chars-forward " \t")
      (let* ((filename (buffer-substring-no-properties
                        (point) (save-excursion (end-of-line) (point))))
             (hunks (gpb-git--get-hunk-overlays filename))
             (hunk-desc (cond
                         ((= (length hunks) 1) "1 hunk")
                         (t (format "%s hunks" (length hunks))))))
        (make-text-button (point) (progn (end-of-line) (point))
                          'action 'gpb-git:jump-to-file-hunks
                          'filename filename)
        (insert (format " (%s)" hunk-desc))))

    ;; Add links from untracked files to the hunks.
    (when (re-search-forward "^Untracked files:\n" nil t)
      (while (looking-at "^\t[^\t]")
        (skip-chars-forward "\t")
        (let* ((filename (buffer-substring-no-properties
                          (point) (save-excursion (end-of-line) (point)))))
          (make-text-button (point) (progn (end-of-line) (point))
                            'action 'gpb-git:jump-to-file-hunks
                            'filename filename)
          (forward-line 1))))))


(defun gpb-git--add-staged-hunk-links ()
  "Add links from \"Changes to be committed\" to hunk."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^Changes to be committed:\n" nil t)
      (while (looking-at "^\t\\(modified\\|deleted\\|new file\\):")
        (goto-char (match-end 0))
        (skip-chars-forward " \t")
        (let* ((match-string (match-string 1))
               (filename (buffer-substring-no-properties
                          (point) (save-excursion (end-of-line) (point))))
               (hunks (gpb-git--get-hunk-overlays filename))
               (hunk-desc (cond
                           ((= (length hunks) 1) "1 hunk")
                           (t (format "%s hunks" (length hunks))))))
          (make-text-button (point) (progn (end-of-line) (point))
                            'action 'gpb-git:jump-to-file-hunks
                            'filename filename)
          (when (string= match-string "modified")
            (insert (format " (%s)" hunk-desc)))
          (forward-line 1))))))


(defun gpb-git:quit-staging-windows ()
  (interactive)
  (let ((win-list (append (get-buffer-window-list gpb-git:unstaged-buffer-name)
                          (get-buffer-window-list gpb-git:staged-buffer-name))))
    (while win-list
      (dolist (win win-list)
        (quit-window nil win)
        (setq win-list
              (append
               (get-buffer-window-list gpb-git:unstaged-buffer-name)
               (get-buffer-window-list gpb-git:staged-buffer-name)))))))


(defcustom gpb-git:remote-home-dirs nil
  "A list of strings giving remote directory paths.

Each directory should have TRAMP-prefix."
  :type '(repeat directory))


(defun gpb-git:abbreviate-file-name (dir)
  (dolist (remote-dir gpb-git:remote-home-dirs)
    (when (string-prefix-p remote-dir dir)
      (assert (file-remote-p remote-dir))
      (setq dir (replace-regexp-in-string (regexp-quote remote-dir)
                                          (file-remote-p remote-dir)
                                          dir))))
  dir)


(provide 'gpb-git)
