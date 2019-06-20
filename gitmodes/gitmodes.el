;;
;;  Modes for working with Git output
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
;;  calls `gpb-git:stage-hunks' to construct a patch from the
;;  selected hunks and apply it to the Git index.
;;
;;  Similarly, we use `diff-index --cached` to find the changes in the
;;  index relative to HEAD, place these hunks in the buffer
;;  `gpb-git:staged-buffer-name', and give the user an opportunity mark
;;  hunks and portions of hunks there as well.  When done, the user may
;;  call `gpb-git:unstage-hunks' to remove existing changes from the
;;  index.  In this case, we create the patch, but apply it in reverse.
;;
;;  The primary complex data structure used is a hunk alist that contains
;;  information from the Git diff output about a single unit of change.
;;  The function `gpb-git--parse-diff' returns a list of such alists.
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
;;  The function `gpb-git:refresh-status' calls `gpb-git--parse-diff' to
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

(require 'gm-util)
(require 'gm-status)
(require 'gm-logs)
(require 'gm-hunks)

(defcustom gpb-git:remote-home-dirs nil
  "A list of strings giving remote directory paths.

Each directory should have TRAMP-prefix.  This variable is used
by `gpb-git--abbreviate-file-name' to give better directory
names in the UI."
  :type '(repeat directory) :group 'gitmodes)


(defcustom gpb-git:status-buffer-name "*git status*"
  "The name of the buffer used to show Git status."
  :type 'string :group 'gitmodes)

(defcustom gpb-git:unstaged-buffer-name "*unstaged changes*"
  "The name of the buffer used to show staged changes."
  :type 'string :group 'gitmodes)

(defcustom gpb-git:staged-buffer-name "*staged changes*"
  "The name of the buffer used to show unstaged changes."
  :type 'string :group 'gitmodes)

(defcustom gpb-git:patch-buffer-name "*git patch*"
  "The name of the temporary buffer used to construct patches."
  :type 'string :group 'gitmodes)

(defcustom gpb-git:process-output-buffer-name "*git output*"
  "The name of the buffer used to display Git output."
  :type 'string :group 'gitmodes)

(defcustom gpb-git:commit-message-buffer-name "*commit message*"
  "The name of the temporary buffer used to edit commit messages."
  :type 'string :group 'gitmodes)


(defvar gpb-git:currently-focused-hunk nil
  "Tracks the currently focused hunk (see `gpb-git--update-highlights').")

(defvar gpb-git:commit-messages nil
  "We save all commit messages so they can be recovered.")

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


(defface gpb-git:marked-line-face
  `((t :background ,(gpb-git--blend-colors "khaki2" "white" 0.72 0.22)))
  "Face used for the marked revision in a log buffer.")


;;
;;  Keymaps
;;


(defvar gpb-git:user-command-prefix-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "s" 'gpb-git:show-status)
    (define-key map "c" 'gpb-git:commit)
    (define-key map "g" 'gpb-git:show-commit-graph)
    (fset 'gpb-git:user-command-prefix-keymap map)
    map)
  "The prefix keymap for user commands.

Bind to this to a prefix of your choosing (e.g., \"\C-cv\")")


;;
;;  Buffer modes
;;


(define-derived-mode gpb-git:commit-message-mode diff-mode "Git Commit"
  "Mode for editing a Git commit message and completing the commit."
  (local-set-key "\C-c\C-c" 'gpb-git:complete-commit))




;;
;;  Primary user functions
;;


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



;;
;;  Functions
;;


(defun gpb-git--post-command-hook ()
  "Updates hunk highlighting after each user command."
  (when (derived-mode-p 'gpb-git:hunk-selection-mode)
    ;; If the mark will be deactivated before the next command, we want to
    ;; consider it to already be deactivated when we compute the highlights
    ;; to avoid flicker.
    (let ((mark-active (and mark-active (not deactivate-mark))))
      (gpb-git--update-highlights))))


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

    ;; Show the commit command output.
    (gpb-git:show-status--refresh)
    (switch-to-buffer proc-buf)))


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
      (let ((inhibit-message t))
        (message "Started process %S in %S" cmd (current-buffer))))))


(defun gpb-git:exec-async--process-sentinel (proc change)
  "Process sentinel used handle asyncronous Git commands."
  (gpb-git--trace-funcall #'gpb-git:exec-async--process-sentinel
                          `(,proc ,change))
  (when (eq 'exit (process-status proc))
    (let (func args buf)
      ;; (message "gpb-git:exec-async--process-sentinel(1) %S"
      ;;          (process-buffer proc))
      (with-current-buffer (process-buffer proc)
        (setq func callback-func
              args (cons (current-buffer) callback-args)
              buf callback-buf))
      ;; Evaluate `func' in the buffer where `gpb-git:exec-async' was
      ;; initially called.
      ;; (message "gpb-git:exec-async--process-sentinel(2) %S" buf)
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (let ((debug-on-error t))
            (gpb-git--trace-funcall func args)
            (apply func args))))

      (kill-buffer (process-buffer proc)))))


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


(defun gpb-git:refresh-buffer ()
  "Implements the standard refresh on g behaviour.

User-facing; attempts to preserve window position."
  (interactive)
  (let* ((buf (current-buffer))
         (window (selected-window))
         (window-buf (window-buffer window))
         (ws (window-start))
         (pt (point))
         (reset-window `(lambda ()
                          (goto-char (min ,pt (point-max)))
                          (set-window-start ,window
                                            (min ,ws (point-max)))
                          (forward-line 0)
                          (gpb-git--post-command-hook))))
    (assert (equal buf window-buf))
    (message "gpb-git:refresh-buffer: %s %s" (current-buffer) major-mode)
    (eval `(,@refresh-cmd reset-window))))



(provide 'gitmodes)