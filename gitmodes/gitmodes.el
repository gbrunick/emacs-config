;;
;;  An Emacs Git Interface
;;
;;  Due to a bug/missing feature in TRAMP, `magit' cannot commit hunks to a
;;  Git reposity on a remote machine over TRAMP from a Windows machine.
;;  This package works in this case because we write files rather than
;;  trying to pipe patchs into the stdin of a Git process.  See
;;  https://github.com/magit/magit/issues/3624 and
;;  http://lists.gnu.org/archive/html/tramp-devel/2018-11/msg00020.html
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
;;  The function `gpb-git--refresh-changes' calls `gpb-git--parse-diff' to
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
(require 'gm-shell-commands)

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
  '((t :background "#eeeeee" :inherit default :extend t))
  "Face used for coments and instructions")

(defface gpb-git:file-name
  '((t :background "#ffffff" :height 140 :extend t))
  "Face used for the file name header")

(defface gpb-git:file-header '((t :background "grey95" :extend t))
  "Face used for the hunk header in the pathc buffer")

(defface gpb-git:hunk-header
  '((t :background "gray65" :extend t))
  "Face used for the hunk header")

(defface gpb-git:context-line
  '((t :background "gray95" :extend t))
  "Face used for context lines in a hunk")

(defface gpb-git:deleted-line
  `((t :foreground "#550000"
       :background ,(gpb-git--blend-colors "#f0c0c0" "white" 0.6)
       :extend t))
  "Face used for the deleted lines in a hunk")

(defface gpb-git:added-line
  `((t :foreground "#004400"
       :background ,(gpb-git--blend-colors "#b8e0b8" "white" 0.6)
       :extend t))
  "Face used for the added lines in a hunk")

;; Focused faces

(defface gpb-git:focused-hunk-header
  '((t :background "cornflower blue" :extend t))
  "Face used for context lines in the focused hunk")

(defface gpb-git:focused-context-line
  `((t :background ,(gpb-git--blend-colors "cornflower blue" "white" 0.25)
       :extend t))
  "Face used for context lines in the focused hunk")

(defface gpb-git:focused-added-line
  '((t :foreground "#005500"
       :background "#b8e0b8"
       :extend t))
  "Face used for the added lines in the focused hunk.")

(defface gpb-git:focused-deleted-line
  '((t :foreground "#880000"
       :background "#f0c0c0"
       :extend t))
  "Face used for deleted lines in the focused hunk.")

;; Marked faces

(defface gpb-git:marked-hunk-header
  `((t :background ,(gpb-git--blend-colors "khaki4" "white" 0.6)
       :extend t))
  "Face used for context lines in a marked hunk")

(defface gpb-git:marked-context-line
  `((t :background ,(gpb-git--blend-colors "khaki2" "white" 0.6)
       :extend t))
  "Face used for context lines in the marked hunk")

(defface gpb-git:marked-added-line
  `((t :background ,(gpb-git--blend-colors "khaki2" "white" 0.8)
       :extend t))
  "Face used for the added lines in the marked hunk.")

(defface gpb-git:marked-deleted-line
  `((t :background ,(gpb-git--blend-colors "khaki3" "white" 0.6)
       :extend t))
  "Face used for deleted lines in the marked hunk.")

;; Focused and marked faces

(defface gpb-git:focused-and-marked-hunk-header
  '((t :background "khaki4" :extend t))
  "Face used for context lines in a marked hunk")

(defface gpb-git:focused-and-marked-context-line
  `((t :background ,(gpb-git--blend-colors "khaki2" "white" 0.72 0.22)
       :extend t))
  "Face used for context lines in the marked hunk")

(defface gpb-git:focused-and-marked-added-line
  `((t ;; :foreground "#003000"
       :background ,(gpb-git--blend-colors "khaki2" "black" 0.95)
       :extend t))
  "Face used for the added lines in the marked hunk.")

(defface gpb-git:focused-and-marked-deleted-line
  `((t ;; :foreground "#660000"
       :background ,(gpb-git--blend-colors "#f0c0c0" "khaki3" 0)
       :extend t))
  "Face used for deleted lines in the marked hunk.")


(defface gpb-git:marked-line-face
  `((t :background ,(gpb-git--blend-colors "khaki2" "white" 0.72 0.22)
       :extend t))
  "Face used for the marked revision in a log buffer.")


;;
;;  Keymaps
;;


(defvar gpb-git:user-command-prefix-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "s" 'gpb-git:show-status)
    (define-key map "c" 'gpb-git:commit)
    (define-key map "l" 'gpb-git:show-commit-graph)
    (define-key map "!" 'gpb-git:shell-command)
    (define-key map "p" 'gpb-git:push-changes)
    (fset 'gpb-git:user-command-prefix-keymap map)
    map)
  "The prefix keymap for user commands.

Bind to this to a prefix of your choosing (e.g., \"\C-cv\")")


;;
;;  Functions
;;


(defun gpb-git:refresh-buffer ()
  "Implements the standard refresh on g behaviour.

User-facing; attempts to preserve window position."
  (interactive)
  (let* ((buf (current-buffer))
         (window (selected-window))
         (window-buf (window-buffer window))
         (ws (window-start))
         (pt (point))
         (reset-window `(lambda (&rest args)
                          (goto-char (min ,pt (point-max)))
                          (set-window-start ,window
                                            (min ,ws (point-max)))
                          (forward-line 0)
                          (gpb-git--post-command-hook))))
    (assert (equal buf window-buf))
    (message "gpb-git:refresh-buffer: %s %s" (current-buffer) major-mode)
    (eval `(,@refresh-cmd reset-window))))


(defun gpb-git--reload-all ()
  "Reload all source files."
  (load "gm-util.el")
  (load "gm-status.el")
  (load "gm-logs.el")
  (load "gm-hunks.el")
  (load "gm-shell-commands.el")
  (load "gitmodes.el"))

(provide 'gitmodes)
