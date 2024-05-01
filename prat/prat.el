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
;;  The function `prat-parse-diff' returns a list of such alists.
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
;;  The function `prat-refresh-changes' calls `prat-parse-diff' to
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
;;        marked as a rename (see `prat-mark-as-rename'), the value is a
;;        cons cell of the form (:rename . filename).
;;

(require 'prat-util)
(require 'prat-status)
(require 'prat-logs)
(require 'prat-hunks)
(require 'prat-shell-commands)
(require 'prat-editor)

(defvar prat-debug t
  "When non-nil, we write debug info into `prat-debug-buffer-name'.")

(defvar prat-debug-buffer-name "*Prat Debug*"
  "The name of the buffer used to hold debug information.")

(defcustom prat-remote-home-dirs nil
  "A list of strings giving remote directory paths.

Each directory should have TRAMP-prefix.  This variable is used
by `prat-abbreviate-file-name' to give better directory
names in the UI."
  :type '(repeat directory) :group 'gitmodes)


(defcustom prat-status-buffer-name "*git status*"
  "The name of the buffer used to show Git status."
  :type 'string :group 'gitmodes)

(defcustom prat-unstaged-buffer-name "*unstaged changes*"
  "The name of the buffer used to show staged changes."
  :type 'string :group 'gitmodes)

(defcustom prat-staged-buffer-name "*staged changes*"
  "The name of the buffer used to show unstaged changes."
  :type 'string :group 'gitmodes)

(defcustom prat-patch-buffer-name "*git patch*"
  "The name of the temporary buffer used to construct patches."
  :type 'string :group 'gitmodes)

(defcustom prat-process-output-buffer-name "*git output*"
  "The name of the buffer used to display Git output."
  :type 'string :group 'gitmodes)

(defcustom prat-commit-message-buffer-name "*commit message*"
  "The name of the temporary buffer used to edit commit messages."
  :type 'string :group 'gitmodes)


(defvar prat-currently-focused-hunk nil
  "Tracks the currently focused hunk (see `prat-update-highlights').")

(defvar prat-commit-messages nil
  "We save all commit messages so they can be recovered.")

(defface prat-title
  '((t :background "#eeeeee" :inherit default :extend t))
  "Face used for coments and instructions")

(defface prat-file-name
  '((t :background "#ffffff" :height 140 :extend t))
  "Face used for the file name header")

(defface prat-file-header '((t :background "grey95" :extend t))
  "Face used for the hunk header in the pathc buffer")

(defface prat-hunk-header
  '((t :foreground "#000000"
       :background "gray65" :extend t))
  "Face used for the hunk header")

(defface prat-context-line
  '((t :foreground "#000000"
       :background "gray95" :extend t))
  "Face used for context lines in a hunk")

(defface prat-deleted-line
  `((t :foreground "#550000"
       :background ,(prat-blend-colors "#f0c0c0" "white" 0.6)
       :extend t))
  "Face used for the deleted lines in a hunk")

(defface prat-added-line
  `((t :foreground "#004400"
       :background ,(prat-blend-colors "#b8e0b8" "white" 0.6)
       :extend t))
  "Face used for the added lines in a hunk")

;; Focused faces

(defface prat-focused-hunk-header
  '((t :foreground "#000000"
       :background "cornflower blue" :extend t))
  "Face used for context lines in the focused hunk")

(defface prat-focused-context-line
  `((t :foreground "#000000"
       :background ,(prat-blend-colors "cornflower blue" "white" 0.25)
       :extend t))
  "Face used for context lines in the focused hunk")

(defface prat-focused-added-line
  '((t :foreground "#005500"
       :background "#b8e0b8"
       :extend t))
  "Face used for the added lines in the focused hunk.")

(defface prat-focused-deleted-line
  '((t :foreground "#880000"
       :background "#f0c0c0"
       :extend t))
  "Face used for deleted lines in the focused hunk.")

;; Marked faces

(defface prat-marked-hunk-header
  `((t :foreground "#000000"
       :background ,(prat-blend-colors "khaki4" "white" 0.6)
       :extend t))
  "Face used for context lines in a marked hunk")

(defface prat-marked-context-line
  `((t :foreground "#000000"
       :background ,(prat-blend-colors "khaki2" "white" 0.6)
       :extend t))
  "Face used for context lines in the marked hunk")

(defface prat-marked-added-line
  `((t :foreground "#000000"
       :background ,(prat-blend-colors "khaki2" "white" 0.8)
       :extend t))
  "Face used for the added lines in the marked hunk.")

(defface prat-marked-deleted-line
  `((t :foreground "#000000"
       :background ,(prat-blend-colors "khaki3" "white" 0.6)
       :extend t))
  "Face used for deleted lines in the marked hunk.")

;; Focused and marked faces

(defface prat-focused-and-marked-hunk-header
  '((t :background "khaki4" :extend t))
  "Face used for context lines in a marked hunk")

(defface prat-focused-and-marked-context-line
  `((t :foreground "#000000"
       :background ,(prat-blend-colors "khaki2" "white" 0.72 0.22)
       :extend t))
  "Face used for context lines in the marked hunk")

(defface prat-focused-and-marked-added-line
  `((t ;; :foreground "#003000"
       :foreground "#000000"
       :background ,(prat-blend-colors "khaki2" "black" 0.95)
       :extend t))
  "Face used for the added lines in the marked hunk.")

(defface prat-focused-and-marked-deleted-line
  `((t ;; :foreground "#660000"
       :foreground "#000000"
       :background ,(prat-blend-colors "#f0c0c0" "khaki3" 0)
       :extend t))
  "Face used for deleted lines in the marked hunk.")


(defface prat-marked-line-face
  `((t :foreground "#000000"
       :background ,(prat-blend-colors "khaki2" "white" 0.72 0.22)
       :extend t))
  "Face used for the marked revision in a log buffer.")


;;
;;  Keymaps
;;


(defvar prat-user-command-prefix-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "s" 'prat-show-status)
    (define-key map "c" 'prat-commit)
    (define-key map "r" 'prat-rebase)
    (define-key map "l" 'prat-show-commit-graph)
    (define-key map "!" 'prat-shell-command)
    (define-key map "p" 'prat-push-changes)
    (fset 'prat-user-command-prefix-keymap map)
    map)
  "The prefix keymap for user commands.

Bind to this to a prefix of your choosing (e.g., \"\C-cv\")")


;;
;;  Functions
;;


(defun prat-push-changes ()
  (interactive)
  (let ((cmd (read-string "Git Shell Command: " "git push -u origin HEAD")))
    (prat-shell-command cmd)))

(defun prat-refresh-buffer ()
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
                          (prat-post-command-hook))))
    (cl-assert (equal buf window-buf))
    (message "prat-refresh-buffer: %s %s" (current-buffer) major-mode)
    (eval `(,@refresh-cmd reset-window))))


(defun prat-reload-all ()
  "Reload all source files."
  (load "prat-util.el")
  (load "prat-status.el")
  (load "prat-logs.el")
  (load "prat-hunks.el")
  (load "prat-shell-commands.el")
  (load "prat-editor.el")
  (load "prat.el"))

(provide 'prat)
