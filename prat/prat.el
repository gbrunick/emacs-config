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

(require 'prat-util)
(require 'prat-status)
(require 'prat-logs)
(require 'prat-hunks)
(require 'prat-shell-command)
(require 'prat-change-buffers)
(require 'prat-stash)
(require 'prat-tree)

(defcustom prat-unstaged-buffer-name "*unstaged changes*"
  "The name of the buffer used to show staged changes."
  :type 'string :group 'prat)

(defcustom prat-staged-buffer-name "*staged changes*"
  "The name of the buffer used to show unstaged changes."
  :type 'string :group 'prat)

(defcustom prat-patch-buffer-name "*git patch*"
  "The name of the temporary buffer used to construct patches."
  :type 'string :group 'prat)

(defcustom prat-extra-git-config '("advice.statusHints=false"
                                   "advice.waitingForEditor=false")
  "Extra configuration setting for Git.  Mainly used to disable hints.
Passed to Git using initial -c arguments."
  :type '(repeat string) :group 'prat)

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
  `((t :inherit region))
  "Face used for marked files in status buffer.")


(defvar prat-base-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "!" 'prat-shell-command)
    map)
  "The keymap for `prat-base-mode'")

(define-derived-mode prat-base-mode special-mode
  "Prat Base Mode"
  "\nBase mode for all buffers managed by the `prat' package."
  (setq-local buffer-read-only t
              tab-width 4
              truncate-lines t))


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
    (define-key map "p" 'prat-push)
    (define-key map "S" 'prat-stash)

    map)
  "The prefix keymap for user commands.

Bind to this to a prefix of your choosing (e.g., \"\C-cv\")")

;; We do this so you can bind the symbol in keymaps.
(fset 'prat-user-command-prefix-keymap prat-user-command-prefix-keymap)

;;
;;  Functions
;;


(prat-define-shell-command prat-stash
  "Issue a stash command."
  :command "git stash"
  :confirm t)

(prat-define-shell-command prat-commit
  "Commit currently staged changes to Git.
With a prefix argument, amends previous commit."
  :command "git commit"
  :prefix-arg "--amend"
  :bufname "*Git Commit*"
  :confirm t)

(prat-define-shell-command prat-rebase
  "Rebase current branch.
With a prefix argument, interactive rebase."
  :command "git rebase"
  :prefix-arg "--interactive"
  :bufname "*Git Rebase*"
  :confirm t)

(prat-define-shell-command prat-push
  "Push current branch to origin"
  :command "git push -u origin HEAD"
  :confirm t)

(prat-define-shell-command prat-show-status
  "Show `git status` output"
  :command "git status -u --show-stash"
  :bufname "*Git Status*"
  :title (format "Status in %s" default-directory))



(defun prat-reload-all ()
  "Reload all source files."
  (interactive)
  (load "prat-async-shell-command.el")
  (load "prat-change-buffers.el")
  (load "prat-hunks.el")
  (load "prat-logs.el")
  (load "prat-shell-command.el")
  (load "prat-stash.el")
  (load "prat-status.el")
  (load "prat-util.el")
  (load "prat.el"))

(provide 'prat)
