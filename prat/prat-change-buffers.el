;;
;; prat-change-buffers.el
;;
;; Function to show changes/diffs in a buffer.
;;

;; Base modes

(defvar prat-hunk-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'prat-forward-command)
    (define-key map [(backtab)] 'prat-backward-command)
    (define-key map "p" 'prat-backward-command)
    (define-key map "P" 'prat-backward-file-command)
    (define-key map "n" 'prat-forward-command)
    (define-key map "N" 'prat-forward-file-command)
    (define-key map (kbd "RET") 'prat-goto-line)
    (define-key map "g" 'prat-shell-command-refresh)
    (fset 'prat-hunk-view-mode-map map)
    map)
  "Base keymap for hunk viewing.")

(define-derived-mode prat-hunk-view-mode prat-base-mode
  "Hunk Buffer"
  "\nBase mode for buffers showing hunks."
  (setq-local header-line-format '(:eval (prat-compute-hunk-buffer-header)))
  (setq-local buffer-read-only t)
  (setq-local tab-width 4)
  (setq-local revert-buffer-function 'prat-revert-changes-buffer))

(defvar prat-hunk-selection-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "m" 'prat-mark-hunk-command)
    (define-key map "M" 'prat-mark-file-command)
    (define-key map "r" 'prat-mark-as-rename)
    (define-key map "u" 'prat-unmark-hunk-command)
    (define-key map "U" 'prat-unmark-file-command)
    (set-keymap-parent map 'prat-hunk-view-mode-map)
    (fset 'prat-hunk-selection-mode-map map)
    map)
  "Base keymap for hunk viewing and selection.")

(define-derived-mode prat-hunk-selection-mode prat-hunk-view-mode
  "Hunk Buffer"
  "\nBase mode for buffers showing hunks."
  (add-hook 'post-command-hook 'prat-post-command-hook))


;; Unstaged changes

(defvar prat-unstaged-changes-mode-map
  (let ((map (make-sparse-keymap)))
    ;; "add" new files to the index.
    (define-key map "a" 'prat-stage-hunks)
    ;; "delete" changes.
    (define-key map "d" 'prat-revert-marked-hunks)
    (define-key map "w" 'prat-toggle-whitespace-diff-args)
    (set-keymap-parent map 'prat-hunk-selection-mode-map)
    (fset 'prat-unstaged-changes-mode-map map)
    map)
  "The keymap used for unstaged hunks.")

(define-derived-mode prat-unstaged-changes-mode prat-hunk-selection-mode
  "Unstaged Changes"
  "\nMode for selecting unstages changes to be added to the index.

\\{prat-unstaged-changes-mode-map}\n"
  (setq-local staged-changes-buffer nil))

(defun prat-show-unstaged-changes (&optional button)
  (interactive)
  (prat-shell-command "git diff --histogram --find-renames --stat --patch"
                      prat-unstaged-buffer-name
                      nil
                      (format "Unstaged changes in %s" default-directory)
                      'prat-unstaged-changes-mode))

;; Staged Changes

(defvar prat-staged-changes-mode-map
  (let ((map (make-sparse-keymap)))
    ;; "reset" marked hunks
    (define-key map "r" 'prat-unstage-hunks)
    (set-keymap-parent map 'prat-hunk-selection-mode-map)
    (fset 'prat-staged-changes-mode-map map)
    map)
  "The keymap used for staged hunks.")

(define-derived-mode prat-staged-changes-mode prat-hunk-selection-mode
  "Staged Changes"
  "\nMode for selecting stages changes to be removed from the index.

\\{prat-staged-changes-mode-map}\n"
  (setq-local staged-changes-buffer t))

(defun prat-show-staged-changes (&optional button)
  (interactive)
  (prat-shell-command
   "git diff --cached --histogram --find-renames --stat --patch"
   "*staged changes*"
   nil
   (format "Staged changes in %s" default-directory)
   'prat-staged-changes-mode))

(defun prat-show-commit (hash)
  "Write information about the commit HASH into the current buffer."
  (interactive (read-string "Commit: "))
  (prat-shell-command
   (format "git show --stat --patch %s --" hash)
   (format "*commit: %s*" hash)
   nil
   (format "Commit %s in %s" hash default-directory)))

(defun prat-show-commit-diff (hash1 hash2)
  "Display the changes from HASH1 to HASH2."
  (prat-shell-command
   (format "git diff --stat --patch --histogram --find-renames %s..%s --"
           hash1 hash2)
   (format "*diff %s...%s*" hash1 hash2)
   nil
   (format "Changes from %s to %s in %s" hash1 hash2 default-directory)))


(provide 'prat-change-buffers)
