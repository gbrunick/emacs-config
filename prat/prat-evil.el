;; Don't cover the prat buffer bindings.  There aren't that many.
(with-eval-after-load 'evil

  (evil-define-key 'normal prat-base-mode-map
    "!" 'prat-shell-command)

  (evil-define-key 'normal prat-shell-command-output-mode-map
    "!" 'prat-shell-command
    "q" 'quit-window)

  (evil-define-key 'normal prat-commit-graph-mode-map
    "d" 'prat-commit-graph-mode--show-diff
    "gr" 'prat-refresh-commit-graph
    (kbd "RET") 'prat-commit-graph--show-commit
    "!" 'prat-shell-command)

  (evil-define-key 'normal prat-hunk-view-mode-map
    "\t" 'prat-forward-command
    [(backtab)] 'prat-backward-command
    ;; "p" 'prat-backward-command
    ;; "P" 'prat-backward-file-command
    ;; "n" 'prat-forward-command
    ;; "N" 'prat-forward-file-command
    (kbd "RET") 'prat-goto-line
    "gr" 'prat-shell-command-refresh)

  (evil-define-key 'normal prat-hunk-selection-mode-map
    "m" 'prat-mark-hunk-command
    "M" 'prat-mark-file-command
    "r" 'prat-mark-as-rename
    "u" 'prat-unmark-hunk-command
    "U" 'prat-unmark-file-command)

  (evil-define-key 'normal prat-show-commit-files-mode-map
    (kbd "RET") 'prat-show-commit-graph--show-file-version)

  (evil-define-key 'normal prat-show-status-mode-map
    "\t" 'forward-button
    "gr" 'prat-show-status--refresh
    "m" 'prat-show-status--mark-file
    "u" 'prat-show-status--unmark-file
    "a" 'prat-show-status--add-files
    "r" 'prat-show-status--reset-files
    "s" 'prat-show-status--stash-files
    "!" 'prat-shell-command)

  (evil-define-key 'visual prat-show-status-mode-map
    "a" 'prat-show-status--add-files
    "r" 'prat-show-status--reset-files
    "s" 'prat-show-status--stash-files)

  (evil-define-key 'normal prat-staged-changes-mode-map
    "r" 'prat-unstage-hunks)

  (evil-define-key 'normal prat-unstaged-changes-mode-map
    "a" 'prat-stage-hunks
    "d" 'prat-revert-marked-hunks
    "w" 'prat-toggle-whitespace-diff-args)

  (evil-define-key 'visual prat-unstaged-changes-mode-map
    "a" 'prat-stage-hunks)

  (evil-define-key 'normal prat-show-tree-mode-map
    "gr" 'prat-refresh-file-tree))


(provide 'prat-evil)
