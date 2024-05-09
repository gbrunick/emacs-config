;; Don't cover the prat buffer bindings.  There aren't that many.
(with-eval-after-load 'evil
  (evil-make-overriding-map prat-command-output-mode-map)
  (evil-make-overriding-map prat-commit-graph-mode-map)
  (evil-make-overriding-map prat-edit-mode-map)
  (evil-make-overriding-map prat-hunk-selection-mode-map)
  (evil-make-overriding-map prat-hunk-view-mode-map)
  (evil-make-overriding-map prat-show-commit-files-mode-map)
  (evil-make-overriding-map prat-show-status-mode-map)
  (evil-make-overriding-map prat-staged-changes-mode-map)
  (evil-make-overriding-map prat-unstaged-changes-mode-map))

(provide 'prat-evil)
