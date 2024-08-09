(require 'prat-shell-command)

(defcustom prat-show-stash-command
  "git stash show -u --stat -p"
  "Command used to show a stash.")

(defcustom prat-stash-list-command
  "git stash list --format=\"%gd: (%cr) %gs\""
  "Command used to list all stashes.")

(prat-define-shell-command prat-stash-command "git stash"
                           :confirm t)

(prat-define-shell-command prat-show-stash-list
                           prat-stash-list-command
                           :bufname "*Stash List*"
                           :title "Stash list in %s")

(defvar prat-stash-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" 'prat-refresh-stash-list)
    (define-key map (kbd "RET") 'prat-show-stash)
    map)
  "The keymap used when viewing the commit graph.")

(define-derived-mode prat-stash-list-mode prat-shell-command-output-mode
  "Stash List"
  "Mode for buffers displaying the Git stash list.
\\{prat-stash-list-mode-map}")

(defun prat-show-stash (stash)
  (interactive (list (save-excursion
                       (forward-line 0)
                       (if (re-search-forward "^\\(stash[^:]+\\):" (pos-eol) t)
                           (match-string 1)
                         ""))))
  (let ((cmd (format "%s %s" prat-show-stash-command stash)))
    (prat-shell-command cmd "*show stash*")))


;; Register `prat-stash-list-mode' with `prat-shell-command'.

(defun prat-use-stash-list-mode-p (cmd)
  "Function for `prat-shell-command-major-mode-hook'"
  (message "prat-stash-hook: %S" cmd)
  (when (string-match "^git stash list" cmd) #'prat-stash-list-mode))

(add-hook 'prat-shell-command-major-mode-hook #'prat-use-stash-list-mode-p)

(provide 'prat-stash)
