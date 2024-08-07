(defvar prat-stash-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" 'prat-refresh-stash-list)
    (define-key map (kbd "RET") 'prat-show-stash)
    map)
  "The keymap used when viewing the commit graph.")

(define-derived-mode prat-stash-list-mode prat-base-mode
  "Stash List"
  "\nMode for buffers displaying the Git stash list.

\\{prat-stash-list-mode-map}\n")

(define-derived-mode prat-stash-pop-mode prat-shell-command-output-mode
  "Major mode for `git stash pop` output."
  (setq-local prat-confirm-refresh t))

(defun prat-show-stash-list (&rest args)
  "Show the Git stash list."
  (interactive)
  (let* ((buf (get-buffer-create "*Git stash list*"))
         (repo-root (prat-find-repo-root))
         (inhibit-read-only t)
         ;; Show when the revision was stashed.
         (cmd "git stash list --format=\"%gd: (%cr) %gs\""))

    (with-current-buffer buf
      (setq default-directory repo-root)
      (erase-buffer)
      (prat-stash-list-mode)
      (insert (format "Stash list in %s\n\n" default-directory))
      (insert (format "> %s\n\n" cmd))
      (setq-local shell-command cmd
                  shell-output-start (point))
      (prat-refresh-stash-list))

    (switch-to-buffer buf)))


(defun prat-refresh-stash-list ()
  "Update the commit graph in the current buffer."
  (interactive)
  (prat-log-call)
  (let ((inhibit-read-only t))
    (setq-local refresh t)
    (prat-async-shell-command shell-command #'prat-refresh-stash-list-1)))


(defun prat-refresh-stash-list-1 (buf start end complete)
  (prat-log-call)
  (unless complete
    (let ((new-output (with-current-buffer buf
                        (buffer-substring-no-properties start end)))
          (inhibit-read-only t)
          pos)
      (when refresh
        (delete-region shell-output-start (point-max))
        (setq-local refresh nil))
      (setq pos shell-output-start)
      (save-excursion
        (goto-char pos)
        (insert new-output)
        (ansi-color-apply-on-region pos (point-max))))))

(defun prat-show-stash ()
  (interactive)
  (save-excursion
    (forward-line 0)
    (when (re-search-forward "^\\(stash[^:]+\\):" (pos-eol) t)
      (let ((cmd (format "git stash show -u --stat -p %s" (match-string 1))))
        (prat-shell-command cmd)))))

(provide 'prat-stash)
