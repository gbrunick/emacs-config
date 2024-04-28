(defvar prat-windows-editor-temp-dir "c:\\Temp"
  "A path containing no spaces where we can write CMD.exe scripts.

No amount of escaping or quoting appears to convince Git to run a
script referenced in GIT_EDITOR or GIT_SEQUENCE_EDITOR that
contains a space in its path, so we write a script to this
directory so we can reference it when calling git for interactive
commands.")

(defvar git-command-output-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "!" 'prat-shell-command)
    (define-key map "q" 'bury-buffer)
    map))

(define-derived-mode git-command-output-mode special-mode "Git Command")

(defun prat-shell-command (cmd &optional bufname)
  "Execute CMD in a new buffer and pop to that buffer.

CMD is a string that is passed through to an interactive Bash or
cmd.exe process.  BUFNAME is a string giving the name of the
buffer in which the results are written.  Any current contents
are deleted."
  (interactive "sGit Shell Command: ")
  (let* ((editor-script (prat-get-editor-script))
         (env-vars (list (format "GIT_EDITOR=%s" editor-script)
                         (format "GIT_SEQUENCE_EDITOR=%s" editor-script)))
         (repo-root (prat-find-repo-root))
         (bufname "*Git Shell Command*"))
    (shpool-shell-command cmd bufname env-vars repo-root)))


(defun prat-send-signal-to-git ()
  (prat-trace-funcall)
  (if (prat-use-cmd-exe)
      (unless (= (call-process-shell-command "waitfor /si EmacsEditDone") 0)
        (error "Could not send signal to Git process"))
    (prat-async-shell-command
     (format "echo done. > %s" pipe-file) default-directory)))

(defun prat-commit (&optional amend)
  (interactive "P")
  (if amend
      (prat-shell-command "git commit --amend")
    (prat-shell-command "git commit")))

(defun prat-interactive-rebase (hash)
  (interactive (list (read-string "Base Commit: ")))
  (prat-shell-command (format "git rebase -i %s" hash)))


(defvar prat-edit-mode-keywords
  (list "^pick" "^reword" "^edit" "^squash" "^fixup" "^exec" "^drop"))

(defvar prat-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'prat-finish-edit)
    map)
  "Keymap for `prat-edit-mode'.")

(defvar prat-edit-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    table)
  "Lines starting with # are comments.")

(define-derived-mode prat-edit-mode text-mode "Git Edit"
  "Mode for editing a file during an interactive Git command."
  ;; :syntax-table prat-edit-mode-syntax-table
  (use-local-map prat-edit-mode-map)
  (setq font-lock-defaults (list 'prat-edit-mode-keywords))
  (font-lock-fontify-buffer)
  (add-hook 'kill-buffer-hook 'prat-kill-buffer-hook nil t))

(defun prat-kill-buffer-hook ()
  (ignore-errors
    (with-current-buffer command-buffer
      (prat-send-signal-to-git))))

(defun prat-finish-edit ()
  "Finish edit and show Git process buffer."
  (interactive)
  (save-buffer)
  ;; The buffer local variable `command-buffer' was set by
  ;; `prat-shell-command-1'.
  (let ((buf command-buffer))
    (kill-buffer)
    (with-current-buffer buf (prat-send-signal-to-git))
    (switch-to-buffer buf)))


(defun prat-use-cmd-exe (&optional dir)
  "Are we using cmd.exe rather than bash?

We use cmd.exe when we are on the local filesystem of a Windows
machine.  In all other cases (i.e., not on Windows machine or on
a Windows machine but working remotely via TRAMP) we use bash."
  (let ((dir (or dir default-directory)))
    (and (eq window-system 'w32)
         (ignore-errors (not (file-remote-p default-directory))))))


(defun prat-reset-worker-pool ()
  "Reset the worker pool of worker processes"
  (interactive)
  (dolist (dir-buf prat-worker-pool)
    (let* ((buf (cdr dir-buf))
           (proc (get-buffer-process buf)))
      (when (process-live-p proc) (kill-process proc))
      (kill-buffer buf)))
  (setq prat-worker-pool nil))


(defun prat-push-changes ()
  (interactive)
  (let ((cmd (read-string "Git Shell Command: " "git push -u origin HEAD")))
    (prat-shell-command cmd)))


(defun prat-get-editor-script (&optional dir)
  (cond
   ;; If we are working locally on a Windows machine, use the
   ;; CMD script.
   ((prat-use-cmd-exe dir)
    (let ((path (concat (file-name-as-directory
                        prat-windows-editor-temp-dir)
                       "git-editor.cmd")))
      (unless (file-exists-p path)
        (copy-file (locate-library "git-editor.cmd") path))
      path))

   (t
    (let ((script-file (locate-library "git-editor.bash"))
          (tmpfile-name (prat-get-temporary-file "git-editor.bash"))
          ;; If we are on windows, we still need to write a Bash script
          ;; with Unix line endings.
          (coding-system-for-write 'us-ascii-unix))
      (cl-assert (not (null script-file)))
      (with-temp-file tmpfile-name
        (insert-file-contents script-file)
        ;; One more attempt to avoid DOS line endings.
        (delete-trailing-whitespace))
      ;; Set the permissions so the owner can read, write and execute.
      ;; 448 = 7 * 8 * 8
      (set-file-modes tmpfile-name 448)
      tmpfile-name))))


(provide 'prat-shell-commands)

