;; prat-editor.el
;;
;; Wrappers for the Git commit and interactive rebase commands that require
;; the user to edit file.
;;

(require 'prat-shell-commands)

(defvar prat-rebase-command-history nil
  "History list for rebase commands.")

(defvar prat-pending-edit-buffer nil
  "The buffer that is currently handling a Git edit.

Only one Git edit can be active at a time.")

(defvar prat-remote-edit-scripts nil
  "An alist with one entry per remote.

The key is a TRAMP prefix as returned by `file-exists-p' and the
value is string giving the path to a copy of prat-editor.bash.")


(defun prat-commit (&optional amend)
  "Commit currently staged changes to Git.
With a prefix argument, amends previous commit."
  (interactive "P")
  (let ((cmd (concat "git"
                     " -c advice.waitingForEditor=false"
                     " -c advice.statusHints=false"
                     " commit")))
    (when amend (setq cmd (concat cmd " --amend")))
    (prat-editor-command cmd "commit" "*Git Commit*")))

(defun prat-rebase (&optional interactive)
  (interactive "P")
  (let ((cmd (if interactive "git rebase --interactive" "git rebase")))
    (prat-editor-command
     (read-shell-command "Rebase command: " cmd prat-rebase-command-history)
     "rebase" "*Git Rebase*")))

(defun prat-editor-command (cmd action buffer-or-name)
  "Execute Git command CMD that may require editing a file.

ACTION is a short description like 'commit' or 'rebase'.  It is
used in some user dialogs.  On completion, shows the command
output in BUFFER-OR-NAME and swithced to this buffer."
  (prat-log-call)
  (when (and prat-pending-edit-buffer
             (buffer-live-p prat-pending-edit-buffer))
    ;; We `pop-to-buffer' in an effort to make it clear this is a
    ;; conflicting pending edit; not the intended edit.
    (pop-to-buffer prat-pending-edit-buffer)
    (error "You must first complete the current edit"))
  (setq prat-pending-edit-buffer nil)
  (let* ((repo-root (prat-find-repo-root))
         (editor-script (prat-get-editor-script))
         ;; We use a named pipe for IPC on Linux.
         (editor-pipe (concat (file-name-as-directory repo-root)
                              ".prat-editor-pipe"))
         (env-vars
          (cond
           ((prat-use-cmd-exe-p)
            ;; Git doesn't seem to like spaces in paths on Windows
            ;; regardless of my attempts at escaping, so I put the spaces
            ;; in PATH.
            (list (format "PATH=%s;%%PATH%%" (file-name-directory editor-script))
                  (format "GIT_EDITOR=%s" (file-name-nondirectory editor-script))
                  (format "GIT_SEQUENCE_EDITOR=%s" (file-name-nondirectory
                                                    editor-script))
                  "GIT_PAGER="))
           (t
            (list (format "GIT_EDITOR=\"%s\"" (file-local-name editor-script))
                  (format "GIT_SEQUENCE_EDITOR=\"%s\"" (file-local-name
                                                        editor-script))
                  (format "PRAT_EDITOR_PIPE=\"%s\""
                          (file-local-name editor-pipe))
                  "GIT_PAGER="
                  "TERM=dumb"))))
         proc)

    ;; Be robust to failures that leave a residual pipe file, but throw a
    ;; warning as this is a bug.
    (when (file-exists-p editor-pipe)
      (warn (format "Deleting %s" editor-pipe))
      (delete-file editor-pipe))

    ;; `prat-editor-callback' opens a buffer to edit the required file.  It
    ;; uses the infomation we provide in `prat-edit-info'.
    (setq proc (prat-async-shell-command
                cmd repo-root #'prat-editor-callback env-vars))
    (with-current-buffer (process-buffer proc)
      (setq-local prat-edit-info
                  (list :command cmd
                        :action action
                        :output-buffer buffer-or-name
                        :repo-root repo-root
                        :pipe-file editor-pipe)))))


(defun prat-editor-callback (buf start end complete)
  "Callback function for commit and interactive rebase commands"
  (prat-log-call)
  (let ((remote-prefix (or (file-remote-p default-directory) ""))
        ;; Remove any quotes around filenames.
        (file-regex (format "^File: \"?\\([^\"\n]+\\)\"?$"))
        (inhibit-read-only t)
        info file-name)
    (with-current-buffer buf
      (setq info prat-edit-info)
      (save-excursion
        (save-match-data
          (goto-char start)
          (cond
           (complete
            ;; Skip the output from out editor script and show the Git
            ;; command output.
            (when (re-search-forward "^Command output:" end t)
              (forward-line 1)
              (setq start (point)))
            (let ((cmd (plist-get info :command))
                  (buf (plist-get info :output-buffer))
                  (output-txt (buffer-substring-no-properties start end)))
              (with-current-buffer (get-buffer-create buf)
                (erase-buffer)
                (special-mode)
                (save-excursion (insert output-txt))
                ;; git rebase uses carriage returns to overwrite lines.
                (save-excursion
                  (while (re-search-forward "^[^]*" nil t)
                    (delete-region (match-beginning 0) (match-end 0))))
                ;; Put the command at the top of the output buffer.
                (save-excursion
                  (insert cmd)
                  (insert "\n\n")))
              (when (eq prat-pending-edit-buffer (current-buffer))
                (setq prat-pending-edit-buffer nil))
              (switch-to-buffer buf)))
           (t
            ;; The process waits for the completion of editing, so we have
            ;; to look for these string before the process completes.
            (when (re-search-forward file-regex end t)
              (setq file-name (match-string-no-properties 1))
              (setq prat-pending-edit-buffer
                    (find-file (concat remote-prefix file-name)))
              (with-current-buffer prat-pending-edit-buffer
                (prat-edit-mode)
                (setq-local prat-edit-info info)
                ;; This happens in a callback so call any post command
                ;; hooks so they can respond to the updated current buffer.
                (run-hooks 'post-command-hook))
              (switch-to-buffer prat-pending-edit-buffer)))))))))


(defun prat-get-editor-script (&optional dir)
  "Get a path to an editor script.

Ensures the script is available on remote Linux systems.  Always
returns a local filename suitable for use with GIT_EDITOR and
GIT_SEQUENCE_EDITOR."
  ;; `prat-use-cmd-exe-p' and `make-nearby-temp-file' use
  ;; `default-directory' below.
  (let* ((default-directory (or dir default-directory))
         (remote (file-remote-p default-directory))
         key-value local-script remote-script)
    (cond
     ((prat-use-cmd-exe-p) (locate-library "prat-editor.cmd"))
     ((not remote) (locate-library "prat-editor.bash"))
     (t
      ;; We have a remote `dir' so we need copy over the editor script.
      (setq key-value (assoc remote prat-remote-edit-scripts))
      (setq remote-script (cdr key-value))
      (unless (and remote-script (file-exists-p remote-script))
        (setq local-script (locate-library "prat-editor.bash"))
        (cl-assert local-script)
        (setq remote-script (make-nearby-temp-file "prat-editor-" nil ".bash"))
        (cl-assert remote-script)
        ;; If we are on windows, we still need to write a Bash script
        ;; with Unix line endings.
        (let ((coding-system-for-write 'us-ascii-unix))
          (with-temp-file remote-script
            (insert-file-contents local-script)
            ;; One more attempt to avoid DOS line endings.
            (delete-trailing-whitespace)))
        ;; Set the permissions so the owner can read, write and execute.
        ;; 448 = 7 * 8 * 8
        (set-file-modes remote-script 448)
        (push (cons remote remote-script) prat-remote-edit-scripts))
      remote-script))))

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
  (add-hook 'kill-buffer-hook
            'prat-edit-kill-buffer-hook nil t)
  (add-hook 'kill-buffer-query-functions
            'prat-edit-kill-buffer-query-function nil t))

(defun prat-signal-editor-script ()
  "Let the editor script know the edit is done.

Expects to be called from a buffer where `prat-edit-info' is
defined."
  (prat-log-call)
  (ignore-errors
    (let ((repo-root (plist-get prat-edit-info :repo-root)))
      (cond
       ((prat-use-cmd-exe-p)
        ;; prat-editor.cmd waits for this signal.
        (prat-async-shell-command "waitfor /si EmacsEditDone"))
       (t
        ;; prat-editor.sh blocks while reading from this named pipe.
        (prat-async-shell-command
         (format "echo done. > .prat-editor-pipe") repo-root))))))

(defun prat-edit-kill-buffer-hook ()
  "Finish edit and show Git process buffer."
  (interactive)
  ;; Ensure we cancel the current operation, even if the user has saved the
  ;; file.
  (erase-buffer)
  (save-buffer)
  (prat-signal-editor-script))

(defun prat-edit-kill-buffer-query-function ()
  "Confirm that the user want to abort the edit."
  (interactive)
  ;; We don't want Emacs to ask again because the buffer is modified.
  ;; `prat-edit-kill-buffer-hook' is just going to erase the buffer anyway.
  (set-buffer-modified-p nil)
  (let ((action (plist-get prat-edit-info :action)))
    (yes-or-no-p (format "Cancel %s? " action))))

(defun prat-finish-edit ()
  "Finish edit and show Git process buffer."
  (interactive)
  (save-buffer)
  (remove-hook 'kill-buffer-hook 'prat-edit-kill-buffer-hook t)
  (remove-hook 'kill-buffer-query-functions
               'prat-edit-kill-buffer-query-function t)
  (prat-signal-editor-script)
  (kill-buffer))

(provide 'prat-editor)
