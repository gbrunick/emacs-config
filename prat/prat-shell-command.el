;; prat-shell-command.el
;;
;; Provides the `prat-shell-command' function.
;;

(require 'prat-async-shell-command)


(defcustom prat-auto-refresh-commands '("^git status")
                                        ;; "^git log")
  "A list of regular expressions.
If a shell command matches a regular expression in this list, we configure
the ouput buffer to automatically refresh when a Git command changes the
current state."
  :type '(repeat (string))
  :group 'prat)

(defvar prat-rebase-command-history nil
  "History list for rebase commands.")

(defvar prat-pending-edit-buffer nil
  "The buffer that is currently handling a Git edit.

Only one Git edit can be active at a time.")

(defvar prat-remote-edit-scripts nil
  "An alist with one entry per remote.

The key is a TRAMP prefix as returned by `file-exists-p' and the
value is string giving the path to a copy of prat-editor.bash.")

(defvar prat-shell-command-output-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "!" 'prat-shell-command)
    (define-key map "g" 'prat-shell-command-refresh)
    map))

(define-derived-mode prat-shell-command-output-mode
  prat-base-mode "Git Output")

(defun prat-commit (&optional amend)
  "Commit currently staged changes to Git.
With a prefix argument, amends previous commit."
  (interactive "P")
  (let ((cmd "git commit"))
    (when amend (setq cmd (concat cmd " --amend")))
    (prat-shell-command cmd "*Git Commit*")))

(defun prat-rebase (&optional interactive)
  (interactive "P")
  (let ((cmd "git rebase"))
    (when interactive (setq cmd (concat cmd " --interactive")))
    (prat-shell-command
     (read-shell-command "Shell command: " cmd prat-rebase-command-history)
     "*Git Rebase*")))

(defvar-local prat-shell-command-info nil
  "Defined in shell command output buffers.
Copied into edit buffers that are requested by Git during interactive
commands.")

(defun prat-shell-command (cmd &optional bufname title major-mode-func)
  "Execute Git command CMD that may require editing a file.

On completion, shows the command output in a buffer named BUFNAME and
switches to this buffer."
  (interactive (list (read-shell-command "Shell Command: ")))
  (prat-log-call)

  (let* ((buf (get-buffer-create (or bufname "*Shell Command Output*")))
         (dir default-directory)
         (editor-script (prat-get-editor-script))
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
            (list (format "GIT_EDITOR=\"bash '%s'\"" (file-local-name editor-script))
                  (format "GIT_SEQUENCE_EDITOR=\"bash '%s'\"" (file-local-name
                                                               editor-script))
                  "GIT_PAGER="
                  "TERM=dumb"))))
         (subcommand (prat-get-subcommand cmd))
         (subcommand-major-mode (plist-get subcommand :major-mode))
         (inhibit-read-only t)
         pos)

    (with-current-buffer buf
      (erase-buffer)
      (insert (or title dir) "\n\n")
      (insert "> " cmd)

      ;; Set the major-mode.
      (cond
       (major-mode-func       (funcall major-mode-func))
       (subcommand-major-mode (funcall subcommand-major-mode))
       (t                     (prat-shell-command-output-mode)))

      (setq-local default-directory dir)
      (setq-local prat-shell-command-info
                  `( :command ,cmd
                     :env-vars ,env-vars
                     :output-buffer ,buf
                     :directory ,dir
                     :output-pos ,(point)
                     :markup-func ,(plist-get subcommand :markup-func)))

      (dolist (regex prat-auto-refresh-commands)
        (when (string-match regex cmd)
          (setq-local prat-auto-refresh-this-buffer t)))

      (prat-shell-command-refresh)
      (forward-line 0)
      (switch-to-buffer buf))))


(defun prat-shell-command-refresh ()
  (interactive)
  (let ((cmd (plist-get prat-shell-command-info :command))
        (env-vars (plist-get prat-shell-command-info :env-vars))
        (output-pos (plist-get prat-shell-command-info :output-pos))
        (inhibit-read-only t))
    (save-excursion
      (goto-char output-pos)
      (insert " ")
      (prat-insert-spinner))

    ;; `prat-async-shell-command-1' increments
    ;; `prat-git-state-change-count' each time we run a Git command that
    ;; changes the current state.
    (setq-local prat-shell-command-info
                (plist-put prat-shell-command-info
                           :git-state prat-git-state-change-count))

    (prat-async-shell-command cmd #'prat-shell-command-refresh-1
                              env-vars 'no-check)))


(defun prat-shell-command-refresh-1 (buf start end complete)
  "Implementation detail of `prat-shell-command'

Processes the output from a shell command."
  (prat-log-call)
  (save-match-data
    (cond
     (complete
      (let ((cmd (plist-get prat-shell-command-info :command))
            (output-pos (plist-get prat-shell-command-info :output-pos))
            (edit-buffer (plist-get prat-shell-command-info :edit-buffer))
            (markup-func (plist-get prat-shell-command-info :markup-func))
            (output-text (with-current-buffer buf
                           (goto-char start)
                           (when (and (buffer-live-p prat-pending-edit-buffer)
                                      (re-search-forward
                                       "^SHOW_CURRENT_EDIT_BUFFER$" end t))
                            (display-buffer prat-pending-edit-buffer))
                           ;; Skip the output from our editor script and
                           ;; show the Git command output.
                           (when (re-search-forward "^GIT_OUTPUT_START:" end t)
                             (forward-line 1))
                           (buffer-substring-no-properties (point) end)))
            (inhibit-read-only t)
            ;; Preserve the line position in the buffer.
            (initial-line (line-number-at-pos))
            beg)

        ;; (message "initial-line: %S %S" initial-line major-mode-func)
        (delete-region output-pos (point-max))
        (goto-char (point-max))
        (insert "\n\n")
        (setq beg (point))
        (insert output-text)

        ;; Look for diff output.
        (goto-char beg)
        (when (re-search-forward "^diff --git" nil t)
          (prat-format-hunks (pos-bol)))

        ;; (message "markup-func: %S" markup-func)
        (when markup-func
          (goto-char output-pos)
          (funcall markup-func))

        (goto-char (point-min))
        (forward-line (1- initial-line))
        (run-hooks 'post-command-hook)))

     (t
      (let* ((info prat-shell-command-info) file-name)
        (with-current-buffer buf
          (save-excursion
            (goto-char start)
            ;; The process waits for the completion of editing, so we have
            ;; to look for this string before the process completes.
            (when (re-search-forward "^File: \"?\\([^\"\n]+\\)\"?$" end t)
              (setq file-name (concat (or (file-remote-p default-directory) "")
                                      (match-string-no-properties 1))
                    prat-pending-edit-buffer (find-file file-name))

              (with-current-buffer prat-pending-edit-buffer
                (prat-edit-mode)
                (setq-local prat-shell-command-info info)
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

Expects to be called from the buffer where are editing a file for Git."
  (prat-log-call)
  (ignore-errors
    (cond
     ((prat-use-cmd-exe-p)
      ;; prat-editor.cmd waits for this signal.
      (prat-async-shell-command "waitfor /si EmacsEditDone"))
     (t
      ;; prat-editor.sh blocks while reading from this named pipe.
      (prat-async-shell-command "echo done. > .prat-editor-lock")))

    (let ((lock-file  ".prat-editor-lock"))
      (when (file-exists-p lock-file)
        (delete-file lock-file)
        (message "Deleted %s" (expand-file-name lock-file))))))

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
  (let ((cmd (plist-get prat-shell-command-info :command)))
    (yes-or-no-p (format "Cancel %s? " cmd))))

(defun prat-finish-edit ()
  "Finish edit and show Git process buffer."
  (interactive)
  (save-buffer)
  (remove-hook 'kill-buffer-hook 'prat-edit-kill-buffer-hook t)
  (remove-hook 'kill-buffer-query-functions
               'prat-edit-kill-buffer-query-function t)
  (prat-signal-editor-script)
  (kill-buffer))


;; Major-mode selection for shell output buffers.

(defvar prat-shell-subcommands
  '(("^git status" :major-mode prat-show-status-mode
                   :markup-func prat-markup-status-output
                   :auto-refresh t)))


(defun prat-get-subcommand (cmd)
  (catch 'found-subcommand
    (dolist (subcmd prat-shell-subcommands)
      (when (string-match (car subcmd) cmd)
        (throw 'found-subcommand (cdr subcmd))))))


;; Support for autorefreshing status and log buffers.

;; (define-minor-mode prat-auto-refresh-mode
;;   "When enabled, we refresh Git status and log buffers as needed."
;;   :global t
;;   (let ((hook 'window-buffer-change-functions)
;;         (func #'prat-refresh-all-buffers))
;;     (if prat-auto-refresh-mode
;;         (add-hook hook func)
;;       (remove-hook hook func))))

(defvar-local prat-auto-refresh-this-buffer nil
  "If non-nil, we call `prat-shell-command-refresh' on this buffer when the
Git state changes.")

(defun prat-refresh-all-buffers (&rest args)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when prat-auto-refresh-this-buffer
        (let ((git-state (plist-get prat-shell-command-info :git-state)))
          (when (< git-state prat-git-state-change-count)
            ;; (message "Refreshing %S..." (current-buffer))
            (prat-shell-command-refresh)))))))

;; (defun prat-refresh-visible-buffers (&rest args)
;;   (let* ((windows (flatten-list (mapcar #'window-list (frame-list))))
;;          (buffers (flatten-list (mapcar #'window-buffer windows))))
;;     (dolist (buf (delete-dups buffers))
;;       (with-current-buffer buf
;;         (when prat-auto-refresh-this-buffer
;;           (let ((git-state (plist-get prat-shell-command-info :git-state)))
;;             (when (< git-state prat-git-state-change-count)
;;               (message "Refreshing %S..." (current-buffer))
;;               (prat-shell-command-refresh))))))))



(provide 'prat-shell-command)
