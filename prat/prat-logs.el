(require 'prat-shell-command)

(defcustom prat-show-commit-graph-command
  "git log --graph --oneline --decorate --color"
  "Command used to show the commit graph.")

(defcustom prat-log-excluded-branches '("refs/stash")
  "Add globs to this list to exclude branches from git log output."
  :type '(repeat (string))
  :group 'prat)

(defvar prat-commit-graph-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "d" 'prat-commit-graph--show-diff)
    (define-key map (kbd "RET") 'prat-commit-graph--show-commit)
    (define-key map "\C-c\C-c" 'prat-commit-graph--checkout)
    (define-key map "\C-c\C-r" 'prat-commit-graph--reset)
    (define-key map "\C-c\C-d" 'prat-commit-graph--delete-branch)
    (define-key map "\C-c\C-f" 'prat-commit-graph--list-commit-files)
    map)
  "The keymap used when viewing the commit graph.")

(define-derived-mode prat-commit-graph-mode prat-shell-command-output-mode
  "Commit Graph"
  "Mode for buffers displaying the Git commit graph.
\\{prat-commit-graph-mode-map}"
  (push #'prat-markup-commit-graph-output prat-shell-command-markup-functions))


(defun prat-show-commit-graph (&optional repo-root)
  "Show the Git commit graph in a buffer."
  (interactive)
  (let* ((buf (get-buffer-create "*Git commit graph*"))
         (default-directory (or repo-root (prat-find-repo-root)))
         (inhibit-read-only t)
         (cmd prat-show-commit-graph-command))

    (dolist (glob prat-log-excluded-branches)
      (setq cmd (format "%s --exclude=\"%s\"" cmd glob)))
    (setq cmd (format "%s --all" cmd))

    (prat-shell-command cmd "*Commit Graph*" (format "Commit graph in %s"
                                                     default-directory))))


(defun prat-markup-commit-graph-output ()
  "Add text properties to git log --graph output.

Adds a :commit-hash and :branch-names property to each log entry and puts
a :branch-name property on each branch name."
  (interactive "r")
  (let ((inhibit-read-only t) bound name branch-names)
    (while (re-search-forward "^[* \\|/-]+\\.? +\\([a-f0-9]+\\) " end t)
      ;; Mark the whole line with the hash
      (put-text-property (pos-bol) (pos-eol) :commit-hash
                         (match-string-no-properties 1))

      ;; Now look for branch names in the decorations.
      (when (re-search-forward "(.*)" (pos-eol) t)
        (setq branch-names nil)
        (goto-char (1+ (match-beginning 0)))
        (setq bound (match-end 0))
        ;; If we are the line that contains HEAD, skip past this.
        (re-search-forward "HEAD -> " bound t)
        (while (re-search-forward " *\\([^,)]+\\)[,)] *" bound t)
          (setq name (match-string-no-properties 1))
          (put-text-property (match-beginning 1) (match-end 1)
                             :branch-name name)
          (push name branch-names))
        (put-text-property (pos-bol) (pos-eol) :branch-names
                           (reverse branch-names))))))


(defun prat-commit-graph--commit-at (&optional pos branch)
  "Find the branch or commit at position POS.
POS defaults to the point.  If BRANCH is non-nil, we only return branch
names (not commit hashes)."
  (let* ((txt (if pos "POS" "point"))
         (pos (or pos (point))))
    (or (get-text-property pos :branch-name)
        (car (get-text-property pos :branch-names))
        (if branch (error "No branch at %s" txt)
          (get-text-property pos :commit-hash))
        (error "No commit at %s" txt))))

(defmacro prat-commit-graph--defcmd (name base-cmd &optional alt-cmd branch-only)
  "Define a simple command that operates on a branch or commit.

NAME is the name of the function that is defined.  BASE-CMD is the Git
command to call.  If ALT-CMD is provided, this command is invoked when the
command has a prefix argument.  BRANCH is non-nil, the command requires a
branch name, rather than a commit hash."
  `(defun ,name (arg)
     (interactive "P")
     (let ((cmd (format "git %s %s"
                        (if arg (or ,alt-cmd ,base-cmd) ,base-cmd)
                        (prat-commit-graph--commit-at nil ,branch-only))))
       (prat-async-shell-command (read-shell-command "Shell command: " cmd)))))

;; Simple commands that operate on the current branch/commit.
(prat-commit-graph--defcmd prat-commit-graph--checkout "checkout" nil)
(prat-commit-graph--defcmd prat-commit-graph--reset "reset")
(prat-commit-graph--defcmd prat-commit-graph--delete-branch
                           "branch -d" "branch -D" t)

(defun prat-commit-graph--show-commit (&optional arg)
  "Show commit at point.
With a prefix argument, we ignore whitespace changes."
  (interactive "P")
  (let ((hash (prat-commit-graph--commit-at)))
    (if arg
        (prat-show-commit hash "-b")
      (prat-show-commit hash))))

(defun prat-commit-graph--list-commit-files (&optional pos)
  (interactive)
  (let ((hash (prat-commit-graph--commit-at)))
      (prat-show-file-tree hash)))


(defun prat-show-file-history (&optional filename)
  (interactive)
  (let* ((filename (or filename (buffer-file-name)))
         (basename (file-name-nondirectory filename))
         (cmd `("git" "log" "--follow" "-p" "--" ,basename))
         (buf (get-buffer-create (format "*git log: %s*" basename)))
         (dir default-directory)
         (inhibit-read-only t))
    (with-current-buffer buf
      (setq buffer-read-only t
            default-directory dir)
      (erase-buffer)
      (insert (format "%s\n\n" (mapconcat 'identity cmd " ")))
      (apply 'process-file (car cmd) nil t t (cdr cmd))
      (diff-mode))
    (pop-to-buffer buf)))


;; Register `prat-commit-graph-mode' with `prat-shell-command'.

(defun prat-use-commit-graph-mode-p (cmd)
  "Function for `prat-shell-command-major-mode-hook'"
  (message "prat-use-commit-graph-mode: %S" cmd)
  (when (string-match "^git log --graph --oneline" cmd) #'prat-commit-graph-mode))

(add-hook 'prat-shell-command-major-mode-hook #'prat-use-commit-graph-mode-p)

(provide 'prat-logs)
