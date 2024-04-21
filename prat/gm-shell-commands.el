(defvar gpb-git:windows-editor-temp-dir "c:\\Temp"
  "A path containing no spaces where we can write CMD.exe scripts.

No amount of escaping or quoting appears to convince Git to run a
script referenced in GIT_EDITOR or GIT_SEQUENCE_EDITOR that
contains a space in its path, so we write a script to this
directory so we can reference it when calling git for interactive
commands.")

(defvar git-command-output-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "!" 'gpb-git:shell-command)
    (define-key map "q" 'bury-buffer)
    map))

(define-derived-mode git-command-output-mode special-mode "Git Command")

(defun gpb-git:shell-command (cmd &optional bufname)
  "Execute CMD in a new buffer and pop to that buffer.

CMD is a string that is passed through to an interactive Bash or
cmd.exe process.  BUFNAME is a string giving the name of the
buffer in which the results are written.  Any current contents
are deleted."
  (interactive "sGit Shell Command: ")
  (let* ((editor-script (gpb-git:get-editor-script))
         (env-vars (list (format "GIT_EDITOR=%s" editor-script)
                         (format "GIT_SEQUENCE_EDITOR=%s" editor-script)))
         (repo-root (gpb-git--find-repo-root))
         (bufname "*Git Shell Command*"))
    (shpool-shell-command cmd bufname env-vars repo-root)))


(defun gpb-git:async-shell-command (cmd dir &optional callback)
  "Execute CMD in DIR and call CALLBACK as output becomes available.

CMD is a string that is passed through to a Bash or cmd.exe
process.  This string must be properly quoted by the caller.  DIR
is a string giving the working directory in which the command is
executed.  Remote TRAMP directories are supported.  CALLBACK is a
function that accepts (BUF START END COMPLETE).  It is called in
the buffer which was active when `gpb-git:async-shell-command'
was called as output become available.  If that buffer is no
longer alive, CALLBACK is not be called.

CALLBACK is called as output is read from the worker process.
The buffer containing process output is passed to CALLBACK as
BUF.  START and END give the region that contains new output.
The new output only contains complete lines (i.e., newline
terminated).  COMPLETE is t when this worker has finished
processing command and nil otherwise."
  (gpb-git--trace-funcall)
  (let ((buf (current-buffer))
        (server-buf (gpb-git:get-git-server-buf dir))
        proc proc-cmd)
    (with-current-buffer server-buf
      ;; Set variables for `gpb-git:async-shell-command--process-filter'.
      (setq-local callback-buf buf)
      (setq-local callback-func callback)
      (setq proc (get-buffer-process (current-buffer)))

      (set-process-filter proc #'gpb-git:async-shell-command--process-filter)
      (setq proc-cmd
            (format (if (gpb-git:use-cmd-exe)
                        "echo %s:output-start & %s & echo %s:output-end"
                      "echo %s:output-start ; %s ; echo ; echo %s:output-end")
                    gpb-git:process-output-marker
                    cmd
                    gpb-git:process-output-marker))

      (unless (gpb-git:use-cmd-exe)
        ;; Bash doesn't echo the command, so we echo it into the buffer
        ;; directly.  This ensures that each
        ;; `gpb-git:process-output-marker` we echo above starts at the
        ;; beginning of an output line.
        (setq proc-cmd (format "echo \"%s\" && %s" proc-cmd proc-cmd)))

      (process-send-string proc (format "%s\n" proc-cmd))
      (let ((inhibit-message t))
        (message "Sent command %S to %S" cmd (current-buffer))))))


(defun gpb-git:async-shell-command--process-filter (proc string)
  "Process filter for `gpb-git:async-shell-command'."
  (gpb-git--trace-funcall)
  (when (buffer-live-p (process-buffer proc))
    (let ((proc-buf (process-buffer proc))
          ;; echo includes the trailing space when called from cmd.exe.
          (start-regex (format "^%s:output-start ?\n"
                               gpb-git:process-output-marker))
          (end-regex (format "^%s:output-end ?\n"
                             gpb-git:process-output-marker))
          ;; The let-bound variables `output-start' and `output-end' will
          ;; delimit the new lines of output that result from inserting
          ;; `string'.
          output-start output-end complete)

      (with-current-buffer proc-buf
        ;; Buffer local variables set by gpb-git:async-shell-command:
        ;;
        ;; * `output-start-marker' and `output-end-marker' give the
        ;;   location of the current matches to `start-regex' and
        ;;   `end-regex'.  The markers are both initialized to the start of
        ;;   the buffer prior to the first shell command.
        ;;
        ;; * `callback-func' and `callback-buf' give the callback function
        ;;   and the buffer in which it should be called.
        ;;
        ;; Insert `string`, advance the process marker, and record the
        ;; beginning and end of the inserted text as `output-start` and
        ;; `output-end`.
        (save-excursion
          (goto-char (process-mark proc))
          ;; We return complete lines.
          (setq output-start (save-excursion (beginning-of-line) (point)))
          (insert string)
          (set-marker (process-mark proc) (point))
          (setq output-end (save-excursion (beginning-of-line) (point))))

        (when (<= output-start-marker output-end-marker)
          ;; We are waiting on the next output start marker.
          (save-excursion
            (goto-char output-end-marker)
            (when (re-search-forward start-regex nil t)
              (set-marker output-start-marker (match-end 0)))))

        (when (> output-start-marker output-end-marker)
          ;; We are waiting on the next output end marker.
          (setq output-start (max output-start output-start-marker))
          (goto-char output-start)
          (forward-line 0)
          (when (re-search-forward end-regex nil t)
            (setq complete t)
            (set-marker output-end-marker (match-beginning 0))
            ;; Trim the current output region.
            (setq output-end (min output-end output-end-marker))
            (push `(,default-directory . ,proc-buf) gpb-git:worker-pool))

          (save-restriction
            (narrow-to-region output-start-marker output-end)
            ;; Capture the buffer local variable `callback-func' so we can
            ;; call it in the buffer `callback-buf'.
            (when callback-func
              (let ((f callback-func))
                (with-current-buffer callback-buf
                  (funcall f proc-buf output-start output-end complete))))))))))


(defun gpb-git:send-signal-to-git ()
  (gpb-git--trace-funcall)
  (if (gpb-git:use-cmd-exe)
      (unless (= (call-process-shell-command "waitfor /si EmacsEditDone") 0)
        (error "Could not send signal to Git process"))
    (gpb-git:async-shell-command
     (format "echo done. > %s" pipe-file) default-directory)))

(defun gpb-git:commit (&optional amend)
  (interactive "P")
  (if amend
      (gpb-git:shell-command "git commit --amend")
    (gpb-git:shell-command "git commit")))

(defun gpb-git:interactive-rebase (hash)
  (interactive (list (read-string "Base Commit: ")))
  (gpb-git:shell-command (format "git rebase -i %s" hash)))


(defvar gpb-git:edit-mode-keywords
  (list "^pick" "^reword" "^edit" "^squash" "^fixup" "^exec" "^drop"))

(defvar gpb-git:edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'gpb-git:finish-edit)
    map)
  "Keymap for `gpb-git:edit-mode'.")

(defvar gpb-git:edit-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    table)
  "Lines starting with # are comments.")

(define-derived-mode gpb-git:edit-mode text-mode "Git Edit"
  "Mode for editing a file during an interactive Git command."
  ;; :syntax-table gpb-git:edit-mode-syntax-table
  (use-local-map gpb-git:edit-mode-map)
  (setq font-lock-defaults (list 'gpb-git:edit-mode-keywords))
  (font-lock-fontify-buffer)
  (add-hook 'kill-buffer-hook 'gpb-git:kill-buffer-hook nil t))

(defun gpb-git:kill-buffer-hook ()
  (ignore-errors
    (with-current-buffer command-buffer
      (gpb-git:send-signal-to-git))))

(defun gpb-git:finish-edit ()
  "Finish edit and show Git process buffer."
  (interactive)
  (save-buffer)
  ;; The buffer local variable `command-buffer' was set by
  ;; `gpb-git:shell-command-1'.
  (let ((buf command-buffer))
    (kill-buffer)
    (with-current-buffer buf (gpb-git:send-signal-to-git))
    (switch-to-buffer buf)))


(defun gpb-git:use-cmd-exe (&optional dir)
  "Are we using cmd.exe rather than bash?

We use cmd.exe when we are on the local filesystem of a Windows
machine.  In all other cases (i.e., not on Windows machine or on
a Windows machine but working remotely via TRAMP) we use bash."
  (let ((dir (or dir default-directory)))
    (and (eq window-system 'w32)
         (ignore-errors (not (file-remote-p default-directory))))))


(defun gpb-git:reset-worker-pool ()
  "Reset the worker pool of worker processes"
  (interactive)
  (dolist (dir-buf gpb-git:worker-pool)
    (let* ((buf (cdr dir-buf))
           (proc (get-buffer-process buf)))
      (when (process-live-p proc) (kill-process proc))
      (kill-buffer buf)))
  (setq gpb-git:worker-pool nil))


(defun gpb-git:push-changes ()
  (interactive)
  (let ((cmd (read-string "Git Shell Command: " "git push -u origin HEAD")))
    (gpb-git:shell-command cmd)))


(defun gpb-git:get-editor-script (&optional dir)
  (cond
   ;; If we are working locally on a Windows machine, use the
   ;; CMD script.
   ((gpb-git:use-cmd-exe dir)
    (let ((path (concat (file-name-as-directory
                        gpb-git:windows-editor-temp-dir)
                       "git-editor.cmd")))
      (unless (file-exists-p path)
        (copy-file (locate-library "git-editor.cmd") path))
      path))

   (t
    (let ((script-file (locate-library "git-editor.bash"))
          (tmpfile-name (gpb-git:get-temporary-file "git-editor.bash"))
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


(provide 'gm-shell-commands)

