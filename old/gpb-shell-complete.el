;; To activate this:
;;
;; (gpb-shell-complete-mode 1)
;;

(define-minor-mode gpb-shell-complete-mode
  "Global minor mode to enable readline completion in `shell' buffers.

Installs advice around `shell' that handles completion setup."
  :global t
  (if gpb-shell-complete-mode
      (advice-add 'shell :around #'gpb-shell-complete--shell-advice)
    (advice-remove 'shell #'gpb-shell-complete--shell-advice)))


(defvar gpb-shell-complete-buffer-name "*shell-complete*")

(defvar gpb-shell-complete-uuid
  "4f6e2b56-63de-40c3-a57f-c022633ad7df")

(defvar gpb-shell-complete--output-marker
  (format "END:%s" gpb-shell-complete-uuid))

(defun gpb-shell-complete--shell-advice (orig-fun &optional buffer working-dir)
  "Advice for `shell'.

When called with a prefix argument, prompts the user for the
buffer name and initial working directory.  Giving a directory
that starts with a TRAMP prefix to starts a process on the remote
machine."
  (interactive
   (when current-prefix-arg
     (list
      ;; buffer
      (read-buffer "Shell buffer: "
                   ;; If the current buffer is an inactive
                   ;; shell buffer, use it as the default.
                   (if (and (eq major-mode 'shell-mode)
                            (null (get-buffer-process
                                   (current-buffer))))
                       (buffer-name)
                     (generate-new-buffer-name "*shell*")))
      ;; working-dir
      (read-directory-name "Default directory: " default-directory nil t nil))))

  (let ((buf (or buffer (generate-new-buffer "*shell*")))
        (dir (or working-dir default-directory))
        (config-script (with-temp-buffer
                         (insert-file-contents
                          (locate-library "gpb-shell-complete.bash"))
                         (buffer-substring-no-properties
                          (point-min) (point-max)))))
    (switch-to-buffer buf)
    (with-current-buffer buf
      (setq default-directory dir)
      (when (file-remote-p dir) (setq-local explicit-shell-file-name "bash"))

      ;; Call the original `shell' function to start the process and set up
      ;; `shell-mode'.
      (funcall orig-fun buf)

      ;; Restart bash with readline enabled.
      (gpb-shell-complete--send-input config-script)
      (setq-local completion-at-point-functions
                  `(gpb-shell-complete--complete-at-point-function)))))


(defun gpb-shell-complete--complete-at-point-function ()
  "Completion function compatible with `completion-at-point-functions'"
  (interactive)
  (let* ((buf (current-buffer))
         (proc (get-buffer-process buf))
         (pmark (process-mark proc))
         (buf2 (get-buffer-create gpb-shell-complete-buffer-name))
         (inhibit-read-only t)
         input)

    ;; Is there input after the most recent Bash prompt?
    (when (> (point) pmark)
      (with-current-buffer buf2 (erase-buffer))
      (setq input (buffer-substring-no-properties pmark (point)))
      ;; Insert `input' into the line buffer, trigger compeletion with TAB,
      ;; and then clear the line buffer with \C-u.
      (gpb-shell-complete--send-input (format "%s\t\C-u" input))

      ;; START END COLLECTION
      (list (save-excursion (skip-chars-backward "^ \t/") (point))
            (point)
            (gpb-shell-complete--read-completions)))))


(defun gpb-shell-complete--read-completions ()
  "Read the list of completeions from `gpb-shell-complete-buffer-name'"
  (with-current-buffer gpb-shell-complete-buffer-name
    (goto-char (point-min))
    (skip-chars-forward "\n")
    (split-string (buffer-substring-no-properties
                   (point)
                   (progn
                     (search-forward gpb-shell-complete--output-marker)
                     (forward-line 0)
                     (point)))
                  "\n" t " ")))


(defun gpb-shell-complete--send-input (input)
  "Send INPUT and wait for response.

We direct process output to `gpb-shell-complete-buffer-name'"
  (let* ((buf (current-buffer))
         (buf2 (get-buffer-create gpb-shell-complete-buffer-name))
         (proc (get-buffer-process buf))
         (prompt-regexp (with-current-buffer buf comint-prompt-regexp))
         (pmark (process-mark proc))
         (save-filter (process-filter proc))
         (save-sentinel (process-sentinel proc))
         (save-proc-mark (marker-position (process-mark proc)))
         (pos1 (marker-position pmark))
         (inhibit-read-only t)
         pos)

    (unwind-protect
        (progn
          ;; Wait for the initial prompt to appear in `buf' before we steal
          ;; `proc'.
          (with-current-buffer buf
            (while (not (save-excursion
                          (goto-char (point-min))
                          (re-search-forward prompt-regexp nil t)))
              (accept-process-output proc 0.1)))

          (with-current-buffer buf2 (erase-buffer))
          (set-process-filter proc nil)
          (set-process-sentinel proc nil)
          (set-process-buffer proc buf2)
          (set-marker (process-mark proc) 1 buf2)
          (process-send-string proc (format "%s\n" input))
          (process-send-string proc (format "echo %s\n"
                                            gpb-shell-complete--output-marker))

          (with-current-buffer buf2
            ;; Read the output complete marker
            (while (not (save-excursion
                          (goto-char (point-min))
                          (setq pos (search-forward
                                     gpb-shell-complete--output-marker
                                     nil t))
                          pos))
              (accept-process-output proc 0.1))
            ;; Read the next prompt in the completion buffer.
            (while (and (not (save-excursion
                               (goto-char pos)
                               (re-search-forward prompt-regexp nil t))))
              (accept-process-output proc 0.1))))

      ;; Put the bash process back in its original buffer.
      (set-process-filter proc save-filter)
      (set-process-sentinel proc save-sentinel)
      (set-process-buffer proc buf)
      (set-marker (process-mark proc) save-proc-mark buf))))
