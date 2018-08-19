;;
;;  Customizaiton related to the ESS package
;;
;;  See: https://ess.r-project.org/
;;

(require 'cl-lib)


(add-hook 'ess-mode-hook 'gpb:ess-mode-hook)
(add-hook 'inferior-ess-mode-hook 'gpb:inferior-ess-mode-hook)

(defun gpb:ess-mode-hook ()
  (when (require 'gpb-text-objects nil t)
    (setq-local execute-text-object-function 'gpb:ess-eval-text-object)))


(defun gpb:inferior-ess-mode-hook ()
  (local-set-key [?\t] 'ess-complete-object-name)
  (local-set-key "\r" 'gpb:inferior-ess-send-or-copy-input)
  ;; Get rid of the annoying "smart underscore" behaviour.
  (local-set-key "_" 'self-insert-command)
  (add-hook 'comint-redirect-hook 'gpb:pop-to-buffer nil t))


(defun gpb:pop-to-buffer ()
  (message "current buffer: %S" (current-buffer))
  (message "gpb:output-buffer: %S" ())
  (let ((buffer gpb:output-buffer))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (setq-local buffer-read-only t)
      (set-buffer-modified-p nil)
      (goto-char 0)
      ;; Some packages (like `gpb-modal'), get confused if we change
      ;; buffers asyncronously in a callback function.  Running the
      ;; `post-command-hook' helps them get sorted.
      (run-hooks 'post-command-hook))))


(defun gpb:inferior-ess-send-or-copy-input ()
  "Send the current line or copy a previous line to the current line.

This is a replacement for the function `inferior-ess-send-input'
which immediately sends input when called on a previous line in
an ESS inferior buffer."
  (interactive)
  (cond
   ;; If we end the line with a ?, we put the output in another window for
   ;; easier browsing.  This is a workaround for reading function
   ;; definitions.  Ideally, we would prefer to jump to the definition but
   ;; I haven't figures that out yet.
   ((and (comint-after-pmark-p) (looking-back "? *"))
    (skip-chars-backward " ")
    (skip-chars-backward "?")
    (let* ((proc (get-buffer-process (current-buffer)))
           (pmark (process-mark proc))
           (initial-pmark (marker-position pmark))
           (command (buffer-substring pmark (point)))
           (buffer (get-buffer-create (concat "*" command "*"))))
      (delete-region initial-pmark (save-excursion (end-of-line) (point)))
      ;; Send a blank line and wait fora responds to trigger all the proper
      ;; comint prompt accounting.
      (inferior-ess-send-input)
      (while (or (= (marker-position pmark) initial-pmark)
                 (not (looking-back "> *")))
        (accept-process-output proc 3 nil t))
      ;; Now insert the fake input above.
      (save-excursion
        (goto-char initial-pmark)
        (insert (concat command "?"))
        (comint-add-to-input-history (concat command "?")))

      ;; Now actually send the command and pipe the results to a new buffer.
      (with-current-buffer buffer
        (setq-local default-directory nil)
        (setq-local buffer-read-only nil)
        (erase-buffer)
        (insert (format "#\n#  %s\n#\n\n" command))
        (ess-r-mode 1))
      (setq-local gpb:output-buffer buffer)
      (comint-redirect-send-command command buffer nil)))

   ((comint-after-pmark-p)
    (inferior-ess-send-input))
   (t
    (let ((old-input (funcall comint-get-old-input)))
      (goto-char (point-max))
      (while (looking-back "\n")
        (backward-char))
      (insert old-input)))))


;; Additional support for text objects
(defun gpb:ess-eval-text-object (obj start end)
  "Evalute text object in the ESS process assocaited with the buffer."

  ;; It is unlikely that ESS is going to start the process correctly.
  ;; Better to just error out if it is going to try.  We do want to allow
  ;; it to search for existing interpreters as that logic seems pretty
  ;; involved.  See `ess-request-a-process'."
  (cl-letf (((symbol-function 'ess-start-process-specific)
             (lambda (&rest) (error (concat "No interpreter process is "
                                            "associated with this buffer.")))))
    (ess-force-buffer-current "Process: " nil t nil))
  (let ((ess-eval-deactivate-mark nil)
        (ess-eval-visibly t))
    (ess-eval-region start end nil)))
