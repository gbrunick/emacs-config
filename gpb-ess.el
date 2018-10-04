;;
;;  Customizaiton related to the ESS package
;;
;;  See: https://ess.r-project.org/
;;

(require 'cl-lib)
(require 'ffap)

(add-hook 'ess-mode-hook 'gpb:ess-mode-hook)
(add-hook 'inferior-ess-mode-hook 'gpb:inferior-ess-mode-hook)

(setq ess-use-auto-complete nil)

(defun gpb:ess-mode-hook ()
  ;; Get rid of the annoying "smart underscore" behaviour.
  (local-set-key "_" 'self-insert-command)
  (local-set-key "\C-cb" 'gpb:ess-insert-browser)
  (local-set-key "\C-cq" 'gpb:ess-send-quit-command)
  (setq-local ess-indent-with-fancy-comments nil)
  (when (require 'yasnippet nil t)
    (yas-minor-mode 1))
  (when (require 'gpb-text-objects nil t)
    (gpb-modal--define-command-key "q" 'fill-paragraph t)
    (setq-local execute-text-object-function 'gpb:ess-eval-text-object)))


(defun gpb:inferior-ess-mode-hook ()
  (local-set-key [?\t] 'ess-complete-object-name)
  (local-set-key "\r" 'gpb:inferior-ess-send-or-copy-input)
  (local-set-key "\C-n" 'comint-next-input)
  (local-set-key "\C-p" 'gpb-comint:previous-input)
  (local-set-key "\C-ct" 'gpb:ess-send-traceback-command)
  (local-set-key "\C-cq" 'gpb:ess-send-quit-command)

  ;; Get rid of the annoying "smart underscore" behaviour.
  (local-set-key "_" 'self-insert-command)

  ;; Implementation detail of "?" help.
  (add-hook 'comint-redirect-hook 'gpb:show-definition-buffer nil t)

  ;; Track the current line when debugging like pdbtrack in Python.
  (add-hook 'comint-output-filter-functions
            'gpb:ess-debug-track-comint-output-filter-function nil t)

  (when (require 'gpb-text-objects nil t)
    (gpb-modal--define-command-key "g" 'gpb:ess-goto-line t)))


(defun gpb:ess-goto-line (arg)
  "Attempt to jump to an R source line.

With a prefix argument, show the line but don't jump."
  (interactive "P")
  (if (and (eq major-mode 'inferior-ess-mode)
           (comint-after-pmark-p)
           comint-last-output-start)

      ;; Move the point before the line that tells us the current statement.
      (save-excursion
        (goto-char comint-last-output-start)
        (and (re-search-forward "^debug at " nil t)
             (gpb:ess-goto-line arg)))

    ;; Otherwise, look for a line number at the point.
    (let ((text (substring-no-properties
                 (buffer-substring (save-excursion
                                     (re-search-backward "[ \t]\\|^")
                                     (skip-chars-forward " \t")
                                     ;; Unit test failures are wrapped in
                                     ;; Failure(@ ... ):
                                     (when (looking-at "Failure(@")
                                       (goto-char (match-end 0)))
                                     (point))
                                   (save-excursion
                                     (re-search-forward "[ \t]\\|$")
                                     (skip-chars-backward " \t):")
                                     (point))))))
      (when (string-match "\\([^#:]+\\)[#:]\\([0-9]+\\):?" text)
        (let* ((tramp-prefix (file-remote-p default-directory))
               (filename (match-string 1 text))
               (line-number (string-to-number (match-string 2 text)))
               (buf (or (and (file-exists-p (concat tramp-prefix filename))
                             (find-file-noselect (concat tramp-prefix filename)))
                        (get-buffer (file-name-nondirectory filename))
                        (error "Can't find file %S" filename)))
               (window (display-buffer buf 'other-window))
               (vertical-margin (and window (/ (window-height window) 4))))
          (with-current-buffer buf
            ;; Force the window to scroll a bit.
            (goto-line (- line-number vertical-margin))
            (set-window-point window (point))
            (redisplay)
            (goto-line (+ line-number vertical-margin))
            (set-window-point window (point))
            (redisplay)

            (goto-line line-number)
            (skip-chars-forward " \t")
            (set-window-point window (point))
            (let ((ov (make-overlay (save-excursion
                                      (beginning-of-line)
                                      (point))
                                    (save-excursion
                                      (end-of-line)
                                      (when (looking-at-p "\n")
                                        (forward-char 1))
                                      (point)))))
              (overlay-put ov 'face 'region)
              (overlay-put ov 'window window)
              (run-at-time 0.5 nil `(lambda () (delete-overlay ,ov)))))
          (unless arg
            (select-window window)))))))


(defun gpb:show-definition-buffer ()
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
  "Evalute text object in the ESS process associated with the buffer."

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



(defun gpb:ess-insert-browser ()
  "Insert \"browser()\" at the point as save the buffer."
  (interactive)
  (save-match-data
    (let ((pt (point)))
      (beginning-of-line)
      ;; I don't understand why save-excursion doesn't do what I want here.
      (let ((pt (point)))
        (insert "browser()\n")
        (goto-char pt))
      (indent-according-to-mode))))


(defun gpb:ess-debug-track-comint-output-filter-function (output)
  (when (and (> (length output) 0)
             (save-excursion
               (beginning-of-line)
               (looking-at-p "Browse\\[[0-9]+\\]>")))
    (gpb:ess-goto-line t))
  output)


(defun gpb:ess-send-traceback-command ()
  (interactive)
  (ess-send-string (get-buffer-process (current-buffer)) "traceback()" t))


(defun gpb:ess-send-quit-command ()
  (interactive)
  (ess-send-string (ess-get-process) "Q" t))
