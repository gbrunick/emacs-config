;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Comint mode customization
;;
;;  Loading this file does not change the behavior of Emacs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'compile)

(defun gpb-comint:current-input ()
  "Returns the current line of input."
  (let* ((proc (get-buffer-process (current-buffer)))
         (proc-mark (process-mark proc)))
    (if (> (point) proc-mark)
        (buffer-substring-no-properties proc-mark (point))
      "")))


(defun gpb-comint:current-interaction ()
  "The prompt + input + output that contains the point.

Returns a list of the form (BEG END)."
  (list (save-excursion (move-end-of-line 1)
                        (comint-previous-prompt 1)
                        (forward-line 0)
                        (point))
        (save-excursion (move-end-of-line 1)
                        (comint-next-prompt 1)
                        (forward-line 0)
                        (point))))


(defun gpb-comint:current-prompt ()
  "Returns the current prompt."
  (let* ((proc (get-buffer-process (current-buffer)))
         (proc-mark (process-mark proc)))
    (buffer-substring-no-properties
     (save-excursion
       (goto-char proc-mark)
       (forward-line 0)
       (point))
     proc-mark)))


(defun gpb-comint:delete-output (arg)
  "Delete the last prompt and output.
  With an argument, clear the buffer."
  (interactive "P")
  (let ((inhibit-read-only t))
    (if arg
        ;; Delete all prompts except for the current prompt
        (delete-region
         (save-excursion
           (goto-char (point-min))
           (comint-next-prompt 1)
           (forward-line 0)
           (point))
         (or (ignore-errors (overlay-start comint-last-prompt-overlay))
             (save-excursion
               (goto-char (point-max))
               (skip-chars-backward " \t\n")
               (forward-line 0)
               (point))))
      ;; Delete last prompt
      (apply 'delete-region (gpb-comint:current-interaction)))
    ;; (when (and comint-scroll-show-maximum-output
    ;;            (>= (window-end (selected-window) t) (point-max)))
    (save-excursion
      (goto-char (point-max))
      (recenter -1))))


(defun gpb-comint:erase-buffer ()
  (interactive)
  (goto-char (point-max))
  (let ((inhibit-read-only t)
        (inhibit-field-text-motion t))
    (save-excursion
      (beginning-of-line)
      (delete-region (point-min) (point)))))



(defun gpb-comint:goto-error-or-send ()
  (interactive)
  (cond
   ((get-text-property (point) 'compilation-message)
    (compile-goto-error))
   ((or (comint-after-pmark-p)
        ;; put comint modes with no process below
        (derived-mode-p 'inferior-emacs-lisp-mode))
    (let ((inhibit-read-only t)
          (proc (get-buffer-process (current-buffer))))
      (when (and (boundp 'compilation-shell-minor-mode)
                 compilation-shell-minor-mode)
        (compilation-forget-errors))
      (comint-send-input)))
   (t
    (let ((old-input (funcall comint-get-old-input)))
      (goto-char (point-max))
      (while (looking-back "\n")
        (backward-char))
      (insert old-input)))))


(defun gpb-comint:mark-interaction ()
  "Mark the prompt + input + output that contains the point."
  (interactive)
  (multiple-value-bind (beg end) (gpb-comint:current-interaction)
    (push-mark beg nil t)
    (goto-char end)))


(defun gpb-comint:comint-truncate-output (&optional _string)
  "This function can be on `comint-output-filter-functions'."
  (interactive)
  (let ((beg (save-excursion (comint-next-prompt -1) (point)))
        (end (marker-position (process-mark (get-buffer-process
                                             (current-buffer))))))
    (when (> (- end beg) 10000)
      (let ((inhibit-read-only t))
        (delete-region (+ beg 5000) (- end 5000))
        (save-excursion
          (goto-char (+ beg 5000))
          (insert "\n*** some output omitted ***\n"))))))


(defun gpb-comint:split-long-lines (&optional string)
  "This function can be on `comint-output-filter-functions'.

Due to the way Emacs font-locking seems to work, overly long
lines can lock everything up, so we insert newlines if things get
out of control just to prevent a lock up.

"
  (interactive)
  (save-excursion
    (goto-char (process-mark (get-buffer-process (current-buffer))))
    (when (> (current-column) 500)
      (insert-before-markers "\n    [line break inserted]\n"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Minor mode for bold comint prompt
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defface gpb-comint:current-prompt-face
  '((t :weight bold :inherit comint-highlight-prompt))
  "Face used for the current prompt in `gpb-comint:bold-prompt-mode'")

(defface gpb-comint:old-prompt-face
  '((t :weight normal :inherit comint-highlight-prompt))
  "Face used for previous prompts in `gpb-comint:bold-prompt-mode'")


(defun gpb-comint:update-prompt-face-filter (&optional str beg)
  (when (and comint-last-output-start
             (marker-position comint-last-output-start))
    (let* ((beg (or beg (max (point-min) (- comint-last-output-start 1000))))
           (match-beg (text-property-any
                       beg comint-last-output-start
                       'font-lock-face 'comint-highlight-prompt))
           match-end)
      (when match-beg
        (setq match-end (next-single-property-change match-beg 'font-lock-face))
        (add-text-properties match-beg match-end '(font-lock-face
                                                   gpb-comint:old-prompt-face))
        (gpb-comint:update-prompt-face-filter str match-end))))
  str)


(define-minor-mode gpb-comint:bold-prompt-mode ()
  "Minor mode to make the comint prompt bold" nil nil nil
  (cond
   (gpb-comint:bold-prompt-mode
    (set (make-local-variable 'face-remapping-alist) nil)
    (aput 'face-remapping-alist 'comint-highlight-prompt
          'gpb-comint:current-prompt-face)
    (add-hook 'comint-output-filter-functions
              'gpb-comint:update-prompt-face-filter t t))
   (t
    (adelete 'face-remapping-alist 'comint-highlight-prompt)
    (remove-hook 'comint-output-filter-functions
                 'gpb-comint:update-prompt-face-filter t))))


(defun gpb-comint:enable-bold-prompt-mode ()
  "Convenience function for adding to hooks."
  (gpb-comint:bold-prompt-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Minor mode for logging process output
;;
;;  To enable logging in the shell buffer:
;;    M-x: gpb-comint:trace-output-mode
;;
;;  To view the log buffer:
;;    (trace-comint--log-open-log)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (require 'log4e nil t)
  (log4e:deflogger "trace-comint" "%t [%l] %m" "%H:%M:%S"))

(defun gpb-comint:trace-process-output-filter (output)
  (trace-comint--log-trace "Output: %S\n" output)
  output)

(define-minor-mode gpb-comint:trace-output-mode ()
  "Minor mode to make the comint prompt bold" nil nil nil
  (cond
   (gpb-comint:trace-output-mode
    (trace-comint--log-set-level 'trace)
    (trace-comint--log-enable-logging)
    ;(trace-comint--log-open-log)
    (add-hook 'comint-preoutput-filter-functions
              'gpb-comint:trace-process-output-filter nil t))
   (t
    (trace-comint--log-disable-logging)
    (remove-hook 'comint-preoutput-filter-functions
                 'gpb-comint:trace-process-output-filter t))))

(provide 'gpb-comint)
