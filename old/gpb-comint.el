;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Comint mode customization
;;
;;  Loading this file does not change the behavior of Emacs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'compile)


(defun gpb-comint:previous-input (arg)
  "Select a previous input line.

Runs `comint-previous-input', `comint-history-isearch-backward',
or `comint-history-isearch-backward-regexp' depending on the
number of prefix arguments."
  (interactive "p")
  (message "Ags: %s %S" arg arg)
  (cond
   ((>= arg 16) (call-interactively 'comint-history-isearch-backward-regexp))
   ((>= arg 4) (call-interactively 'comint-history-isearch-backward))
   (t (call-interactively 'comint-previous-input))))


(defun gpb-comint:isearch-backward-command (arg)
  "Do-what-I-mean comint isearch backwars.

If before the process mark, call `isearch-backward'.  If called
after the mark with no prefix arguments, call
`comint-history-isearch-backward'.  If called after the mark with
no prefix arguments, call
`comint-history-isearch-backward-regexp'."
  (interactive "p")
  (if (comint-after-pmark-p)
      (cond
       ((>= arg 4)
        (call-interactively 'comint-history-isearch-backward-regexp))
       (t
        (call-interactively 'comint-history-isearch-backward)))
    (call-interactively 'isearch-backward)))


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
  (cl-multiple-value-bind (beg end) (gpb-comint:current-interaction)
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
out of control just to prevent a lock up."
  (interactive)
  (save-excursion
    (goto-char (process-mark (get-buffer-process (current-buffer))))
    (when (> (current-column) 500)
      (insert-before-markers "\n    [line break inserted]\n"))))


(provide 'gpb-comint)
