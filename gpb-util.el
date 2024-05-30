
(defun gpb-next-window (arg)
  "Select next window using `other-window'

Rotate the buffers within the current window when a prefix
argument is given."
  (interactive "P")
  (if arg
      (let* ((wins (window-list))
             (bufs (mapcar (lambda (x) (window-buffer x)) wins)))
        (setq bufs (append (cdr bufs) (list (car bufs))))
        (cl-mapcar (lambda (w b) (set-window-buffer w b))
                   wins bufs)))
  (other-window 1 t)
  (when (and (boundp 'evil-mode) evil-mode)
    (evil-change-to-initial-state)))

(defun gpb-previous-window ()
  (interactive)
  (other-window -1 t)
  (when (and (boundp 'evil-mode) evil-mode)
    (evil-change-to-initial-state)))


(defun gpb-forward-page (&optional arg)
  "Move cursor to the last row in the window.

If the cursor is already on the last row, scroll forward a page.  ARG gives
a repeatition count.  If ARG is negative, we first move the cursor to the
first row in the  window and then scroll backwards by pages."
  (interactive "p")
  (message "gpb-forward-page: %S" arg)
  (let* ((arg (or arg 1))
         ;; Last point on the first line.
         (first-line-pt (save-excursion (move-to-window-line 0)
                                        (end-of-line)
                                        (point)))
         ;; First point on the last line.
         (last-line-pt (save-excursion (move-to-window-line -1)
                                       (forward-line 0)
                                       (point)))
         (scroll-preserve-screen-position 'always))
    (unless (memq last-command '(previous-line next-line))
      ;; see line-move-visual
      (setq temporary-goal-column (/ (float (car (nth 2 (posn-at-point))))
                                     (frame-char-width))))
    (cond
     ;; Moving forward
     ((> arg 0)
      (setq last-command 'next-line this-command 'next-line)
      (cond
       ((< (point) last-line-pt) (move-to-window-line -1))
       (t (scroll-up) (move-to-window-line -1)))
      (gpb-forward-page (1- arg)))

     ;; Moving backward
     ((< arg 0)
      (setq last-command 'previous-line this-command 'previous-line)
      (cond
       ((> (point) first-line-pt) (move-to-window-line 0))
       (t (scroll-down) (move-to-window-line 0)))
      (gpb-forward-page (1+ arg))))

    (vertical-motion (cons (or goal-column
                               (truncate (or (car-safe temporary-goal-column)
                                             temporary-goal-column)))
                           0))))

(defun gpb-backward-page (arg)
  "Move to top of page, or scroll up by a single page"
  (interactive "p")
  (message "gpb-backward-page: %S" arg)
  (gpb-forward-page (- arg)))


(defun gpb-delete-path-segment-backwards ()
  (interactive)
  (delete-region (point)
                 (save-excursion
                   (save-match-data
                     (when (looking-back "[/\\]") (backward-char))
                     (if (re-search-backward "[/\\]" nil t)
                         (forward-char)
                       (move-beginning-of-line nil))
                     (point)))))


(defun gpb-keyboard-quit ()
  (interactive)
  (cond
   ;; or (recursion-depth)?
   (completion-in-region-mode
    (completion-in-region-mode -1))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   ((and (boundp 'evil-state)
         (memq evil-state '(insert visual)))
    (evil-force-normal-state))
   (t
    (call-interactively 'keyboard-quit))))


;; Make some commands preserve an active mark.
;;
;; We apply to `undo' and `redo' so it is easier repeat

(defun gpb-preserve-mark (symbol)
  "Prevent the function attached to SYMBOL from deactivating the mark"
  (advice-add symbol :after 'gpb-set-deactivate-mark-nil))

(defun gpb-set-deactivate-mark-nil (&rest r)
  (setq deactivate-mark nil))


;;
;; Make some multi-character commands repeatable by their last keystroke.
;;

(defun gpb-repeatable-command-advice (f &rest args)
  "This is used to make a command repeatable with a single key.

Repeating the final key sequence of the command sequence that invoked the
advised command repeats the command."
  (let* ((this-keys (this-command-keys))
         (last-char (substring this-keys (1- (length this-keys))))
         (keymap (make-sparse-keymap))
         (help (format "\"%s\" repeats the last command" last-char)))
    ;; (message "this-keys: %S" this-keys)
    ;; (message "last-char: %S" last-char)
    ;; (message "help: %S" help)
    ;; (message "interactive: %S" (called-interactively-p))
    (define-key keymap last-char #'repeat)
    ;; Make sure `f' succeeds before we install the repeater keymap.
    (apply f args)
    (when (called-interactively-p)
      ;; Install `keymap' for the next keysequence.  If we see the same key,
      ;; that triggers `repeat' and `repeat' handles repeating itself from
      ;; there.
      (set-transient-map keymap nil nil help))))

(defun gpb-make-repeatable (&rest commands)
  (dolist (command commands)
    (advice-add command :around 'gpb-repeatable-command-advice)))



;;
;; Make some commands give feedback in the message buffer
;;

(defmacro gpb-add-feedback (command msg)
  "Echo MSG in the minibuffer when `command' is run interactively.

MSG can be a string or an Sexp that evaluates to a string when evaluated in
an after advice function.  Return the symbol corresponding to the
dynamically generated advice function."
  (declare (indent defun))
  (let ((advice-symbol (intern (format "gpb-%s-feedback-advice" command))))
    `(progn
       ;; Define an advice function with a dynamically generated name.
       (defun ,advice-symbol (&rest args)
         "Dynamically generated advice function.  See `gpb-add-feedback'."
         (when (called-interactively-p) (message "%s" ,msg)))

       ;; And then arrange for it to be called after `command'.
       (advice-add #',command :after #',advice-symbol)

       ;; Return the new advice function.
       #',advice-symbol)))


;; To debug the macro gpb-add-feedback:
;;
;; (with-current-buffer (get-buffer-create "*macroexpand*")
;;   (erase-buffer)
;;   (fundamental-mode)
;;   (insert (pp-to-string (macroexpand
;;                          '(gpb-add-feedback revert-buffer
;;                             (format "Reverted %s" (current-buffer)))))))


(defun gpb-insert-message ()
  (interactive)
  (let ((obj (substring-no-properties (current-kill 0)))
        beg new-pt)
    (forward-line 0)
    (setq beg (point))
    (insert (format "(message \"%s" obj))
    (setq new-pt (copy-marker (point)))
    (insert (format ": %%S\" %s)\n" obj))
    (indent-region beg (point))
    (goto-char new-pt)))


(defun gpb-comint-delete-last-output ()
  (interactive)
  (let ((inhibit-read-only t))
    (save-excursion
      (comint-previous-prompt 1)
      (delete-region (point)
                     (progn
                       (comint-next-prompt 1)
                       (point))))))

(provide 'gpb-util)
