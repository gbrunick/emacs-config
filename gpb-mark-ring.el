;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  A global minor mode for improved mark ring interaction.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar gpb-mark-ring-global-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap set-mark-command] 'gpb-mring--set-mark-command)
    map))

(define-minor-mode gpb-mark-ring-mode
  "A global minor mode for improved mark ring interaction."
  :global t
  :keymap gpb-mark-ring-global-mode-map)

(defvar gpb-mring--set-mark-command--scope nil "equal to 'local or 'global")

(defvar gpb-mring--set-mark-command--direction 1)

(defvar gpb-mring--overlays nil)
(make-variable-buffer-local 'gpb-mring--overlays)

(defvar gpb-mring--show-marks-post-command-hook--hide-marks nil)


(defun gpb-mark-ring-flash-line ()
  (recenter (/ (window-height) 4))
  (let ((ov (make-overlay (save-excursion (forward-line 0) (point))
                          (save-excursion (forward-line 1) (point)))))
    (overlay-put ov 'face 'region)
    (sit-for 0.15)
    (delete-overlay ov)))


(defun gpb-mring--show-marks (&optional buf temporary)
  (interactive)
  (setq buf (or buf (current-buffer)))
  (with-current-buffer buf
    (gpb-mring--hide-marks buf)
    (let ((count 0) positions)
      (dolist (m (cons (mark-marker) mark-ring))
        (when (> count (/ (length mark-ring) 2.0))
          (decf count (+ (length mark-ring) 2)))
        (incf count)
        (unless (member (marker-position m) positions)
          (push (marker-position m) positions)
          (let* ((str "*") ;;(format "%d" count))
                 (o (make-overlay m (+ (marker-position m) (length str))))
                 ;; If the overlay covers a newline, then we need to
                 ;; show the newline and everything that follows the
                 ;; newline so that the alignment of the next line of
                 ;; text is not disturbed.
                 (tail-str (let ((s (buffer-substring (overlay-start o)
                                                      (overlay-end o))))
                             (while (and (> (length s) 0)
                                         (not (equal (elt s 0) ?\n)))
                               (setq s (substring s 1)))
                             s))
                 (display (concat (propertize str 'face
                                              '(:background "light blue"))
                                  tail-str)))
            (overlay-put o 'display display)
            ;; (overlay-put o 'face '(:background "light grey"))
            (push o gpb-mring--overlays)))))
    (when temporary
      (setq gpb-mring--show-marks-post-command-hook--hide-marks nil)
      (add-hook 'post-command-hook 'gpb-mring--show-marks-post-command-hook
                nil t))))

(defun gpb-mring--show-marks-post-command-hook ()
  (if gpb-mring--show-marks-post-command-hook--hide-marks
      (progn
        (gpb-mring--hide-marks)
        (remove-hook 'post-command-hook
                     'gpb-mring--show-marks-post-command-hook
                     t))
    (setq gpb-mring--show-marks-post-command-hook--hide-marks t)))

(defun gpb-mring--hide-marks (&optional buf)
  (interactive)
  (setq buf (or buf (current-buffer)))
  (with-current-buffer buf
    (dolist (o gpb-mring--overlays) (delete-overlay o))
    (setq gpb-mring--overlays nil)))

(defun remove-duplicates-from-mark-ring (ring-symbol)
  (let (result)
    (dolist (m (symbol-value ring-symbol))
      (if (member m result)
          (move-marker m nil)
        (setq result (cons m result))))
    (set ring-symbol (nreverse result))))

(defun gpb-mring--set-mark-command (&optional arg)
  "Replaces `set-mark-command'"
  (interactive "p")
  (cond
   ;; With multiple arguments: always start popping a global mark forward
   ((>= arg 8)
    (setq gpb-mring--set-mark-command--scope 'global
          gpb-mring--set-mark-command--direction 1)
    (pop-global-mark)
    (gpb-mark-ring-flash-line))

   ;; With single argument and no repetition: start popping local marks
   ((and (>= arg 2) (not (eq last-command 'gpb-mring--set-mark-command)))
    (setq gpb-mring--set-mark-command--scope 'local
          gpb-mring--set-mark-command--direction 1)
    (gpb-pop-mark)
    (gpb-mark-ring-flash-line)
    (gpb-mring--show-marks nil t))

   ;; With repetition, a single arg, and currently popping: reverse
   ;; the direction
   ((and (>= arg 2)
         (eq last-command 'gpb-mring--set-mark-command)
         gpb-mring--set-mark-command--scope)
    (setq gpb-mring--set-mark-command--direction
          (- gpb-mring--set-mark-command--direction))
    (gpb-mring--set-mark-command 1))

   ;; With repetition, a single arg, and not currently popping:
   ;; start popping
   ((and (>= arg 2)
         (eq last-command 'gpb-mring--set-mark-command)
         (null gpb-mring--set-mark-command--scope))
    (setq gpb-mring--set-mark-command--direction 'local
          gpb-mring--set-mark-command--direction 1)
    (gpb-mring--set-mark-command 1))

   ;; With repetition and no args and popping local
   ((and (eq last-command 'gpb-mring--set-mark-command)
         (eq gpb-mring--set-mark-command--scope 'local))
    (gpb-pop-mark gpb-mring--set-mark-command--direction)
    (gpb-mark-ring-flash-line)
    (gpb-mring--show-marks nil t))

   ;; With repetition and no args and popping global
   ((and (eq last-command 'gpb-mring--set-mark-command)
         (eq gpb-mring--set-mark-command--scope 'global)
         (eq gpb-mring--set-mark-command--direction 1))
    (pop-global-mark)
    (gpb-mark-ring-flash-line))

   ;; With repetition and no args and popping global backwards
   ((and (eq last-command 'gpb-mring--set-mark-command)
         (eq gpb-mring--set-mark-command--scope 'global)
         (eq gpb-mring--set-mark-command--direction -1))
    (setq global-mark-ring (nreverse global-mark-ring))
    (pop-global-mark)
    (gpb-mark-ring-flash-line)
    (setq global-mark-ring (nreverse global-mark-ring)))

   ;; When the mark is active and empty
   (mark-active
    (deactivate-mark)
    (unless (window-minibuffer-p) (message "Mark deactivated")))

   ;; Default action (activate mark)
   (t
    (setq gpb-mring--set-mark-command--scope nil
          gpb-mring--set-mark-command--direction 1)
    (set-mark-command nil))))


(defun gpb-pop-mark (&optional n)
  "Move point to mark and pop next mark.

After this command:
    pt, mk, ring(0), ..., ring(-1) = mk, ring(0), ..., ring(-1), pt
This is different than the default pop-to-mark-command which sets
    pt, mk, ring(0), ..., ring(-1) = mk, ring(0), ..., ring(-1), mk
and throws away the current point.

With a prefix argument, pop from the global mark ring.
"
  (interactive "p")
  (if (null (mark t)) (error "Mark is not set."))
  (setq n (or n 1))
  (remove-duplicates-from-mark-ring 'mark-ring)
  (cond
   ((< n 0)
    ;; (message "before: %S %S %S" (point) (mark t) mark-ring)
    (let ((pt (point))
          (last-marker (car (last mark-ring)))
          mark-active)
      (goto-char last-marker)
      (set-marker last-marker nil)
      (setq mark-ring (cons (copy-marker (mark t)) (nbutlast mark-ring)))
      (set-mark pt))
    ;; (message "after: %S %S %S" (point) (mark t) mark-ring)
    (if (< n -1) (gpb-pop-mark (1+ n))
      (message "Unpopped mark")))
   ((> n 0)
    ;; (message "before: %S %S %S" (point) (mark t) mark-ring)
    (let ((pm (copy-marker (point)))
          (m (mark t))
          active-mark)
      (goto-char (mark t))
      (when mark-ring
        (set-mark (car mark-ring))
        (setq mark-ring (cdr mark-ring)))
      (setq mark-ring (nconc mark-ring (list pm)))
      (while (and mark-ring (= (point) pm))
        (goto-char (mark t))
        (set-mark (car mark-ring))
         (setq mark-ring (cdr mark-ring))))
    ;; (message "after: %S %S %S" (point) (mark t) mark-ring)
    (if (> n 1) (gpb-pop-mark (1- n))
      (message "Popped mark"))))
  (deactivate-mark))

(provide 'gpb-mark-ring)
