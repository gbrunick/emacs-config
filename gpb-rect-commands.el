;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This code shows an adjustable rectangle and reads a rectangle command.
;;
;; To show the rectangle, we clone the current buffer and then work on
;; the copy until we are done with the rectangle operation.  We
;; install a top level keymap in a buffer local copy
;; `emulation-mode-map-alists' that remaps the standard movement
;; commands to picture-mode like versions.  It remains in effect until
;; the rectangle command has been read, at which point it is removed.
;; We also install a buffer local post-command-hook to redraw the
;; rectangle after each command.  Once we know the extents of the
;; rectangle and the operation to be performed, we kill the cloned
;; buffer (which destroys the buffer local copy of
;; `emulation-mode-map-alists'), return to the original buffer, and
;; perform the operation.
;;
;; We generally keep the cloned buffer read only.  If the user
;; modifies the cloned buffer, then these changes will be lost when we
;; return to the original buffer which would be confusing.  By keeping
;; the cloned buffer read only, it appears to the user that they may
;; not modify the buffer while "rectangle mode is active".  The
;; functions in this module explicitly inhibit the read only flag so
;; that they may adds spaces to the cloned the buffer to allow
;; `picture-mode' like navigation.
;;
;; The main motivation for cloning the buffer is that it is much
;; easier to highlight of the rectangle when you can freely add spaces
;; the buffer in which the rectangle is displayed, as opposed to
;; trying to add all the spaces using display properties so that you
;; don't disturb the contents of the buffer.  To see this, simply
;; compare the complexity of `gpb-rect--show-rectangle' to
;; `cua--highlight-rectangle'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'rect)
(require 'picture)

(defvar gpb-rect-active nil
  "Like `mark-active' for rectangle mode")

;; The following minor mode has no functionality and is defined only
;; to provide documention to the user when they use `describe-mode'.
(define-minor-mode gpb-rectangle-mode
  "A minor mode for showing and acting on the region as a rectangle.

\\{gpb-rectangle-map}")

(defvar gpb-rectangle-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map [(x)] 'gpb-rect--kill-rectangle)
    ;; (define-key map [(v)] 'gpb-rect--yank-rectangle)
    ;; (define-key map [(d)] 'gpb-rect--delete-rectangle)
    ;; (define-key map [(c)] 'gpb-rect--copy-rectangle)
    ;; (define-key map [(o)] 'gpb-rect--exchange-point-and-mark)
    ;; (define-key map [(r)] 'gpb-rect--string-rectangle)

    (define-key map [(control x)] 'gpb-rect--kill-rectangle)
    (define-key map [(control c)] 'gpb-rect--copy-rectangle)
    (define-key map [(control v)] 'gpb-rect--yank-rectangle)
    ;; ;; (define-key map " " 'gpb-rect--insert-or-overwrite-spaces-in-rectangle)
    (define-key map [(control r)] 'gpb-rect--string-rectangle)
    ;; (define-key map [(control i)] 'gpb-rect--string-rectangle)
    (define-key map [(control g)] 'gpb-rect--cancel-rect)
    (define-key map [(control o)] 'gpb-rect--exchange-point-and-mark)
    (define-key map [(escape)] 'gpb-rect--cancel-rect)
    (define-key map [(control ?\ )] 'gpb-rect--set-mark)

    (define-key map [remap backward-char] 'gpb-rect--move-left)
    (define-key map [remap next-line]     'gpb-rect--move-down)
    (define-key map [remap previous-line] 'gpb-rect--move-up)
    (define-key map [remap forward-char]  'gpb-rect--move-right)
    (define-key map [remap delete-char] 'gpb-rect--delete-rectangle)
    (define-key map [remap delete-backward-char] 'gpb-rect--delete-rectangle)
    (define-key map [remap backward-delete-char-untabify]
      'gpb-rect--delete-rectangle)
    (define-key map [remap self-insert-command] 'gpb-rect--error)
    (define-key map [remap set-mark-command] 'gpb-rect--set-mark)

    ;; The following is no longer necessary.
    ;; (define-key map [t] 'gpb-rect--default)
    map))

(defun gpb-rect--begin-rect-command (arg)
  "Beginning showing the rectangle.

With a prefix argument, allow free movement of the cursor, but do
not active the rectangle."
  (interactive "P")
  ;; (unless (or mark-active arg) (push-mark))
  (let ((orig-buf (current-buffer))
        (new-buf (let ((buffer-file-name nil))
                   (clone-buffer (concat (buffer-name) " [Rect]"))))
        (pt (point))
        (m (if (use-region-p) (mark) (point)))
        (win-start (window-start)))

    (ignore-errors (deactivate-mark))
    (switch-to-buffer new-buf)
    (setq buffer-offer-save nil)
    (remove-hook 'kill-buffer-query-functions 'secdb-kill-buffer-query-function t)

    (setq buffer-read-only t buffer-auto-save-file-name nil)
    (let ((alists emulation-mode-map-alists))
      (set (make-local-variable 'emulation-mode-map-alists)
           (append `(((t . ,gpb-rectangle-map))) alists)))

    (set (make-local-variable 'gpb-rect--orig-buffer) orig-buf)
    (set (make-local-variable 'gpb-rect--save-rectangle-overlays) nil)
    (set (make-local-variable 'backup-inhibited) t)
    (set (make-local-variable 'gpb-rect-active) nil)
    ;; We certainly don't want transient-mark-mode as the whole point
    ;; of the cloned buffer is to show the region as a rectangle.
    (set (make-local-variable 'transient-mark-mode) nil)
    ;; Enable documentation for user
    (gpb-rectangle-mode 1)

    (set-window-start nil win-start)
    (goto-char pt)
    (unless arg (gpb-rect--set-mark m))
    (add-hook 'post-command-hook 'gpb-rect--show-rectangle nil t)))


(defun gpb-rect--error ()
  (interactive)
  (gpb-rect--cancel-rect)
  (error "You may not edit the buffer when the rectangle is active"))


(defun gpb-rect--cancel-rect ()
  (interactive)
  (let ((temp-buf (current-buffer))
        (orig-buf gpb-rect--orig-buffer)
        (line (line-number-at-pos))
        (col (current-column))
        (win-start (window-start)))
    (kill-buffer temp-buf)
    (switch-to-buffer orig-buf)
    (goto-line line)
    (move-to-column col)
    (set-window-start nil win-start)))

;  The following command is now obsolete.  Originally I was using
;  `overriding-terminal-local-map' to install the local key map.  This
;  completely replaces all other keymaps, so it is not possible to
;  remap command using this approach.  My initial solution was to
;  write the function `gpb-rect--default' to handle the remapping of
;  command.  I then realized that its much easier to just install a
;  top level keymap in `emulation-mode-map-alists'.  This is much
;  nicer, because emacs now consults all active keymps itself, and I
;  handle the remapping of commands directly in `gpb-rectangle-map'
;  (as god/nature intended).
;; (defun gpb-rect--default ()
;;   "The default command in the overriding rectangle keymap.

;;   We lookup up the command that would be executed if the
;;   rectangle keymap were not active, and then we decide what to do
;;   based upon this command.  This allows us to modify the cursor
;;   movement commands without changing the keybindings set by user.
;;   This approach should be compatible with things like VIM
;;   emulation modes that install completely different keybindings
;;   for cursor movement."
;;   (interactive)
;;   (let (cmd (keys (this-command-keys)))
;;     ;; (message "keys: %S" keys)
;;     (let (remapped-key-sequence (overriding-local-map nil))
;;       (setq remapped-keys (lookup-key local-function-key-map keys)
;;             cmd (or (key-binding keys)
;;                     (let ((remapped-key-sequence
;;                            (lookup-key local-function-key-map keys)))
;;                       (when remapped-keys
;;                         (setq keys remapped-keys)
;;                         (key-binding keys))))))
;;     ;; If the key maps to a keymapp, then we need to read more keys to
;;     ;; determine the command
;;     (when (keymapp cmd)
;;       (let ((overriding-local-map (make-sparse-keymap)))
;;         (set-keymap-parent overriding-local-map cmd)
;;         (define-key overriding-local-map [t] (lambda () (interactive)
;;                                                (error "%s is undefined"
;;                                                       (key-description
;;                                                        this-command))))
;;         (setq cmd (lookup-key cmd (read-key-sequence "") t))))
;;     (cond
;;      ((eq cmd 'backward-char) (gpb-rect--move-left))
;;      ((eq cmd 'next-line)     (gpb-rect--move-down))
;;      ((eq cmd 'previous-line) (gpb-rect--move-up))
;;      ((eq cmd 'forward-char)  (gpb-rect--move-right))
;;      ((member cmd '(delete-char delete-backward-char
;;                                 backward-delete-char-untabify))
;;       (gpb-rect--delete-rectangle))
;;      (t
;;       (when cmd (let ((buffer-read-only t) (overriding-local-map nil)
;;                       (win (selected-window)))
;;                   (set-window-dedicated-p win t)
;;                   (unwind-protect
;;                       (call-interactively cmd)
;;                     (set-window-dedicated-p win nil))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Command for operating on rectangles
;;
;;  We delegate as much as we can to the standard emacs rectangle
;;  commands.  We generally have to use the command which operate on
;;  lines from rect.el, rather than the higher level commands which
;;  operate on regions.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gpb-rect--set-mark (pos)
  "Set the mark to pos without activating transient mark mode."
  (interactive "d")
  (setq pos (or pos (point)))
  (if (and (eq (point) (mark)) gpb-rect-active)
      (setq gpb-rect-active nil)
    (setq gpb-rect-active t)
    ;; Don't use `set-mark'; we don't want to run the hooks in
    ;; `activate-mark-hook' because we are not really "activating the
    ;; mark".  We are "activating the rectangle" which is different
    ;; and not compatible.
    (set-marker (mark-marker) (point))))

;; (defun gpb-rect--set-mark (pos)
;;   (setq gpb-rect-active t)
;;   (set-marker (mark-marker) pos))

(defun gpb-rect--exchange-point-and-mark ()
  "Exchange the point and mark but don't activate the region."
  (interactive)
  (unless gpb-rect-active (error "Mark is not active"))
  (let ((pt (point)))
    (goto-char (mark t))
    (set-marker (mark-marker) pt)))

(defun gpb-rect--move-left ()
  (interactive)
  (let ((inhibit-read-only t))
    (picture-backward-column 1)))

(defun gpb-rect--move-right ()
  (interactive)
  (let ((inhibit-read-only t))
    (picture-forward-column 1)))

(defun gpb-rect--move-down ()
  (interactive)
  (let ((inhibit-read-only t))
    (picture-move-down 1)))

(defun gpb-rect--move-up ()
  (interactive)
  (let ((inhibit-read-only t))
    (picture-move-up 1)))

(defun gpb-rect--apply-to-lines (func &rest args)
  (let ((rect (gpb-rect--current-rectangle)))
    (gpb-rect--cancel-rect)
    (apply 'gpb-rect--apply-to-lines-1 rect func args)))

(defun gpb-rect--string-rectangle ()
  (interactive)
  (let* (;; (overriding-local-map nil)
         (str (read-string "Replace rectangle (default fill with whitespace): "
                           nil nil 'white-space)))
    (if (eq str 'white-space)
        (gpb-rect--apply-to-lines 'clear-rectangle-line t))
    (gpb-rect--apply-to-lines 'string-rectangle-line str t)))

(defun gpb-rect--copy-rectangle ()
  (interactive)
  (let ((lines (list nil)))
    (gpb-rect--apply-to-lines 'extract-rectangle-line lines)
    (setq killed-rectangle (nreverse (cdr lines)))
    (gpb-rect--copy-rectangle-to-kill-ring)))

(defun gpb-rect--kill-rectangle ()
  (interactive)
  (let ((lines (list nil)))
    (gpb-rect--apply-to-lines 'delete-extract-rectangle-line lines nil)
    (setq killed-rectangle (nreverse (cdr lines)))
    (gpb-rect--copy-rectangle-to-kill-ring)))

(defun gpb-rect--delete-rectangle ()
  (interactive)
  (let ((lines (list nil)))
    (gpb-rect--apply-to-lines 'delete-extract-rectangle-line lines nil)
    (setq killed-rectangle (nreverse (cdr lines)))))

(defun gpb-rect--yank-rectangle ()
  (interactive)
  (gpb-rect--cancel-rect)
  (yank-rectangle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  The rectangle highlighting code
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun gpb-rect--current-rectangle ()
  "Returns the list (col1, row1, col2, row2)

These numbers are inclusive."
  (save-excursion
    (let* ((pt (point))
           (mark (mark t))
           (first-line (progn (goto-char (min pt mark)) (line-number-at-pos)))
           (col1 (current-column))
           (last-line (progn (goto-char (max pt mark))
                             (line-number-at-pos)))
           (col2 (current-column)))
      (list (min col1 col2) first-line
            (max col1 col2) last-line))))

(defun gpb-rect--copy-rectangle-to-kill-ring ()
  "Copies the current killed-rectangle into the kill ring."
  (interactive)
  (kill-new (mapconcat (function
                        (lambda (row)
                          (replace-regexp-in-string "[ \t]*$" "\n" row)))
                       killed-rectangle "")))

(defun gpb-rect--apply-to-lines-1 (rect func &rest args)
  "Apply FUNC to each line in the RECT.

RECT is a list of the form (col1, row1, col2, row2) where co11 <=
col2 and row1 <= row2.  FUNC should operate on the current line
and is called like (apply FUNC col1 col2 ARGS)"
  (save-excursion
    (let* ((col1 (nth 0 rect))
           (row1 (nth 1 rect))
           (col2 (nth 2 rect))
           (row2 (nth 3 rect))
           (lines-left (1+ (- row2 row1))))
      (goto-char (point-min))
      (forward-line (1- row1))
      (while (> lines-left 0)
        (move-to-column (min col1 col2) t)
        (setq from (point))
        (move-to-column (max col1 col2) t)
        (apply func col1 col2 args)
        (decf lines-left)
        (forward-line)))))

(setq gpb-rect--save-rectangle-overlays nil)

(defun gpb-rect--show-rectangle ()
  "Show the current rectangle with corners at point and mark.

  This code adds spaces if they are need to show the full
  rectangle.  This greatly simplifies the code.  Compare this
  highlighting code with cua-rect.el

  We keep track of and reuse the overlays in
  `gpb-rect--save-rectangle-overlays'.  This variable is buffer
  local and lives in the cloned buffer used to display the
  rectangle."
  (save-excursion
    (let* ((inhibit-read-only t)
           (old gpb-rect--save-rectangle-overlays)
           ov)
      (when gpb-rect-active
        (setq gpb-rect--save-rectangle-overlays nil)
        (gpb-rect--apply-to-lines-1
         (gpb-rect--current-rectangle)
         (lambda (col1 col2)
           (move-to-column (min col1 col2) t)
           (setq from (point))
           (move-to-column (max col1 col2) t)
           (if old
               (progn (setq ov (pop old))
                      (move-overlay ov from (point)))
             (setq ov (make-overlay from (point)))
             (overlay-put ov 'face 'region))
           (if (equal col1 col2)
               (overlay-put ov 'before-string
                            (propertize " " 'face 'region ;
                                        'display '(space-width 0.3)))
             (overlay-put ov 'before-string ""))
           (push ov gpb-rect--save-rectangle-overlays))))
      (mapcar (function delete-overlay) old))))


(provide 'gpb-rect-commands)
