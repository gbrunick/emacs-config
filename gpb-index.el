;; Code to generate an index for a buffer using `imenu'.

(defvar gpb-index-mode-map 
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\C-m" 'gpb-index-jump-to-marker)
    (define-key keymap "o" 'gpb-index-jump-to-marker-other-window)
    (define-key keymap "q" 'quit-window)
    (define-key keymap "\C-G" 'quit-window)
    (set-keymap-parent keymap button-map)
    keymap))

(with-eval-after-load 'evil
  (evil-make-overriding-map gpb-index-mode-map 'normal))

(define-derived-mode gpb-index-mode special-mode 
  "Index Buffer"
  "\nMode for buffer that shows `imenu' index."
  (setq-local buffer-read-only t))

(defvar-local gpb-index-buffer nil
  "The buffer which contains the index for the current buffer.")

(defvar-local gpb-index-source-buffer nil
  "The buffer which is being indexed.")


(defun gpb-show-index-buffer (&optional arg)
  "Show index buffer.

With a prefix argument we show it another window."
  (interactive "P")
  (let ((source-point (point))
        (index-buf (gpb-index-create-buffer)))
    (if arg (switch-to-buffer-other-window index-buf)
      (switch-to-buffer index-buf))
    (gpb-index-set-point source-point)
    (recenter)))

(defun gpb-index-create-buffer (&optional buf)
  "Use `imenu-create-index-function' to create an index buffer for BUF.

The buffer created contains links to the various objects that
are identified by `imenu-create-index-function'."
  (let* ((src-buf (or buf (current-buffer)))
         (items (with-current-buffer src-buf
                  (save-excursion
                    (funcall imenu-create-index-function))))
         ;; We put the point in the index in a location that corresponds to
         ;; the point in the original buffer.
         (src-buf-point (with-current-buffer src-buf (point)))
         (index-buf (get-buffer-create
                     (format "Index for %s" (buffer-name src-buf))))
         (inhibit-read-only t)
         (initial-pt 0)
         item label marker)

    ;; On the first pass through items, we remove any nesting.
    (setq items (apply #'append
                       (mapcar (lambda (item) (cond
                                               ((listp (cdr item))
                                                (cdr item))
                                               (t
                                                (list item))))
                               items)))

    ;; On the second pass we sort by current market position.
    (setq items (sort items (lambda (x y)
                              (< (marker-position (cdr x))
                                 (marker-position (cdr y))))))
    

    ;; Now we write the buttons into the buffer.
    (with-current-buffer index-buf
      (erase-buffer)
      (gpb-index-mode)
      (setq-local gpb-index-source-buffer src-buf)
      (insert (format "\n  =====  %s  =====\n\n" (buffer-name src-buf)))

      (dolist (item items)
        (setq label (car item)
              marker (cdr item))
        (insert "  ")
        ;; Set the initial point in the index buffer to the last item whose
        ;; marker lies before the point in the source buffer.
        (when (<= marker src-buf-point)
          (setq initial-pt (point)))
        (insert-text-button
         (format "%s" (car item))
         'marker (cdr item)
         'action 'gpb-index-jump-to-marker)
        (insert "\n")))

    ;; Arrange for the source buffer to kill the index buffer when it is
    ;; killed.
    (with-current-buffer src-buf
      (setq-local gpb-index-buffer index-buf)
      (add-hook 'kill-buffer-hook 'gpb-index-mode--kill-buffer-hook nil t))

    index-buf))


(defun gpb-index-set-point (source-buffer-point)
  "Move to the last button with a marker before SOURCE-BUFFER-POINT."
  (goto-char (point-max))
  (while (and (forward-button -1 nil nil t)
              (let ((marker (get-text-property (point) 'marker)))
                (> marker source-buffer-point))))) 

(defun gpb-index-jump-to-marker (&optional button other-window)
  (interactive)
  (let* ((marker (save-excursion
                   (forward-line 0)
                   (forward-button 1)
                   (get-char-property (point) 'marker)))
         (buf (marker-buffer marker))
         win)
    (when (or (null buf) (not (buffer-live-p buf)))
      (user-error "Buffer has been deleted"))

    (cond
     (other-window
      (setq win (display-buffer buf t))
      (set-window-point win marker))
     (t
      (switch-to-buffer buf nil t)
      (setq win (selected-window))
      (goto-char marker)))

    ;; Recent the window around point if it is large enough.
    (with-selected-window win
      (recenter (when (> (window-height) 30) 10)))))


(defun gpb-index-jump-to-marker-other-window ()
  (interactive)
  (gpb-index-jump-to-marker 'no-button 'no-select))


(defun gpb-index-mode--kill-buffer-hook ()
  "Kill index buffer when we kill the source buffer."
  (remove-hook 'kill-buffer-hook 'gpb-index-mode--kill-buffer-hook t) 
  (ignore-error (kill-buffer gpb-index-buffer)))


(provide 'gpb-index)
