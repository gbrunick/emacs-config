(defun load-safe (filename)
  "Issue warnings rather than failing when things fail to load."
  (condition-case-unless-debug err
      (load filename)
    (error (message "Error loading %s: %S" filename err))))

(defvar gpb-grep-history nil
  "Used to record previously entered GREP regular expressions.")

(defcustom gpb-grep-exclude-dirs nil
  "A list of directories to exclude."
  :type '(repeat string))

(defcustom gpb-grep-exclude-files nil
  "List of regular expressions used to exclude files."
  :type '(repeat string))

(defun gpb-grep (dir regex)
  (interactive (list (read-directory-name "Directory: ")
                     (read-from-minibuffer "Regex: "
                                           nil nil nil
                                           'gpb-grep-history)))
  (let* ((compilation-ask-about-save nil)
         (default-directory dir)
         (exclude-dirs (mapconcat (lambda (x)
                                    (format "--exclude-dir=\"%s\" " x))
                                  gpb-grep-exclude-dirs
                                  ""))
         (exclude-files (mapconcat (lambda (x)
                                     (format "--exclude=\"%s\" " x))
                                   gpb-grep-exclude-files
                                   ""))
         (cmd (cl-case (window-system)
                (w32
                 (concat "grep -RinHI "
                         exclude-dirs
                         exclude-files
                         "--line-buffered -E "
                         (format "\"%s\" ." regex)))
                (otherwise
                 (concat "grep -RinH "
                         exclude-dirs
                         exclude-files
                         (format "-E \"%s\" ." regex))))))
    (grep cmd)))

(defun gpb:create-imenu-index-buffer (&optional arg)
  "Use `imenu-create-index-function' to create an index buffer.

The buffer created contains links to the various objects that
are identified by `imenu-create-index-function'."
  (interactive "P")
  (let* ((buf (or (and (boundp 'gpb:reference-buffer)
                       (prog1
                           gpb:reference-buffer
                         (unless (buffer-live-p gpb:reference-buffer)
                           (user-error "Buffer has been deleted"))))
                  (current-buffer)))
         (keymap (make-sparse-keymap))
         (items (with-current-buffer buf
                  (save-excursion
                    (funcall imenu-create-index-function))))
         (src-buffer-point (with-current-buffer buf (point)))
         (bufname (format "Index for %s" (buffer-name buf)))
         (inhibit-read-only t)
         (initial-pt 0)
         item section-name)
    (with-current-buffer (get-buffer-create bufname)
      (erase-buffer)
      (insert (format "\n    =====  %s  =====\n\n" bufname))
      (dolist (item items)
        (cond
         ;; We have a section header and a list of subitems
         ((listp (cdr item))
          (insert (format "    %s\n\n" (car item)))
          (dolist (subitem (cdr item))
            (insert (format "        %s" (car subitem)))
            (make-button (save-excursion (back-to-indentation) (point))
                         (point)
                         'link (cdr subitem)
                         'action (lambda (btn) (gpb:jump-to-index-item :here)))
            (when (< (marker-position (cdr subitem)) src-buffer-point)
              (setq initial-pt (point)))
            (insert "\n"))
          (insert "\n\n"))
         ;; We have a single item
         (t
          (insert (format "    %s " (car item)))
          (make-button (save-excursion (back-to-indentation) (point))
                       (point) 'link (cdr item) 'other-window nil
                       'action (lambda (btn) (gpb:jump-to-index-item :here)))
          (when (< (marker-position (cdr item)) src-buffer-point)
            (setq initial-pt (point)))
          (insert "\n"))))

      (setq-local gpb:reference-buffer buf)
      (define-key keymap "\t" 'forward-button)
      (define-key keymap [(backtab)] 'backward-button)
      (define-key keymap "q" 'quit-window)
      (define-key keymap "g" 'gpb:create-imenu-index-buffer)
      (define-key keymap "o" (lambda () (interactive)
                               (gpb:jump-to-index-item :other-switch)))
      (define-key keymap "\C-o" (lambda () (interactive)
                                  (gpb:jump-to-index-item :other-no-switch)))
      (use-local-map keymap)
      (evil-mode 1)
      (setq buffer-read-only t))

    (with-current-buffer buf
      (setq-local gpb:index-buffer (get-buffer bufname))
      (add-hook 'kill-buffer-hook
                'gpb:create-imenu-index-buffer--kill-buffer-hook
                nil t))

    (unless (string= (buffer-name) bufname)
      (if arg (switch-to-buffer-other-window bufname)
        (switch-to-buffer bufname))
      (with-current-buffer bufname
        (goto-char initial-pt)
        (forward-button 1))
      (recenter))))


(defun gpb:jump-to-index-item (how)
  "This is an implementation detail of `gpb:create-imenu-index-buffer'."
  (interactive)
  (let* ((marker (get-char-property (point) 'link))
         (buf (marker-buffer marker)))
    (when (null buf) (user-error "Buffer has been deleted"))
    ;; If called using the button, switch in the current window.
    ;; Otherwise, show in the other window.
    (cl-case how
      (:here
       (switch-to-buffer buf)
       (goto-char marker)
       (recenter (when (> (window-height) 30) 10)))
      (:other-switch
       (switch-to-buffer-other-window buf)
       (goto-char marker)
       (recenter (when (> (window-height) 30) 10)))
      (:other-no-switch
       (let ((win (display-buffer buf t)))
         (set-window-point win marker)
         (with-selected-window win
           (recenter (when (> (window-height) 30) 10)))))
      (t (error "Invalid how: %S" how)))))


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
  (when (memq (window-system) '(x w32 ns))
    (x-focus-frame (selected-frame))))

(defun gpb-previous-window ()
  (interactive)
  (other-window -1 t)
  (when (memq (window-system) '(x w32 ns))
    (x-focus-frame (selected-frame))))

(define-key emacs-lisp-mode-map "\C-c\C-c" 'gpb-eval-buffer)

(defun gpb-eval-buffer ()
  (interactive)
  (eval-buffer)
  (message "Evaluated %s" (buffer-name)))

(defun gpb-kill-buffer ()
  "Don't ask which buffer to kill.  Just kill the current buffer."
  (interactive)
  (let ((buf-name (buffer-name)))
    (kill-buffer)
    (message "Killed %s" buf-name)))

;; (defun gpb-quit-all ()
;;   (interactive)
;;   (evil-curre

(defun gpb-forward-page-1 (&optional arg)
  "Move to top of page, then scroll up by a single page"
  (interactive "p")
  (setq arg (or arg 1))
  (let* (;; (initial-pt (save-excursion (beginning-of-line) (point)))
         (first-line-pt (save-excursion (move-to-window-line 0)
                                        (end-of-line)
                                        (point)))
         (center-line (truncate (* 0.5 (- (window-height) 1))))
         (center-line-pt (save-excursion (move-to-window-line center-line)
                                         (point)))
         (last-line-pt (save-excursion (move-to-window-line -1)
                                       (point)))
         (next-screen-context-lines 1)
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
       ((< (point) center-line-pt) (move-to-window-line center-line))
       ((< (point) last-line-pt) (move-to-window-line -1))
       (t (scroll-up) (move-to-window-line -1)))
      (gpb-forward-page-1 (1- arg)))
     ;; Moving backward
     ((< arg 0)
      (setq last-command 'previous-line this-command 'previous-line)
      (cond
       ((> (point) (save-excursion (goto-char center-line-pt)
                                   (end-of-line)
                                   (point)))
        (move-to-window-line center-line))
       ((> (point) first-line-pt) (move-to-window-line 0))
       (t (scroll-down) (move-to-window-line 0)))
      (gpb-forward-page-1 (1+ arg))))

    (vertical-motion (cons (or goal-column
                               (truncate (or (car-safe temporary-goal-column)
                                             temporary-goal-column)))
                           0))))

(defun gpb-backward-page-1 ()
  "Move to top of page, or scroll up by a single page"
  (interactive)
  (gpb-forward-page-1 -1))

(defun gpb:delete-path-segment-backwards ()
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

(defun gpb:create-imenu-index-buffer--kill-buffer-hook ()
  (when (and (boundp 'gpb:index-buffer)
             (buffer-live-p gpb:index-buffer))
    (kill-buffer gpb:index-buffer)))

(provide 'gpb-util)
