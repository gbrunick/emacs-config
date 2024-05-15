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

(defun preserve-mark (symbol)
  "Prevent the function attached to SYMBOL from deactivating the mark" 
  (advice-add symbol :after 'set-deactivate-mark-nil)) 

(defun set-deactivate-mark-nil (&rest r)
  (setq deactivate-mark nil))
                
  
(defun gpb-new-document ()
  (interactive)
  (let ((keymap (make-sparse-keymap))
        (indent "    ")
        (dir default-directory)
        (menu-buffer-name "*new document*"))
    (cl-flet ((make-button
               (lambda (desc name mode)
                 (insert-button
                  desc 'action `(lambda (button)
                                 (let ((new-buf (generate-new-buffer ,name)))
                                   (with-current-buffer new-buf
                                     (,mode)
                                     (setq-local default-directory ,dir))
                                   (switch-to-buffer new-buf)
                                   (kill-buffer ,menu-buffer-name)))))))

      (define-key keymap "\t" 'forward-button)
      (define-key keymap [(backtab)] 'backward-button)
      (define-key keymap "q" 'gpb-kill-buffer)

      (with-current-buffer (get-buffer-create menu-buffer-name)
        (erase-buffer)
        (insert "Create new buffer:\n\n")

        (insert indent)
        (make-button "Text buffer" "*new text buffer*" 'text-mode)
        (insert "    create a new buffer in text-mode\n\n")


        (when (boundp 'ess-version)
          (insert indent)
          (make-button "R buffer" "*new R buffer*" 'R-mode)
          (insert "       create a new buffer in R-mode\n\n"))

        (insert indent)
        (make-button "Python buffer" "*new Python buffer*" 'python-mode)
        (insert "  create a new buffer in python-mode\n\n")

        (insert indent)
        (make-button "LaTeX buffer" "*new lateX buffer*" 'LaTeX-mode)
        (insert "   create a new buffer in LaTeX-mode\n\n")

        (insert indent)
        (make-button "Emacs Lisp buffer" "*new emacs lisp buffer*"
                     'emacs-lisp-mode)
        (insert "   create a new buffer in emacs-lisp-mode\n\n")

        (use-local-map keymap)
        (beginning-of-buffer)
        (forward-button 1))
      (switch-to-buffer menu-buffer-name))))

(provide 'gpb-util)
