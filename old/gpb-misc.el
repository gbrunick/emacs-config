;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  gpb-misc.el
;;
;;  This is just a collection of misc useful (mainly interactive)
;;  functions.  Most of this stuff was pulled out of gpb-init.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ffap)


(defvar gpb-view-file-cache
  (concat temporary-file-directory
          (file-name-as-directory "emacs-file-cache"))
  "Directory used to store local copies of remote files for local viewing.")


(defun as-root ()
  "Close the current buffer and revisit the buffer as root."
  (interactive)
  (if (buffer-modified-p)
      (message "You must save the buffer first.")
    (let ((filename (buffer-file-name)))
      (kill-buffer)
      (find-file (concat "/sudo::" filename)))))

(defun close-all-files ()
  "Close all buffers associated with files or directories"
  (interactive)
  (dolist (b (buffer-list))
    (cond
     ((buffer-file-name b)
      (kill-buffer-if-not-modified b))
     ((with-current-buffer b
        (and (member major-mode '(dired-mode Man-mode help-mode
                                             completion-list-mode
                                             grep-mode reftex-select-label-mode))))
      (kill-buffer b))
     ((with-current-buffer b
        (and (member major-mode '(comint-mode fundamental-mode))
             (not (member (buffer-name b) '("*Messages*" "*scratch*"
                                            "*Pymacs*")))
             (not (buffer-file-name b))
             (or (not (get-buffer-process b))
                 (memq (process-status (get-buffer-process b))
                       '(open run stop)))))
      (kill-buffer b)))))

(defun chmod ()
  "Overide the default which asks for a file name."
  (interactive)
  (let ((file (buffer-file-name)))
    (set-file-modes file (read-file-modes nil file))))

(defun copy-buffer-file-name (arg)
  "Copy the full path of the current buffer to the kill ring
With an argument, copies the TRAMP prefix for a remote file.
"
  (interactive "P")
  (let ((file-name (buffer-file-name)))
    (if file-name
        (progn
          (setq file-name (expand-file-name file-name))
          (unless arg (setq file-name (file-local-name file-name)))
          (kill-new file-name)
          (message "Copied \"%s\" to kill ring" file-name))

      (error "Buffer is not associated with a file"))))

(defun copy-buffer-default-directory ()
  "Copy the full path of the current buffer to the kill ring"
  (interactive)
  (if default-directory
      (let ((dir-name (expand-file-name default-directory)))
          (kill-new dir-name)
          (message "Copied \"%s\" to kill ring" dir-name))
      (error "Buffer is not associated with a directory")))

(defun insert-file-name (filename &optional args)
  "Insert name of file FILENAME into buffer after point.

  Prefixed with \\[universal-argument], expand the file name to
  its fully canocalized path.  See `expand-file-name'.

  Prefixed with \\[negative-argument], use relative path to file
  name from current directory, `default-directory'.  See
  `file-relative-name'.

  The default with no prefix is to insert the file name exactly as
  it appears in the minibuffer prompt.

  Source: http://www.emacswiki.org/emacs/InsertFileName"
  ;; Based on insert-file in Emacs -- ashawley 20080926
  (interactive "*fInsert file name: \nP")
  (cond ((eq '- args)
         (insert (file-relative-name filename)))
        ((not (null args))
         (insert filename))
        (t
         (insert (expand-file-name filename)))))

(defun format-statement-data ()
  (interactive)
  (goto-char (point-min))
  (let ((max-width 0))
    (while (not (eobp))
      (delete-horizontal-space)
      (if (looking-at "[ \t]*$")
          (kill-line)
        (re-search-forward "[^ \t]+")
        (delete-horizontal-space)
        (when (looking-back ",") (delete-backward-char 1))
        ;; (while (looking-at "[ \t]") (delete-char 1))
        (insert "\t")
        (end-of-line)
        (delete-horizontal-space)
        (re-search-backward "[ \t]+")
        (delete-horizontal-space)
        (when (looking-back ",") (delete-backward-char 1))
        ;;(skip-chars-forward " \t")
        (setq max-width (max max-width (current-column)))
        (insert "\t")
        (unless (looking-at "-") (insert " "))
        ;; (while (looking-back "[ \t]") (delete-backward-char 1))
        (forward-line)))
    (goto-char (point-min))
    (while (not (eobp))
      (end-of-line)
      (re-search-backward "\t")
      (insert (make-string (max 0 (- max-width (current-column))) ?\ ))
      (forward-line))))

(defun get-frame-by-outer-id (outer-window-id)
  "Select a frame using the window manager id of the frame."
  (let ((result nil))
    (dolist (frame (frame-list) result)
      (when (equal (frame-parameter frame 'outer-window-id)
                   outer-window-id)
        (setq result frame)))))

(defun mv (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME.

  See: http://stackoverflow.com/questions/384284/can-i-rename-an-open-file-in-emacs"
  (interactive
   (progn
     (if (not (buffer-file-name))
         (error "Buffer '%s' is not visiting a file!" (buffer-name)))
     (list (read-file-name "New name or directory: "))))
  (if (equal new-name "")
      (error "Aborted move file"))
  (setq new-name (expand-file-name new-name))
  (when (file-directory-p new-name)
    (setq new-name (concat (file-name-as-directory new-name)
                           (file-name-nondirectory (buffer-file-name)))))
  ;; If the file isn't saved yet, skip the file rename, but still
  ;; update the buffer name and visited file.
  (if (file-exists-p (buffer-file-name))
      (rename-file (buffer-file-name) new-name 1))
  (let ((was-modified (buffer-modified-p)))
    ;; This also renames the buffer, and works with uniquify
    (set-visited-file-name new-name)
    (if was-modified
        (save-buffer)
      ;; Clear buffer-modified flag caused by set-visited-file-name
      (set-buffer-modified-p nil))
    (message "Moved to %s." new-name)))

(define-derived-mode pdf-outline-mode
  text-mode "PDF Outline"
  "Major mode for editing a pdf outline."
  (setq indent-line-function 'pdf-outline-mode--insert-tab))

(defun new-document ()
  (interactive)
  (let* ((document-type (gpb-popup-menu-of-choices
                         '(("text file" . text-file)
                           ("latex file" . latex-file)
                           ("python file" . python-file)
                           ("lisp interaction" . lisp-interaction)
                           ("lisp file" . lisp-file)
                           ("sage file" . sage-file))
                         "New buffer")))
    (cond
     ((eq document-type 'text-file)
      (find-file (make-temp-file "scratch-" nil ".txt")))
     ((eq document-type 'latex-file)
      (find-file (make-temp-file "scratch-" nil ".tex"))
      ;; (insert-file (locate-library "templates/math.tex" t))
      )
     ((eq document-type 'lisp-interaction)
      (switch-to-buffer (generate-new-buffer "*scratch*"))
      (lisp-interaction-mode))
     ((eq document-type 'python-file)
      (find-file (make-temp-file "scratch-" nil ".py")))
     ((eq document-type 'lisp-file)
      (find-file (make-temp-file "scratch-" nil ".el")))
     ((eq document-type 'sage-file)
      (find-file (make-temp-file "scratch-" nil ".sage")))
     (t
      (error "I don't know how to make a %s buffer."
             document-type)))))

(defun outer-frame-id (&optional frame)
  (setq frame (or frame (selected-frame)))
  (frame-parameter frame 'outer-window-id))

(defun pdf-outline-mode--insert-tab ()
  (insert "\t"))

(defun reb-query-replace-this-regxp (replace)
  "Uses the regexp built with re-builder to query the target buffer.
This function must be run from within the re-builder buffer, not the target
buffer.

Argument REPLACE String used to replace the matched strings in the buffer.
 Subexpression references can be used (\1, \2, etc)."
  (interactive "sReplace with: ")
  (if (eq major-mode 'reb-mode)
      (let ((reg (reb-read-regexp)))
        (select-window reb-target-window)
        (save-excursion
          (beginning-of-buffer)
          (query-replace-regexp reg replace)))
    (message "Not in a re-builder buffer!")))

(defun uncamelcase-region (beg end)
  "Convert CamelCase string S to lower case with word separator SEP.
Default for SEP is a hyphen \"-\".

If third argument START is non-nil, convert words after that
index in STRING."
  (interactive "r")
  (let ((case-fold-search nil))
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (goto-char (point-min))
        (while (re-search-forward "\\([A-Z]?[a-z]+\\)\\([A-Z]\\)" nil t)

          (replace-match (concat (downcase (match-string 1)) "_"
                                 (downcase (match-string 2)))
                         'fixed-case))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  The main functions in the name space
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun gpb-backward-page-1 ()
  "Move to top of page, or scroll up by a single page"
  (interactive)
  (gpb-forward-page-1 -1))

(defun gpb-bury-or-kill-buffer (arg)
  "Bury the current buffer with a message."
  (interactive "P")
  (let ((buf-name (buffer-name)))
    (cond
     (arg
      (kill-buffer)
      (message "Killed %s" buf-name))
     (t
      (bury-buffer)
      (message "Buried %s" buf-name)))))

(defun gpb-cycle-following-space ()
  (interactive)
  (cond
   ;; There is no trailing whitespace.
   ;; ((and (looking-back "\\S ") (looking-at "$"))
   ;;  ;; (if (eq last-command 'gpb-normalize-space)
   ;;  ;;(progn
   ;;    (forward-line)
   ;;    (delete-indentation))
   ;;(message "This is no trailing whitespace")))
   ;; Remove whitespace at the end of a line

   ((and (not (eq last-command 'gpb-cycle-following-space))
         (or (looking-back "\\s +") (looking-at "\\s +$")))

    (if (looking-at "\\s *$")
        (message "Removed trailing whitespace")
      (message "Removed whitespace"))
    (delete-horizontal-space))
   ;;
   ((and (not (eq last-command 'gpb-cycle-following-space))
         (or (looking-back "\\s +") (looking-at "\\s +$")))
    (if (looking-at "\\s *$")
        (message "Removed trailing whitespace")
      (message "Removed whitespace"))
    (delete-horizontal-space))
   ;; If point is at whitespace, remove it
   ((or (looking-back "\\s \\s +")
        (looking-at "\\s \\s +")
        (and (looking-back "\\s ") (looking-at "\\s ")))
    (delete-horizontal-space))
   ;; Reduce lots of trailing space to just one space
   ((and (looking-back "\\S ") (looking-at "\\s \\s +"))
    (just-one-space)
    (backward-char))
   ;; Reduce one space to none
   ((or (and (looking-back "\\s ") (looking-at "\\S "))
        (and (looking-back "\\S ") (looking-at "\\s ")))
    (delete-horizontal-space))
   (t
    (insert-before-markers " ")
    (backward-char))))

(defun gpb-ediff-with-saved ()
  "Run Ediff between the (modified) current buffer and the buffer's file.

A new buffer is created containing the disc file's contents and
`ediff-buffers' is run to compare that with the current buffer.

This code is taken from fx-misc.el by Dave Love"
  (interactive)
  ;; (unless (buffer-modified-p)
  ;;   (error "Buffer isn't modified"))
  (let ((current (buffer-name))
        (file (or (buffer-file-name)
                  (error "Current buffer isn't visiting a file")))
        (mode major-mode)
        ;; ESS workaround.
        (ess-alist (ignore-errors (symbol-value ess-local-customize-alist))))
    (set-buffer (get-buffer-create (generate-new-buffer-name
                                    (concat current "-on-disc"))))
    (buffer-disable-undo)
    (insert-file-contents file)
    (set-buffer-modified-p nil)
    (when ess-alist (setq-local ess-local-customize-alist ess-alist))
    (funcall mode)
    (ediff-buffers (buffer-name) current)))


(defun gpb-edit-reference (&optional key)
  (find-file "~/docs/math/references/references.xml")
  (when key
    (beginning-of-buffer)
    (search-forward key)
    (back-to-indentation)
    (recenter 0)
    (redisplay)))

(defun gpb-erase-buffer ()
  "Ignore the read only text attributes when erasing the buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun gpb-exec-command/eval-expression ()
  (interactive)
  (or (catch 'switch-to-eval-expression
        (let ((temp-map (make-sparse-keymap)))
          ; If we repeat the key sequence used to run this command, we
          ; throw to `eval-expression.
          (define-key temp-map (this-command-keys)
            (lambda ()
              (interactive)
              (throw 'switch-to-eval-expression nil)))
          (ignore-errors (set-transient-map temp-map))
          (call-interactively 'execute-extended-command)
          t))
      (call-interactively 'eval-expression)))

(defun gpb-execute-shell-script ()
  (interactive)
  (save-buffer)
  ;; (let ((buf (get-buffer-create "*script output*")))
  ;;   (display-buffer buf)
  (gpb-compile (format "\"%s\"" (buffer-file-name))
               :comint t
               :buffer-name "*script output*"))


(defun gpb-ffap ()
  (interactive)
  (let ((filename (ffap-prompter)))
    (cond
     ((and (string-match "\\.html$" filename)
           (y-or-n-p "View in browser?"))
      (when (file-remote-p filename)
        (unless (file-exists-p gpb-view-file-cache)
          (make-directory gpb-view-file-cache))
        (let* ((remote-dir (file-name-directory filename))
               (local-dir (file-name-as-directory
                           (make-temp-file gpb-view-file-cache t)))
               (local-file (concat local-dir
                                   (file-name-nondirectory filename))))
          (if (y-or-n-p "Copy surrounding files?")
              (copy-directory remote-dir local-dir t nil t)
            (copy-file filename local-file nil t))
          (setq filename local-file)))
      (browse-url-of-file filename))

     ;; View Excel files
     ((and (string-match "\\.\\(csv\\|xlsx\\|pdf\\)$" filename)
           (y-or-n-p "View locally?"))
      (when (file-remote-p filename)
        (unless (file-exists-p gpb-view-file-cache)
          (make-directory gpb-view-file-cache))
        (let* ((prefix (concat gpb-view-file-cache
                               (file-name-base filename) "_"))
               (ext (concat "." (file-name-extension filename)))
               (local-file (make-temp-file prefix nil ext)))
          (copy-file filename local-file t t)
          (setq filename local-file)))
      (let ((default-directory gpb-view-file-cache)
            (cmd (format "cmd /C \"start %s\"" filename)))
        (message "cmd: %S" cmd)
        (shell-command cmd)))

     (t
      (find-file-at-point filename)))))


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

(defun gpb-goto-line ()
  (interactive)
  (require 'linum)
  (let ((save-linum-mode (if linum-mode 1 -1)))
    (unwind-protect
        (progn (linum-mode 1) (call-interactively 'goto-line))
      (linum-mode save-linum-mode))))

(defun gpb-graphical-frame-p (frame)
  (member (framep frame) '(x w32 ns)))


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


(defun gpb-remap-esc-in-terminal (map)
  "See `gpb-setup-frame-input-maps'"
  (if (and (equal (this-single-command-keys) [?\e])
           (sit-for 0.05))
      "\C-g" map))

(defun gpb-highlight-next-error ()
  (unless (and (boundp 'gpb-highlight-next-error:overlay)
               (overlayp gpb-highlight-next-error:overlay))
    (setq gpb-highlight-next-error:overlay (make-overlay 1 1)))
  (when (and (boundp 'gpb-highlight-next-error:timer)
             (timerp gpb-highlight-next-error:timer))
    (cancel-timer gpb-highlight-next-error:timer))
  (let ((marker (or (get-text-property (point) 'occur-target)
                    (point-marker))))
    ;; (ignore-errors
    ;;   (cadddr
    ;;    (compilation--message->loc
    ;;     (save-excursion
    ;;       (forward-line 0)
    ;;       (get-text-property (point) 'compilation-message))))))))
    (when marker
      (with-current-buffer (marker-buffer marker)
        (save-excursion
          (goto-char marker)
          (move-overlay gpb-highlight-next-error:overlay
                        (progn (forward-line 0) (point))
                        (progn (forward-line 1) (point)))
          (overlay-put gpb-highlight-next-error:overlay 'face 'region)
          (recenter (/ (window-height) 4))))
      (setq gpb-highlight-next-error:timer
            (run-at-time 0.2 nil `(lambda ()
                                    (delete-overlay
                                     gpb-highlight-next-error:overlay)))))))

(defun gpb-back-to-indentation-or-indent-according-to-mode ()
  (interactive)
  ;; (setq this-command 'back-to-indentation)
  (when (>=(point) (progn (back-to-indentation) (point)))
    (back-to-indentation)
    (setq this-command 'indent-according-to-mode)
    (when (eq (point) (progn (indent-according-to-mode) (point)))
      (setq last-command 'indent-according-to-mode)
      (indent-according-to-mode))))

(defun gpb-insert-newline-after ()
  (interactive)
  (save-excursion (insert "\n") (indent-according-to-mode)))

(defun gpb-insert-space-after (arg)
  (interactive "p")
  (when (> arg 0)
    (let ((pt (point)))
      (insert-before-markers (make-string arg ?\s))
      (goto-char pt))))

(defun gpb-kill-buffer ()
  "Don't ask which buffer to kill.  Just kill the current buffer."
  (interactive)
  (let ((buf-name (buffer-name)))
    (when (kill-buffer) (message "Killed %s" buf-name))))

(defun gpb-kill-line (arg)
  (interactive "P")
  (kill-line arg)
  ;; Remove indentation from next line
  (if (or (looking-at "[ \t]*[])}]") (bolp))
      (delete-horizontal-space)
    (just-one-space))
  (delete-trailing-whitespace))
;; (delete-region (match-beginning 0) (match-end 0))
;;   (delete-horizontal-space)
;; (just-one-space)))

(defun gpb-kill-line-backward ()
  (interactive)
  (if (bolp)
      (kill-line -1)
    (kill-line 0)))

(defun gpb-maximize-frame (&optional direction frame)
  "Maximize the current frame.

  DIRECTION can be 'vertical, 'horizontal, or 'both
  "
  (interactive)
  (setq direction (or direction 'both))
  (setq frame (or frame (selected-frame)))
  (let ((window-id (frame-parameter frame 'outer-window-id))
        (command (cond
                  ((eq direction 'both)
                   "add,maximized_vert,maximized_horz")
                  ((eq direction 'vertical) "add,maximized_vert")
                  ((eq direction 'horizontal) "add,maximized_horz")
                  (t (error "gpb-maximize-frame: bad direction.")))))
    (start-process-shell-command "wmctrl" nil "wmctrl" "-i" "-r"
                                 window-id "-b" command)))

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


(defun gpb-popup-menu-of-choices (choices &optional title below-cursor)
  (let ((map (make-sparse-keymap)))
    (dolist (item (reverse choices))
      (define-key map (make-vector 1 (make-symbol
                                      (prin1-to-string (cdr item))))
        (cons (car item)
              (list 'lambda nil '(interactive)
                    (list 'quote (cdr item))))))
    (when title
      (define-key map [title] (list title)))

    ;; Popup the menu at the cursor if this is a not mouse event
    (if (mouse-event-p last-command-event)
        (popup-menu map)
      (let* ((posn-x-y (posn-x-y (posn-at-point)))
             (posn-x (car posn-x-y))
             (posn-y (if below-cursor
                         (+ (cdr posn-x-y) (frame-char-height))
                       (cdr posn-x-y))))
        (popup-menu map (list (list posn-x posn-y) (selected-window)))))))

(defun gpb-refontify-buffer ()
  (interactive)
  (save-excursion
    (font-lock-fontify-region (point-min) (point-max))))

(defun gpb-reload-buffer ()
  (interactive)
  (let ((pt (point)) (file (buffer-file-name)))
    (if file
        (progn
          (kill-buffer)
          (find-file file)
          ;;(find-alternate-file (buffer-file-name))
          (goto-char pt)))))

(defun gpb-setup-frame-input-maps  (&optional frame)
  "Remap a number of keys when in graphical displays"
  (setq frame (or frame (selected-frame)))
  (with-selected-frame frame
    (cond
     ((eq (framep frame) t)
      ;; The following code remaps ESC to \C-g in a terminal without
      ;; breaking all the other keybindings that depend on escaped key
      ;; sequences.  This approach is suggested by Stefan Monnier; see the
      ;; discussion of GNU Emacs bug #13793 for more information.
      (let ((esc-map (lookup-key input-decode-map [?\e])))
        (define-key input-decode-map [?\e]
          `(menu-item "Remap ESC" ,esc-map :filter gpb-remap-esc-in-terminal)))
      ;; We would also like to set escape to be the low level quit-char.
      ;; Unfortunately next command doesn't seem to do anything as of
      ;; Emacs 23.2.1, but the discussion on emacs-dev suggests that this
      ;; may, at some point, be correctly implemented, so we add the code
      ;; anyway.
      (set-quit-char ?\e))

     (t ;; graphical displays
      (define-key input-decode-map [(escape)] [(control g)])

      ;; Now remap the keypad keys
      (define-key input-decode-map [kp-begin] [down])

      (define-key input-decode-map [C-kp-left]  [C-left])
      (define-key input-decode-map [C-kp-right] [C-right])
      (define-key input-decode-map [C-kp-up]    [C-up])
      (define-key input-decode-map [C-kp-down]  [C-down])
      (define-key input-decode-map [C-kp-begin] [C-down])
      (define-key input-decode-map [C-kp-home]  [C-home])
      (define-key input-decode-map [C-kp-end]   [C-end])
      (define-key input-decode-map [C-kp-prior] [C-prior])
      (define-key input-decode-map [C-kp-next]  [C-next])

      (define-key input-decode-map [M-kp-left]  [M-left])
      (define-key input-decode-map [M-kp-right] [M-right])
      (define-key input-decode-map [M-kp-up]    [M-up])
      (define-key input-decode-map [M-kp-down]  [M-down])
      (define-key input-decode-map [M-kp-begin] [M-down])
      (define-key input-decode-map [M-kp-home]  [M-home])
      (define-key input-decode-map [M-kp-end]   [M-end])
      (define-key input-decode-map [M-kp-prior] [M-prior])
      (define-key input-decode-map [M-kp-next]  [M-next])

      (define-key input-decode-map [M-S-kp-left]  [M-left])
      (define-key input-decode-map [M-S-kp-right] [M-right])
      (define-key input-decode-map [M-S-kp-up]    [M-up])
      (define-key input-decode-map [M-S-kp-down]  [M-down])
      (define-key input-decode-map [M-S-kp-begin] [M-down])
      (define-key input-decode-map [M-S-kp-home]  [M-home])
      (define-key input-decode-map [M-S-kp-end]   [M-end])
      (define-key input-decode-map [M-S-kp-prior] [M-prior])
      (define-key input-decode-map [M-S-kp-next]  [M-next])

      (define-key input-decode-map [C-M-kp-left]  [C-M-left])
      (define-key input-decode-map [C-M-kp-right] [C-M-right])
      (define-key input-decode-map [C-M-kp-up]    [C-M-up])
      (define-key input-decode-map [C-M-kp-down]  [C-M-down])
      (define-key input-decode-map [C-M-kp-begin] [C-M-down])
      (define-key input-decode-map [C-M-kp-home]  [C-M-home])
      (define-key input-decode-map [C-M-kp-end]   [C-M-end])
      (define-key input-decode-map [C-M-kp-prior] [C-M-prior])
      (define-key input-decode-map [C-M-kp-next]  [C-M-next])))))

(defun gpb-transpose-words-backwards ()
  (interactive)
  (transpose-words -1))

(defun gpb-try-to-indent-according-to-mode ()
  "Indent according to mode when there are only spaces in front of the point.\n
Returns t on success and nil on failure."
  (interactive)
  (when (looking-back "^[ \t]*")
    (setq this-command 'indent-according-to-mode)
    (indent-according-to-mode)
    t))

(defun gpb-try-to-yas/next-field ()
  (interactive)
  (condition-case exception
      (progn
        (yas/next-field)
        t)
    ('error nil)))

(defun gpb-toggle-outline-minor-mode ()
  "Toggle code folding in buffer"
  (interactive)
  (unless (boundp 'gpb-toggle-folding-mode)
    (define-minor-mode gpb-toggle-folding-mode
      "Minor mode for code folding"
      :keymap '(("\C-i" . outline-toggle-children))))
  (cond
   (gpb-toggle-folding-mode
    (gpb-toggle-folding-mode -1)
    (outline-minor-mode -1))
   (t
    (gpb-toggle-folding-mode 1)
    (outline-minor-mode 1)
    (hide-body))))

(defun gpb-transpose-sexp-backwards ()
  (interactive)
  (transpose-sexps -1))

(defun gpb-unmaximize-frame ()
  "Maximize the current frame.

  DIRECTION can be vertical or horizontal
  "
  (interactive)
  (let ((window-id (frame-parameter (selected-frame) 'outer-window-id)))
    (start-process-shell-command "wmctrl" nil "wmctrl" "-i" "-r"
                                 window-id "-b"
                                 "remove,maximized_vert,maximized_horz")))

(defun gpb-yank (arg)
  (interactive "*P")
  (if (and arg (require 'browse-kill-ring nil t))
      (browse-kill-ring)
    (when (use-region-p)
      (delete-region (region-beginning) (region-end)))
    (yank)))

(defun gpb-today ()
  (require 'calendar)
  (calendar-date-string (calendar-current-date) nil t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  A minor mode for editing contacts xml file
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar gpb-addressbook-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-d" 'gpb-delete-contact)
    (define-key map "\C-c\C-u" 'gpb-update-contact)
    map)
  "Minor mode for editing the addressbook.")

(define-minor-mode gpb-address-book-mode
  "A minor mode for editting the addressbook.xml"
  nil " adr" gpb-addressbook-map)

(defun gpb-delete-contact ()
  (interactive)
  (save-excursion
    (end-of-line)
    (re-search-backward "<contact")
    (goto-char (match-end 0))
    (if (looking-at-p " deleted=")
        (progn (zap-to-char 1 ?>) (insert ">"))
      (insert " deleted=\"true\""))
    (gpb-update-contact)))

(defun gpb-update-contact ()
  (interactive)
  (save-excursion
    (re-search-forward "<lastUpdate\\|</contact")
    (when (looking-back "<lastUpdate") (kill-whole-line))))

(defun gpb-recenter-buffer-display (buf)
  (let ((cur-window (selected-window))
        (recenter-redisplay t))
    (unwind-protect
        (dolist (win (get-buffer-window-list buf))
          (select-window win)
          (with-current-buffer buf
            (let ((window-line (- (line-number-at-pos)
                                  (line-number-at-pos (window-start)))))
              (message "gpb-recenter-buffer-display: %s %s %s"
                       win buf window-line)
              (cond
               ((< window-line (* 0.25 (window-height)))
                (recenter (ceiling (* 0.25 (window-height)))))
               ((> window-line (* 0.75 (window-height)))
                (recenter (floor (* 0.75 (window-height)))))))
            ;; (message "gpb-recenter-buffer-display: %s %s %s"
            ;;          buf win window-line)
            ;; (recenter (ceiling (* 0.25 (window-height))))
            ;; (let ((window-line2 (- (line-number-at-pos)
            ;;                        (line-number-at-pos (window-start)))))
            ;;   (message "gpb-recenter-buffer-display2: %s" window-line2)))
            (let ((ov (make-overlay (save-excursion (beginning-of-line) (point))
                                    (save-excursion (end-of-line) (point)))))
              (overlay-put ov 'window (selected-window))
              (overlay-put ov 'face 'region)
              (redisplay)
              (run-at-time 0.2 nil 'delete-overlay ov))))

      ;; (overlay-put ov 'face 'region)
      ;; (overlay-put ov 'window (selected-window))
      ;; (sit-for 0.1)
      ;; (delete-overlay ov)))
      (message "gpb-recenter-buffer-display2: %s" cur-window)
      (select-window cur-window))))

(defun dedicated-mode ()
  (interactive)
  (set-window-dedicated-p (selected-window) t))

(defun use-cvs ()
  (interactive)
  (setq vc-handled-backends '(CVS RCS Git SVN SCCS Bzr Hg Mtn Arch)))

(defun use-git ()
  (interactive)
  (setq vc-handled-backends '(Git RCS CVS SVN SCCS Bzr Hg Mtn Arch)))


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


(defun gpb:create-imenu-index-buffer--kill-buffer-hook ()
  (when (and (boundp 'gpb:index-buffer)
             (buffer-live-p gpb:index-buffer))
    (kill-buffer gpb:index-buffer)))


(defun gpb:create-imenu-index-buffer-other-window ()
  (interactive)
  (gpb:create-imenu-index-buffer t))

(defun gpb:show-buffer-other-window ()
  "Show the current buffer in another window."
  (interactive)
  (let ((pop-up-windows t)
        (current-window-point (point))
        (current-window-start (window-start)))
    (pop-to-buffer (current-buffer) t)
    (set-window-start (selected-window) current-window-start)
    (set-window-point (selected-window) current-window-point)))


(defun push-mark:set-global-mark-ring-pos (&optional location &rest rest)
  "Always update the global mark ring position.

When I back to the previous buffer using `pop-global-mark', I
want to go to the last position I marked in that buffer, not the
first position that I marked when I initially switched to the
buffer."
  (when (and global-mark-ring (eq (marker-buffer (car global-mark-ring))
                                  (current-buffer)))
    (set-marker (car global-mark-ring) (or location (point)))))

(advice-add 'push-mark :after 'push-mark:set-global-mark-ring-pos)


(defun calendar ()
  (interactive)
  (let ((buf (get-buffer-create "*calendar*"))
        (inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer)
      (read-only-mode 1)
      (view-mode)
      (switch-to-buffer buf)
      (call-process "cal" nil t nil "--year" "2013")
      (call-process "cal" nil t nil "--year" "2014")
      (call-process "cal" nil t nil "--year" "2015")
      (call-process "cal" nil t nil "--year" "2016")
      (call-process "cal" nil t nil "--year" "2017")
      (call-process "cal" nil t nil "--year" "2018")
      (call-process "cal" nil t nil "--year" "2019")
      (save-excursion
        (call-process "cal" nil t nil "--year" "2020")
        (call-process "cal" nil t nil "--year" "2021"))
      (recenter 10))))


(provide 'gpb-misc)

