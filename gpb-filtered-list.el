;; (defvar gpb-fl-prompt "test: ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  TODO: there seem to be some left over *filtered-list* buffers
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'gpb-recent-files)

(defcustom gpb-fl--recent-file-list-symbol nil ;;'gpb-rf-recent-file-list
  "Symbol whose value is a list of recently accessed files.\n
Used by `gpb-find-file-filtered'.")

(defvar gpb-fl--buffer nil)
(defvar gpb-fl--header nil)
(defvar gpb-fl--current-list nil)
(defvar gpb-fl--result nil)

(define-derived-mode gpb-filtered-list-mode fundamental-mode "Filtered List"
  "Major mode used in the buffer which displays the filtered list"
  (toggle-read-only 1)
  (hl-line-mode 1))

(define-key gpb-filtered-list-mode-map "\C-m" 'exit-minibuffer)
(define-key gpb-filtered-list-mode-map "\C-g" 'abort-recursive-edit)
(define-key gpb-filtered-list-mode-map "q" 'abort-recursive-edit)

(defvar gpb-fl--minibuffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map [remap previous-line] 'gpb-fl--previous-line)
    (define-key map [remap next-line] 'gpb-fl--next-line)

    (define-key map [remap gpb-backward-page-1] 'gpb-fl--scroll-up)
    (define-key map [remap gpb-forward-page-1] 'gpb-fl--scroll-down)

    (define-key map [remap scroll-up] 'gpb-fl--scroll-up)
    (define-key map [remap scroll-down] 'gpb-fl--scroll-down)

    (define-key map [remap previous-history-element] 'gpb-fl--previous-line)
    (define-key map [remap next-history-element] 'gpb-fl--next-line)

    (define-key map [(control p)] 'gpb-fl--previous-line)
    (define-key map [(control k)] 'gpb-fl--previous-line)

    (define-key map [(control n)] 'gpb-fl--next-line)
    (define-key map [(control j)] 'gpb-fl--next-line)

    (define-key map [(home)] 'gpb-fl--beginning-of-buffer)
    (define-key map [(end)] 'gpb-fl--end-of-buffer)

    (define-key map [(prior)] 'gpb-fl--scroll-down)
    (define-key map [(next)] 'gpb-fl--scroll-up)
    map)
  "The keymap that is used in the minibuffer during filtered list
interaction.")

(defmacro gpb-fl--define-command (name &rest body)
  "Define a command that is executed in the buffer showing the filtered list"
  (declare (indent 1))
  `(defun ,name ()
     (interactive)
     (let ((win (get-buffer-window gpb-fl--buffer)))
       (with-selected-window win
         ,@body
         (beginning-of-line)
         (hl-line-highlight)))))

(gpb-fl--define-command gpb-fl--previous-line
  (previous-line))

(gpb-fl--define-command gpb-fl--next-line
  (next-line))

(gpb-fl--define-command gpb-fl--beginning-of-buffer
  (beginning-of-buffer))

(gpb-fl--define-command gpb-fl--end-of-buffer
  (end-of-buffer)
  (backward-char))

(gpb-fl--define-command gpb-fl--scroll-up
  (gpb-backward-page-1))

(gpb-fl--define-command gpb-fl--scroll-down
  (gpb-forward-page-1)
  (when (eobp) (backward-char)))


(defun gpb-fl--update-filtered-list (&rest args)
  (let* ((inhibit-read-only t)
         (minibuffer-window (active-minibuffer-window))
         (search-text
          (if minibuffer-window
              (with-current-buffer (window-buffer minibuffer-window)
                (buffer-substring-no-properties
                 (minibuffer-prompt-end) (point-max)))
            "")))
  (with-current-buffer gpb-fl--buffer
    (erase-buffer)
    (setq header-line-format (and gpb-fl--header
                                  `(:propertize
                                    ,(concat gpb-fl--header
                                             (make-string 100 ?\ ))
                                    face fringe)))
    (let ((terms (split-string search-text "\\s +"))
          (first t)
          (width (1- (window-width (get-buffer-window)))))
      (dolist (line gpb-fl--current-list)
        (unless (member nil (mapcar (lambda (x)
                                      (string-match-p (regexp-quote x)
                                                      (or (car-safe line) line)))
                                    terms))
          (insert (propertize (gpb-util-truncate-string
                               (or (car-safe line) line) 'left width)
                              'result (or (cdr-safe line) line)))
          (insert "\n"))))
    (goto-char (point-min))
    (hl-line-highlight))))


(defun gpb-fl--read-choice (prompt list buf-name
                            &optional header map use-one-window)
  "Display the PROMPT to ask user for a selection from LIST.

The list is shown in a completions buffer and is filtered as the
user types.  If HEADER is given, then this header is prepended to
the list of option.  LIST may be a list of strings or an alist
of (display-string . result) cons cells."
  (save-window-excursion
    (when use-one-window (delete-other-windows))
    (setq gpb-fl--header (and header
                             (gpb-util-center-string header (window-width))))
    (when (get-buffer buf-name)
      (message "Warning: %S already exists." (get-buffer buf-name)))

    (setq gpb-fl--buffer (get-buffer-create buf-name)
          gpb-fl--current-list list)

    (set-window-buffer (selected-window) gpb-fl--buffer)
    (with-current-buffer gpb-fl--buffer
      (gpb-filtered-list-mode)
      (add-hook 'kill-buffer-hook 'abort-recursive-edit nil t))
    (gpb-fl--update-filtered-list)

    (add-hook 'minibuffer-setup-hook 'gpb-fl--setup-minibuffer-hook)
    (add-hook 'minibuffer-exit-hook 'gpb-fl--exit-minibuffer-hook)
    (unwind-protect
        (progn
          (setq gpb-fl--result nil)
          ;; It appears that `read-from-minibuffer' contains a
          ;; `save-window-configuration' or `save-excursion'.  As a
          ;; result, you need to examine the value of the point in the
          ;; window showing the filtered list before the function
          ;; `read-from-minibuffer' has had a chance to move the point
          ;; back to it's initial position.  We use
          ;; `gpb-fl--exit-minibuffer-hook' to determine the return
          ;; value which corresponds to the current line in the list
          ;; window.  This value is recorded in `gpb-fl--result'.
          (read-from-minibuffer prompt nil (or map gpb-fl--minibuffer-map))
          gpb-fl--result)
      ;; Remove all hooks and kill buffer
      (remove-hook 'minibuffer-setup-hook 'gpb-fl--setup-minibuffer-hook)
      (remove-hook 'minibuffer-exit-hook 'gpb-fl--exit-minibuffer-hook)
      (with-current-buffer gpb-fl--buffer
        (remove-hook 'kill-buffer-hook 'abort-recursive-edit t))
      (kill-buffer gpb-fl--buffer)
      (setq gpb-fl--buffer nil
            gpb-fl--header nil
            gpb-fl--current-list nil
            gpb-fl--result nil))))

(defun gpb-fl--setup-minibuffer-hook ()
  "Setup the minibuffer to update the filtered list."
  (add-hook 'after-change-functions 'gpb-fl--update-filtered-list nil t))


(defun gpb-fl--exit-minibuffer-hook ()
  "Clean up the minibuffer and kill the list buffer."
  (gpb-log-forms 'gpb-fl--exit-minibuffer-hook '(with-current-buffer gpb-fl--buffer (point)))
  (remove-hook 'after-change-functions 'gpb-fl--update-filtered-list t)
  (setq gpb-fl--result (with-current-buffer gpb-fl--buffer
                        (get-text-property (point) 'result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Better buffer switching
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gpb-switch-buffer-filtered (arg)
  "Switch buffers using a filter list.

We open the buffer in another window when a single universal
argument (see `universal-argument') is given.  When more than one
universal argument is given, we show hidden the buffers whose
names begin with space."
  (interactive "p")
  (let (buf bufs hidden-bufs buf-name buf-file-name)
    (dolist (x (reverse (buffer-list)))
      ;; We copy these strings so that the we may add properties
      ;; without affecting the string.
      (setq buf-name (substring (buffer-name x) 0)
            buf-file-name (ignore-errors (substring (buffer-file-name x) 0)))
      (cond
       ;; ignore
       ((or (string-match "^ " buf-name)
            (string-match "\\*epc con [0-9]+\\*" buf-name))
        (when (= arg 16)
          (let ((buf-name (substring-no-properties buf-name)))
            (add-text-properties 0 (length buf-name)
                                 '(face ((foreground-color . "gray61")))
                                 buf-name)
            (push buf-name hidden-bufs))))
       ;; show filename
       (buf-file-name
        (push (cons (gpb-fl--make-filename-pretty2 buf-file-name) x)
              bufs))
       ;; use buffer name
       (t
        (add-text-properties 0 (length buf-name)
                             '(face ((foreground-color . "gray61")))
                             (substring-no-properties buf-name))
        (push (cons buf-name x) bufs))))
    ;;(setq bufs (sort bufs (lambda (x y) (eq (elt y 0) ?*))))
    (setq buf (gpb-fl--read-choice "Buffer: " (nconc bufs hidden-bufs)
                                   "*Buffers*" "Buffers"))
    (if (= arg 4) (switch-to-buffer-other-window buf) (switch-to-buffer buf))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Better imenu switching
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gpb-filtered-imenu (arg)
  (interactive "P")
  (setq imenu--index-alist nil)
  (save-excursion (font-lock-fontify-region (point-min) (point-max)))
  (let* ((imenu-data (reverse (gpb-filtered-imenu-1
                               (imenu--make-index-alist)))))
    (if arg
        (progn
          (switch-to-buffer-other-window "*imenu*")
          (with-current-buffer "*imenu*"
            (erase-buffer)
            (insert (format "%S" imenu-data))))
      (let ((choice (gpb-fl--read-choice "Index entry: " imenu-data
                                         "*Imenu*" "Index entries")))
        (imenu choice)
        ;; (reposition-window)))))
        (recenter 0))))) ; (truncate (* .1 (window-height))))))))

(defun gpb-filtered-imenu-1 (list &optional prefix)
  (let (;; (prefix (or prefix ""))
        result)
    (mapcar (lambda (x)
              (cond
               ((imenu--subalist-p x)
                (setq result (append
                              (gpb-filtered-imenu-1
                               (cdr x)
                               (if prefix
                                   (concat prefix " / " (car x))
                                 (car x)))
                              result)))
               ((ignore-errors (>= (cdr x) 0))
                (if (string-equal prefix (car x))
                    (push (cons (car x) x) result)
                  (push (cons (concat
                               (if prefix
                                   (propertize
                                    (concat prefix " / ")
                                    'face '((foreground-color . "gray62")))
                                 "")
                               (car x))
                              x)
                        result)))))
            list)
    result))

(defun gpb-filtered-imenu-2 (list &optional prefix)
  (let ((prefix (or prefix "")) result)
    (mapcar (lambda (x)
              (cond
               ((imenu--subalist-p x)
                (setq result (append
                              (gpb-filtered-imenu-2
                               (cdr x)
                               (propertize
                                (concat prefix (car x) " / ")
                                'face '((foreground-color . "gray63"))))
                              result)))
               ((ignore-errors (>= (cdr x) 0))
                (if (string-equal prefix (car x))
                    (push (cons (car x) x) result)
                  (push (cons (concat prefix (car x))
                              x) result)))))
            list)
    result))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Better find file using recent file list
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar gpb-find-file-filtered--show-recent-files t)
(defvar gpb-find-file-filtered--update-view nil)

(defun gpb-fl--make-filename-pretty (file-name)
  (if (string-equal (file-name-nondirectory file-name) "..")
      "[parent directory]"
    (setq file-name (replace-regexp-in-string
                     (concat "^" (regexp-quote (expand-file-name "~"))) "~/"
                     file-name))
    (let ((dir (substring-no-properties (file-name-directory file-name))))
      (add-text-properties 0 (length dir) '(face ((foreground-color . "gray64")))
                           dir)
      (setq file-name (concat dir
                              (let ((f (file-name-nondirectory file-name)))
                                (if (and (not (file-remote-p file-name))
                                         (file-directory-p file-name))
                                    (file-name-as-directory f)
                                  f)))))))

(defun gpb-fl--make-filename-pretty2 (file-name)
  (let ((dir (file-name-directory file-name)))
  (add-text-properties 0 (length dir) '(face ((foreground-color . "gray67")))
                       dir)
  (setq file-name (replace-regexp-in-string
                   (concat "^" (expand-file-name "~"))  "~"
                   file-name)
        file-name (concat dir
                          (let ((f (file-name-nondirectory file-name)))
                            (if (file-directory-p file-name)
                                (file-name-as-directory f)
                              f))))))

(defvar gpb-find-file-filtered-map
  (let ((map (make-keymap)))
    (set-keymap-parent map gpb-fl--minibuffer-map)
    ;; not sure why the following is necessary
    ;; (define-key map [(control h)] 'gpb-fl-delete-backward-char)
    (define-key map [(control r)]
      'gpb-find-file-filtered--only-show-recent-first)
    (define-key map [(control o)] 'gpb-find-file-switch)
    ;; (define-key [remap isearch-backward]
    ;;   'gpb-find-file-filtered--put-recent-first)
    map))

(defun gpb-find-file-switch ()
  "Switch to a regular find file from a filtered list find file."
  (interactive)
  (run-at-time 0 nil '(lambda () (interactive)
                        (run-hooks 'pre-command-hook)
                        (call-interactively 'find-file)
                        (run-hooks 'post-command-hook)))
  (abort-recursive-edit))

(defun gpb-find-file-filtered--only-show-recent-first ()
  (interactive)
  (setq gpb-find-file-filtered--show-files-in-dir nil
        gpb-find-file-filtered--show-recent-files t
        gpb-find-file-filtered--update-view t)
  (exit-minibuffer))

(defun gpb-find-file-filtered (arg &optional dir)
  "Find file by showing filtered list.

With prefix argument, open file in other window"
  (interactive "P")
  (require 'gpb-dired)
  (setq gpb-find-file-filtered--show-files-in-dir t
        gpb-find-file-filtered--show-recent-files t
        gpb-find-file-filtered--update-view nil)
  (let ((dir (or dir default-directory))
        (continue t)
        (omit-regex (rx-to-string `(or ,@(mapcar (lambda (x)
                                                   `(seq ,x string-end))
                                                 dired-omit-extensions))))
        file files)
    (setq dir (replace-regexp-in-string "^bash:" "" dir))
    (while continue
      (setq files nil)
      (dolist (file-name (delete-dups
                          (append
                           (when (and dir
                                      gpb-find-file-filtered--show-files-in-dir)
                             (ignore-errors (directory-files dir t)))
                           (when gpb-find-file-filtered--show-recent-files
                             (symbol-value gpb-fl--recent-file-list-symbol))
                           ;; copy the previous list
                           nil)))
        (when (and (or (file-remote-p file-name) (file-exists-p file-name))
                   (not (equal "." (file-name-nondirectory file-name)))
                   (not (string-match omit-regex file-name))
                   (or (not dired-omit-mode)
                       (not (string-match dired-omit-files
                                          (file-name-nondirectory file-name))))
                   (not (string-match "^#.*#$"
                                      (file-name-nondirectory file-name))))
          (setq pretty-file-name (gpb-fl--make-filename-pretty file-name)
                files (cons (cons pretty-file-name file-name)
                            files))))
      (setq file (gpb-fl--read-choice "Find file: " (reverse files) "*Files*"
                                      "File list" gpb-find-file-filtered-map))
      (if gpb-find-file-filtered--update-view
          (setq gpb-find-file-filtered--update-view nil)
        (if (file-directory-p file)
            (setq dir file
                  continue t
                  gpb-find-file-filtered--show-recent-files nil)
          (setq continue nil)
          (if (member (file-name-extension file) '("pdf" "dvi" "ps" "png"))
              (shell-command (format "xdg-open \"%s\"" (expand-file-name file)))
            (if arg
                (find-file-other-window file)
              (find-file file))))))))

(provide 'gpb-filtered-list)
