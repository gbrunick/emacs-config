(require 'tex-site)
(require 'tex)
(require 'latex)
(require 'reftex)
(require 'yasnippet)
(require 'compile)
(require 'company)
(require 'company-dabbrev)
;; (load "preview-latex")

(require 'gpb-util)
(require 'gpb-misc)
;; (require 'gpb-command-chains)
;; (require 'gpb-dropdown-list)
;; (require 'gpb-comint-compile)
(require 'gpb-chunks)
(require 'gpb-settings)

(require 'gpb-text-objects)

;; equation relabling for latex.
(autoload 'aux-renumber-single "aux-renum"
  "Renumber labels in a single-file LaTeX document." t)
(autoload 'aux-renumber-multi "aux-renum"
  "Renumber labels in a multi-file LaTeX document." t)

(gpb-util:add-global-compilation-errors
 `((gpb-tex-error ,(concat "^[Ee]rror:\\([./a-zA-Z0-9]"
                           "[-_./a-zA-Z0-9 ]+\\):?\\([0-9]+\\)?$")
                  1 2)
   (gpb-tex-warning ,(concat "^[Ww]arning:\\([./a-zA-Z0-9]"
                             "[-_./a-zA-Z0-9 ]+\\):?\\([0-9]+\\)?$")
                    1 2 nil 1)
   (gpb-tex-bad-box ,(concat "^Bad Box:\\([./a-zA-Z0-9]"
                             "[-_./a-zA-Z0-9 ]+\\):?\\([0-9]+\\)?$")
                    1 2 nil 1)))

(setq TeX-auto-save t
      TeX-parse-self t
      TeX-debug-warnings t
      TeX-debug-bad-boxes nil
      TeX-newline-function 'newline-and-indent
      TeX-show-compilation nil
      TeX-electric-sub-and-superscript nil
      ;;TeX-source-specials-mode t
      TeX-insert-braces nil
      TeX-save-query nil
      TeX-source-correlate-method 'synctex
      gpb-latex-auto-start-viewer nil
      next-error-recenter nil
      LaTeX-beamer-item-overlay-flag nil
      font-latex-fontify-script nil
      ;; font-latex-fontify-sectioning 'color
      )

;; (push "\\smallskip" LaTeX-paragraph-commands-add-locally
(push ".synctex.gz" completion-ignored-extensions)
;; (push ".pdf" completion-ignored-extensions)

(add-to-list 'TeX-expand-list '("%u" okular-make-url))
;; (add-to-list 'TeX-expand-list
;; 	       '("%q" skim-make-url))))

(add-to-list 'LaTeX-auto-regexp-list
             `(,(concat "\\\\autoreftheorem{\\(" TeX-token-char "+\\)}")
               1 LaTeX-auto-environment))

(defun okular-make-url ()
  (concat "'file://"
          (replace-regexp-in-string
           ;; Break out of single quotes, insert single quote, then go
           ;; back into single quotes
           "'" "'\\''"
           (expand-file-name (funcall file (TeX-output-extension) t)
                             (file-name-directory (TeX-master-file)))
           nil t)
          "#src:"
          (TeX-current-line)
          (TeX-current-file-name-master-relative)
          "'"))

;; (defun okular-make-url ()
;;   (concat (funcall file (TeX-output-extension) t)
;;           ;;"file:///"
;;           ;; (expand-file-name (funcall file (TeX-output-extension) t)
;;           ;;                   (file-name-directory (TeX-master-file)))
;;           " "
;;           (TeX-current-line)
;;           " "
;;           (TeX-current-file-name-master-relative)
;;           ))

;; (defun okular-make-url ()
;;   (funcall file (TeX-output-extension) t))
;; (add-to-list 'TeX-view-program-list '("Okular" "tabbed-okular %u"))

;; (defun skim-make-url () (concat
;; 		(TeX-current-line)
;; 		" "
;; 		(expand-file-name (funcall file (TeX-output-extension) t)
;; 			(file-name-directory (TeX-master-file)))
;; 		" "
;; 		(buffer-file-name)))

;; (setq TeX-view-program-list nil)
(add-to-list 'TeX-view-program-list '("Okular" "okular --unique %u"))
;;(add-to-list 'TeX-view-program-list '("Okular" "refresh-okular %u"))
(add-to-list 'TeX-view-program-list '("Oktabus" "oktabus %u"))

(cond
 ((string-equal system-type "gnu/linux")
  (setq TeX-view-program-selection
        '((output-pdf "Oktabus") (output-dvi "Oktabus")))))

(setq-default TeX-auto-regexp-list 'TeX-auto-full-regexp-list)
;; (push (list "^pdf$" "." "okular %o") TeX-output-view-style)
;; (push (list "^pdf$" "." "evince %o") TeX-output-view-style)
;; (push (list "^pdf$" "." "acrobat %o") TeX-output-view-style)

(defun gpb-latex-show-settings-buffer ()
  (interactive)
  (gpb-settings:make-settings-buffer 'gpb-latex-setup-settings-buffer))

(let ((map LaTeX-mode-map))
  (define-key map [(control c)(control c)] 'gpb-latex-compile-document)
  (define-key map "\t" 'gpb-latex-tab-command)
  ;; (define-key map  [(control ?\;)] 'gpb-latex-toggle-entering-math-mode)
  ;; (define-key map [(control c)(control ?\;)] 'comment-dwim)
  (define-key map [(control c)(l)] 'gpb-latex-show-log)
  (define-key map [(control return)] 'gpb-latex-insert-item)
  (define-key map [(control c)(n)] 'TeX-next-error)
  (define-key map [(meta tab)] 'gpb-latex-do-completion)
  (define-key map "\C-p" 'gpb-latex-show-settings-buffer)
  (define-key map [(control =)] 'gpb-try-to-grow-delimiter-size)
  (define-key map [(control -)] 'gpb-try-to-shrink-delimiter-size)
  ;; This conficts with my global bindings
  ;; (define-key map [(meta control m)] nil)

  ;; (define-key map [(control meta left)] 'gpb-latex-find-matching-begin)
  ;; (define-key map [(control meta right)] 'gpb-latex-find-matching-end)
  ;; (define-key map [(meta a)] 'gpb-latex-find-matching-begin)
  ;; (define-key map [(meta e)] 'gpb-latex-find-matching-end)
  ;; (define-key map [(control kp-prior)] 'gpb-latex-previous-environment)
  ;; (define-key map [(control kp-next)] 'gpb-latex-next-environment)
  ;; (define-key map [(meta kp-prior)] 'gpb-latex-previous-environment)
  ;; (define-key map [(meta kp-next)] 'gpb-latex-next-environment)
)


;; (defun gpb-latex-beginning-of-env-or-item ()
;;   (interactive)
;;   (let ((points
;;          (remove nil (list (ignore-errors
;;                              (save-excursion
;;                                (LaTeX-find-matching-begin)
;;                                (unless (looking-at "[ \t]*\\\\begin{document}")
;;                                  (point))))
;;                            (when (member (LaTeX-current-environment)
;;                                          '("enumerate" "itemize" "descritpion"
;;                                            "compactenum" "compactitem"))
;;                              (ignore-errors
;;                                (save-excursion
;;                                  (backward-char)
;;                                  (re-search-backward "\\\\item")
;;                                  (point))))))))))


;;     (when points (goto-char (max points)))))

;;   (goto-char (min (point)
;;                   (max (or (ignore-errors
;;                              (save-excursion
;;                                (LaTeX-find-matching-begin)
;;                                (unless (looking-at "[ \t]*\\\\begin{document}")
;;                                  (point))))
;;                            (point-min))
;;                        (if (member (LaTeX-current-environment)
;;                                    '("enumerate" "itemize" "descritpion"
;;                                      "compactenum" "compactitem"))
;;                            (or (save-excursion
;;                                  (backward-char)
;;                                  (when (re-search-backward "\\\\item" nil t)
;;                                    (point)))
;;                                (point-min))
;;                          (point-min)))))

;; (defun gpb-latex-end-of-env-or-item ()
;;   (interactive)
;;   (goto-char (max (point)
;;                   (min (or (ignore-errors
;;                              (save-excursion
;;                                (LaTeX-find-matching-end)
;;                                (point)))
;;                            (point-max

;; (forward-latex-environment))
;;                     (point))
                  ;; (save-excursion
                  ;;   (forward-line)
                  ;;   (forward-char)
                  ;;   (or (and (re-search-forward (LaTeX-outline-regexp) nil t)
                  ;;            (progn (forward-line -1) (point)))
                  ;;       (point-max)))
;;                   (if (member (LaTeX-current-environment)
;;                               '("enumerate" "itemize" "descritpion"
;;                                 "compactenum" "compactitem"))
;;                       (min ;; End at next item
;;                            (save-excursion
;;                              (when (looking-at "\\\\item")
;;                                (goto-char (match-end 0)))
;;                              (if (re-search-forward "\\\\item" nil t)
;;                                  (point)
;;                                (point-max)))
;;                            ;; or the close of the environment
;;                            (save-excursion
;;                              (forward-latex-environment)
;;                              (match-beginning 0)))
;;                     (point-max))
;;                   (if (member (LaTeX-current-environment)
;;                               '("enumerate" "itemize" "descritpion"
;;                                 "compactenum" "compactitem"))
;;                       (save-excursion
;;                         (forward-char)
;;                         (or (and (re-search-forward "\\\\item" nil t)
;;                                  (match-beginning 0))
;;                             (point-max)))
;;                     (point-max))
;; (forward-latex-environment)
;;                   )))


(defun gpb-latex-previous-environment ()
  (interactive)
  (unless (eq last-command 'gpb-latex-previous-environment)
    (push-mark))
  (re-search-backward "\\\\end{[^}]+}")
  (LaTeX-find-matching-begin)
  (recenter (round (* 0.25 (window-height))))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;  Movement commands
;;
;;    These commands make 'latex-environment and 'latex-item
;;    work with thing-at-point commands
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun gpb-latex-type-of-thing-at-point (arg)
  (interactive "P")
  (cond
   ((looking-at-p "[ \t]*\\\\begin")
    'latex-environment)
   ((looking-at-p "[ \t]*\\\\item")
    'latex-item)))

;; For thing-at-pt compatibility
(defvar gpb-latex-env-regex "\\\\\\(begin\\|end\\)[ \t]*{\\([^}\n]+\\)}")

(defun forward-latex-environment (&optional arg include-list exclude-list)
  "Move forward past ARG environments.  ARG defaults to 1.
Negative ARG moves in front of -ARG environments.

Returns the type of environment moved past/in front of.
If INCLUDE-LIST is given, only environments in the list are count.
If EXCLUDE-LIST is given, only environments in this list do not count.
If an environment appears in both lists, it is excluded.
"
  (interactive "p")
  (setq arg (or arg 1))
  (cond
   ;; Delegate for moving backward
   ((< arg 0)
    (let ((initial-pt (point))
          (cur-env-bounds (gpb-latex-bounds-of-env-at-point)))
      (when cur-env-bounds (goto-char (car cur-env-bounds)))
      (when (>= (point) initial-pt)
        (unless (re-search-backward "\\\\end" nil t)
          (goto-char initial-pt)
          (error "No previous latex enironments"))
        (goto-char (car (gpb-latex-bounds-of-env-at-point)))
        (forward-latex-environment (1+ arg)))))
   ;; Moving forward
   ((> arg 0)
    (let ((initial-pt (point))
          (cur-env-bounds (gpb-latex-bounds-of-env-at-point)))
      (if (and cur-env-bounds (< (point) (cdr cur-env-bounds)))
          ;; Moving to this end of this environent is after point
          (goto-char (cdr cur-env-bounds))
        (unless (re-search-forward "\\\\begin" nil t)
          (goto-char initial-pt)
          (error "No more latex environments"))
        (goto-char (cdr (gpb-latex-bounds-of-env-at-point)))
        (forward-latex-environment (1- arg)))))))

(defun gpb-latex-bounds-of-env-at-point ()
  (interactive)
  (save-excursion
    (cond
     ((looking-at "\\\\begin")
      (cons (point)
            (progn (goto-char (match-end 0))
                   (LaTeX-find-matching-end)
                   (point))))
     ((looking-back "\\\\end{[^}\n]+}")
      (cons (progn (goto-char (match-beginning 0))
                   (LaTeX-find-matching-begin)
                   (point))
            (progn (assert (looking-at "\\\\begin"))
                   (goto-char (match-end 0))
                   (LaTeX-find-matching-end)
                   (point))))
     (t
      (cons (progn (LaTeX-find-matching-begin)
                   (point))
            (progn (assert (looking-at "\\\\begin"))
                   (goto-char (match-end 0))
                   (LaTeX-find-matching-end)
                   (point)))))))

(put 'latex-environment 'bounds-of-thing-at-point
     'gpb-latex-bounds-of-env-at-point)

(defun gpb-latex-bounds-of-outer-env-at-point ()
  "The bounds of the env which contains the env which contains the point."
  (interactive)
  (ignore-errors
    (save-excursion
      (cond
       ((looking-at "\\\\begin")
        (LaTeX-find-matching-begin)
        (gpb-latex-bounds-of-env-at-point))
       ((looking-back "\\\\end{[^}\n]+}")
        (LaTeX-find-matching-end)
        (gpb-latex-bounds-of-env-at-point))
       (t
        (LaTeX-find-matching-begin)
        (LaTeX-find-matching-begin)
        (gpb-latex-bounds-of-env-at-point))))))

(put 'latex-outer-environment 'bounds-of-thing-at-point
     'gpb-latex-bounds-of-outer-env-at-point)

(defvar gpb-latex-item-environments
  '("enumerate" "itemize" "descritpion" "compactenum" "compactitem")
  "A list of environments that contain items")

(makunbound 'forward-latex-item)
(makunbound 'gpb-latex-bounds-of-item-at-point)

;; (defun forward-latex-item (&optional arg)
;;   (interactive "p")
;;   (setq arg (or arg 1))
;;   (cond
;;    ((< arg 0)
;;     (let ((initial-pt (point))
;;           (cur-item-bounds (gpb-latex-bounds-of-item-at-point))
;;           (begin-list-env
;;            (ignore-errors
;;              (save-excursion
;;                (progn
;;                  (while (not (member (LaTeX-current-environment)
;;                                      gpb-latex-item-environments))
;;                    (LaTeX-find-matching-begin))
;;                  (LaTeX-find-matching-begin)
;;                  (point))))))

;;       (unless begin-list-env
;;         (error "Not inside a list environment"))
;;       (when cur-item-bounds (goto-char (car cur-item-bounds)))
;;       (when (>= (point) initial-pt)
;;         (unless (and (re-search-backward "\\\\item" nil t)
;;                      (> (point) begin-list-env))
;;           (goto-char initial-pt)
;;           (error "No more items in this list environment")))
;;       ;;(goto-char (car (gpb-latex-bounds-of-item-at-point)))
;;       (forward-latex-item (1+ arg))))
;;    ((> arg 0)
;;     (let ((initial-pt (point))
;;           (cur-item-bounds (gpb-latex-bounds-of-item-at-point))
;;           (end-list-env
;;            (ignore-errors
;;              (save-excursion
;;                (progn
;;                  (while (not (member (LaTeX-current-environment)
;;                                      gpb-latex-item-environments))
;;                    (LaTeX-find-matching-begin))
;;                  ;; (when (looking-at "\\\\begin")
;;                  ;;   (goto-char (region-end 0)))
;;                  (LaTeX-find-matching-end)
;;                  (point))))))

;;       (unless end-list-env
;;         (error "Not inside a list environment"))
;;       (if (and cur-item-bounds (< (point) (cdr cur-item-bounds)))
;;           (goto-char (cdr cur-item-bounds))
;;         (unless (and (re-search-forward "\\\\item" nil t)
;;                      (< (point) end-list-env))
;;           (goto-char initial-pt)
;;           (error "No more items in this list environment"))
;;         (goto-char (cdr (gpb-latex-bounds-of-item-at-point)))
;;         (forward-latex-item (1- arg)))))))

;; (defun gpb-latex-bounds-of-item-at-point ()
;;   (let ((bounds
;;          (ignore-errors
;;            (save-excursion
;;              (while (not (member (LaTeX-current-environment)
;;                                  gpb-latex-item-environments))
;;                (LaTeX-find-matching-begin))
;;              (cons (if (save-excursion (skip-chars-backward "\\\\item")
;;                                        (looking-at "\\\\item"))
;;                        (match-beginning 0)
;;                      (save-excursion (re-search-backward "\\\\item")
;;                                        (point)))
;;                    (progn
;;                      (when (looking-at "\\\\item")
;;                        (goto-char (match-end 0)))
;;                      (apply 'min
;;                             (remove nil (list (save-excursion
;;                                                 (LaTeX-find-matching-end)
;;                                                 (assert (looking-back
;;                                                          "\\\\end{[^}\n]+}"))
;;                                                 (goto-char (match-beginning 0))
;;                                                 (skip-chars-backward " \t\n")
;;                                                 (point))
;;                                               (ignore-errors
;;                                                 (re-search-forward "\\\\item")
;;                                                 (goto-char (match-beginning 0))
;;                                                 (skip-chars-backward " \t\n")
;;                                                 (point)))))))))))
;;     (when (and bounds
;;                (<= (car bounds) (point))
;;                (<= (point) (cdr bounds)))
;;       bounds)))
(defun forward-latex-item (arg)
  (interactive "p")
  (cond
   ((and (< arg 0) (or (and (looking-at "m") (looking-back "\\\\ite"))
                       (and (looking-at "em") (looking-back "\\\\it"))
                       (and (looking-at "tem") (looking-back "\\\\i"))
                       (and (looking-at "item") (looking-back "\\\\"))))
    (goto-char (match-beginning 0))
    (when (looking-back "^[ \t]+")
      (skip-chars-backward " \t"))
    (forward-latex-item (1+ arg)))

   ((< arg 0)
    (let* ((env-beg (save-excursion
                      (while (not (member (LaTeX-current-environment)
                                          gpb-latex-item-environments))
                        (LaTeX-find-matching-begin))
                      (LaTeX-find-matching-begin)
                      (point)))
           (item-beg (save-excursion
                         (catch 'done
                           (while (and (> (point) env-beg)
                                       (search-backward "\\item" env-beg t))
                             (when (and (not (TeX-in-comment))
                                        (eq env-beg (save-excursion
                                                      (LaTeX-find-matching-begin)
                                                      (point))))
                               (skip-chars-backward " \t")
                               (throw 'done (point))))))))
      (when (not (null item-beg))
        (goto-char item-beg)
        (forward-latex-item (1+ arg)))))

   ((> arg 0)
    (let* ((env-end (save-excursion
                        (while (not (member (LaTeX-current-environment)
                                            gpb-latex-item-environments))
                          (LaTeX-find-matching-end))
                        (LaTeX-find-matching-end)
                        (point)))
           (item-end (save-excursion
                       (goto-char env-end)
                       (re-search-backward "\\\\end{[^}+]")
                       (when (looking-back "^[ \t]*") (forward-line 0))
                       (point))))
      (save-excursion
        (while (looking-at "[ \t]*$") (forward-line 1))
        (when (looking-at "[ \t]*\\\\item") (goto-char (match-end 0)))
        (while (and (< (point) item-end)
                    (search-forward "\\item" item-end t))
          (when (and (not (TeX-in-comment))
                     (eq env-end (save-excursion (LaTeX-find-matching-end)
                                                 (point))))
            (assert (looking-back "\\\\item"))
            (goto-char (match-beginning 0))
            (setq item-end (point)))))
      (goto-char item-end)
      (when (looking-back "^[ \t]+") (forward-line 0))
      (while (looking-back "^[ \t]*\n") (forward-line -1))
      (forward-latex-item (1- arg))))))

(defun backward-latex-item (arg)
  (interactive "p")
  (forward-latex-item (- arg)))

(put 'latex-item 'bounds-of-thing-at-point
     'gpb-latex-bounds-of-item-at-point-1)


;; (defun backward-latex-item (&optional arg)
;;   (interactive "p")
;;   (setq arg (or arg 1))
;;   (cond
;;    ((arg < 0)
;;     (forward-latex-item (- arg)))
;;    ((arg > 0)
;;     (unless (member (LaTeX-current-environment) gpb-latex-item-environments)
;;       (signal 'scan-error "Not in list environment"))
;;     (goto-char (max ;; End at next item
;;                 (save-excursion
;;                   (if (re-search-backward "\\\\item" nil t)
;;                       (point)
;;                     (point-min)))
;;                 ;; or the close of the environment
;;                 (or (ignore-errors
;;                       (save-excursion
;;                         (backward-LaTeX-environment
;;                          1 gpb-latex-item-environments)
;;                         (point)))
;;                     (point-min))))
;;     (skip-chars-forward " \t\n")
;;     (when (> arg 1) (backward-latex-item (1- arg))))))

;; (defun gpb-latex-mark-env-or-item-or-section ()
;;   (cond
;;    ((looking-at "[ \t]*\\\\begin{")
;;     (goto-char (match-end 0))
;;     (LaTeX-mark-environment)
;;     (beginning-of-line)
;;     (exchange-point-and-mark)
;;     t)
;;    ((looking-at "[ \t]*\\\\item ")
;;     (let ((end-of-env (save-excursion (LaTeX-find-matching-end)
;;                                       (beginning-of-line)
;;                                       (point))))
;;       (set-mark
;;        (save-excursion
;;          (end-of-line)
;;          (or (re-search-forward "\\\\item" end-of-env t) end-of-env)))
;;       (beginning-of-line)
;;       (activate-mark))
;;      t)
;;     ((looking-at "\\\\\\(\\(sub\\)*section\\|chapter\\){")
;;      (goto-char (match-end 0))
;;     (LaTeX-mark-section)
;;     (beginning-of-line)
;;     (exchange-point-and-mark)
;;     t)
;;    (t
;;     nil)))


(add-hook 'LaTeX-mode-hook 'gpb-init-LaTeX-buffer)

(defun gpb-init-LaTeX-buffer ()
  "Initialize a LaTeX-mode buffer."
  (interactive)
  (message "gpb-init-LaTeX-buffer...")
  (TeX-PDF-mode-on)
  (LaTeX-math-mode 1)
  ;;(imenu-add-menubar-index)
  (company-mode 1)
  (set (make-local-variable 'normal-auto-fill-function)
       'gpb-latex-do-auto-fill)
  (set (make-local-variable 'fill-paragraph-function)
       'gpb-latex-fill-paragraph-function)
  (set (make-local-variable 'ispell-check-comments) nil)
  (set (make-local-variable 'gpb-latex-label-prefix) "")
  ;;(gpb-latex-set-label-prefix)
  (auto-fill-mode 1)
  (setq indent-line-function 'gpb-latex-indent-line)
  (LaTeX-paragraph-commands-add-locally '("smallskip" "medskip" "bigskip"))
  ;; synctex support Needs auctex version >= .86
  ;; see http://www.bleedingmind.com/index.php/2010/06/17/synctex-on-linux-and-mac-os-x-with-emacs/
  ;;(setq)
  (TeX-source-correlate-mode 1)
  (gpb-latex-process-local-style-files)

  (setq gpb-goto-beginning--function
        'gpb-latex-beginning-of-env-or-item)
  (setq gpb-goto-end--function
        'gpb-latex-end-of-env-or-item)

  (setq gpb-reg-mark-thing--mark-thing-function
        'gpb-latex-mark-env-or-item-or-section)

  (setq gpb-reg--local-marking-alist nil)
  ;; (add-to-list 'gpb-reg--local-marking-alist
  ;;              '(?s latex-environment "[s]ection"))
  (add-to-list 'gpb-reg--local-marking-alist
               '(?e latex-environment "[e]vironment"))
  (add-to-list 'gpb-reg--local-marking-alist
               '(?i latex-item "[i]tem"))

  (setq gpb-goto-beg/end--thing-list
        '(line latex-environment latex-item latex-outer-environment))

  (setq gpb-reg-mark-thing--get-thing-function
        'gpb-latex-type-of-thing-at-point)

  (setq gpb-reg-move-by-thing--get-thing-function
        'gpb-latex-type-of-thing-at-point)

  ;; (gpb-tobj--define-thing-text-object ([(hyper i)] . [(hyper shift i)])
  ;;                                     latex-item t)
  ;; (gpb-tobj--define-inner/outer-text-object "$" nil "\\$" "\\$" inline-math t)
  ;; (gpb-tobj--define-inner/outer-text-object
  ;;  "e" "E" "\\\\begin{[^}]*}" "\\\\end{[^}]*}" gpb-latex-environment t)

  ;;      'LaTeX-mark-environment
  ;; Company code dabbrev setup
  ;; (set (make-variable-buffer-local company-dabbrev-other-buffers) nil)
  ;; (set (make-variable-buffer-local company-dabbrev-char-regexp)
  ;;      "\\w\\|\\s_")
  (setq TeX-complete-list (gpb-util-remove-items
                           TeX-complete-list
                           (lambda (item)
                             (eq (cadr item) 'ispell-complete-word))))

  ;; Overload some keys
  ;; (gpb-add-command-to-chain [(control left)]
  ;;                           'gpb-try-change-delimiter-type-reverse)
  ;; (gpb-add-command-to-chain [(control right)]
  ;;                           'gpb-try-change-delimiter-type)
  ;; (gpb-add-command-to-chain [(control up)]
  ;;                           'gpb-try-to-change-environment-backwards)
  ;; (gpb-add-command-to-chain [(control down)]
  ;;                           'gpb-try-to-change-environment)

  ;; setup reftex
  (turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t)

  ;; Make yasnippet key search stop at the math boundary.
  (make-variable-buffer-local 'yas/key-syntaxes)
  (setq yas/key-syntaxes (append yas/key-syntaxes '("^$")))

  ;; We put backslashes in the punctuation syntax class (so they don't excape
  ;; delimiters) and we put asterisks in the symbol syntax class.  We
  ;; would still like backslashes to escape dollar signs and percent
  ;; signs, so we update font-latex-syntactic-keywords to make this
  ;; happen.  In fact, it seems that changing
  ;; font-latex-syntactic-keywords is actually unreliable, but
  ;; font-latex-syntactic-keywords-extra seems to work so far.
  (set (make-local-variable 'words-include-escapes) t)
  (modify-syntax-entry ?\\ "'")
  (modify-syntax-entry ?* "_")
  (modify-syntax-entry ?| "$")
  (if (not (boundp 'font-latex-syntactic-keywords-extra))
      (message "Warning: font-latex-syntactic-keywords-extra undefined.")
        ;; I guess this is better than nothing in old versions of auctex
        ;; (push '("\\(\\\\\\)\\({\\)" (1 "(") (2 "."))
        ;;       font-latex-syntactic-keywords)
        ;; (push '("\\\\\\(\\$\\)" (1 ".")) font-latex-syntactic-keywords)
        ;; (push '("\\\\\\(\\%\\)" (1 ".")) font-latex-syntactic-keywords)
        ;; (push '("\\(\\\\)langle" (1 "(")) font-latex-syntactic-keywords)
        ;; (push '("\\rangl\\(e\\)" (1 ")")) font-latex-syntactic-keywords))
    ;; Make the '\' of '\{' the matching delimiter for \}
    (push '("\\(\\\\\\)\\({\\)" (1 "(") (2 "."))
          font-latex-syntactic-keywords-extra)

    (push '("\\\\\\(\\$\\)" (1 ".")) font-latex-syntactic-keywords-extra)
    (push '("\\\\\\(\\%\\)" (1 ".")) font-latex-syntactic-keywords-extra)
    (push '("\\(\\\\\\)langle" (1 "(") ) font-latex-syntactic-keywords-extra)
    (push '("\\\\rangl\\(e\\)" (1 ")")) font-latex-syntactic-keywords-extra)
    ;; We must call font-latex-set-syntactic-keywords to reset
    ;; font-latex-syntactic-keywords using the values we just assigned
    ;; to font-latex-syntactic-keywords-extra
    (font-latex-set-syntactic-keywords)
    ;; (message "font-latex-syntactic-keywords: %s" font-latex-syntactic-keywords)
    )

  ;; Load buffer local variables and check if this is a beamer talk
  ;; (hack-local-variables)
  (let ((pdf-on nil))
    (with-current-buffer (gpb-latex--get-master-buffer)
      (save-excursion
        (goto-char (point-min))
        (setq pdf-on (re-search-forward
                      "^[[:space:]]*\\\\documentclass.*{beamer}$"
                      1000 'no-error))))
    (when pdf-on (TeX-PDF-mode-on)))

  ;;(gpb-latex-add-run-tex-support)
  ;; Handle embedded python code blocks
  ;;(run-at-time 0.5 nil 'gpb-latex-add-python-support)
  ;;(gpb-latex-add-python-support)
  (message "gpb-init-LaTeX-buffer...done"))


;; (defun forward-latex-environment-1 (&optional arg)
;;   (interactive "p")
;;   (setq arg (or arg 1))
;;   (cond
;;    ;; Delegate for moving backward
;;    ((< arg 0)
;;     (backward-LaTeX-environment (- arg)))
;;    ;; Moving forward
;;    ((> arg 0)
;;     (re-search-forward "\\\\\\(begin\\|end\\)[ \t]*{[^}\n]+}" nil t)
;;     ;; Recursively move pase subenvironments
;;     (when (save-excursion (goto-char (match-beginning 0))
;;                           (looking-at "\\\\begin"))
;;       (forward-latex-environment-1)
;;       (forward-latex-environment-1)))))
    ;; (when (save-excursion (goto-char (match-beginning 0))
    ;;                        (looking-at-p "\\\\begin"))
    ;;   (while (save-excursion (goto-char (match-beginning 0))
    ;;                        (looking-at-p "\\\\begin"))
    ;;     (forward-latex-environment))
    ;;   (forward-latex-environment)))))

   ;;  (when (looking-at "[^ \t\n]")
   ;;    (skip-chars-backward "^ \t\n"))
   ;;  ;; Move inside of the environment
   ;;  (when (looking-at "[ \t]*\\\\begin[ \t]*{[^}\n]+}")
   ;;    (goto-char (match-end 0)))
   ;;  (LaTeX-find-matching-end)
   ;;  (when (eolp) (forward-line))
   ;;  (forward-latex-environment (1- arg)))

   ;; ;; Moving backward
   ;; ((< arg 0)
   ;;  ;;(when (bolp)(forward-line -1))
   ;;  ;;(beginning-of-line)
   ;;  (LaTeX-find-matching-begin)
   ;;  ;; (when (looking-back "^\\s +")
   ;;  ;;   (beginning-of-line))
   ;;  (forward-latex-environment (1+ arg)))))

;; (defun forward-LaTeX-section (&optional arg)
;;   (cond
;;    (;; Moving forward
;;     (> arg 0)
;;     ;; If you are inside of text, move the beginning
;;     (when (looking-at "[^ \t\n]")
;;       (skip-chars-backward "[^ \t\n]"))
;;     ;; Move inside of the environment
;;     (when (looking-at "[ \t]*\\\\begin[ \t]*{[^}\n]+}")
;;       (goto-char (match-end 0)))
;;     (LaTeX-find-matching-end)
;;     (when (eolp) (forward-line))
;;     (forward-latex-environment (1- arg)))

;;    ;; Moving backward
;;    ((< arg 0)
;;     (when (bolp)(forward-line -1))
;;     (beginning-of-line)
;;     (LaTeX-find-matching-begin)
;;     (when (looking-back "^\\s +")
;;       (beginning-of-line))
;;     (forward-latex-environment (1+ arg)))))

;;(fset 'forward-latex-environment 'LaTeX-find-matching-end)
;;(fset 'mark-LaTeX-section 'LaTeX-mark-section)

(yas/define-snippets
 'latex-mode
 `(("\\frac" "\\frac{${1:numerator}}{${2:denominator}}" "frac")
   ;; ("\\label" "\\label{${1:`(gpb-get-next-reftex-label)`}}$0" "label1")
   ("label" "\\label{${1:`(gpb-get-next-reftex-label)`}}$0" "label2")
   ;; ("\\eqref" "\\eqref{eq:${1:label}$0" "eqref2")
   ("section" "\\section{${1:title}}" "section")
   ("eqn" ,(concat "\\begin{equation} "
                   "`(progn (reftex-label \"equation\") \"\")`\n"
                   "$0\n"
                   "\\end{equation}") "equation")
   ("eqn*"
    ,(concat "\\begin{equation*}\n"
             "$0\n"
             "\\end{equation*}") "equation*")
   ("align"
    ,(concat "\\begin{align} "
             "`(progn (reftex-label \"equation\") \"\")`\n"
             "$0\n"
             "\\end{align}") "align")
   ("align*"
    ,(concat "\\begin{align*}\n"
             "$0\n"
             "\\end{align*}") "align*")
   ("alignat"
    ,(concat "\\begin{alignat}{${1:cols}}\n"
             "$0\n"
             "\\end{alignat}") "alignat")
   ("alignat*"
    ,(concat "\\begin{alignat*}{${1:cols}}\n"
             "$0\n"
             "\\end{alignat*}") "alignat*")
   ("split"
    ,(concat "\\begin{split}\n"
             "$0\n"
             "\\end{split}") "split")
   ))

;; (yas/define-snippets
;;  'latex-mode
;;  `(;; ("\\m" "\\mu") ; nil '(texmathp))
;;    ;; ("\\s" "\\sigma") ; nil '(texmathp))
;;    ;; ("\\t" "\\tau") ; nil '(texmathp))
;;    ("ofp" "\\$(\\Omega${1:^{${2:sup}}}, \\scr F$1, (\\scr F$1_t), \\bb P$1)\\$ $0")
;;    ("\\langle", "\\langle $0 \\rangle")
;;    ("and" "\\text{, and} ")
;;    ("\\int" "\\int_{$0}^{}  \\, d")
;;    ("\\sum" "\\sum_{${1:sub}}^{${2:sup}} $0")
;;    ("half" "\\tfrac{1}{2}$0")           ; nil '(texmathp))
;;    ("ito" "It\\^o")                     ; nil '(texmathp))
;;    ("\\frac" "\\frac{${1:numerator}}{${2:denominator}}$0") ; nil '(texmathp))
;;    ("\\text" "\\text{${1:text}}$0")       ; nil '(texmathp))
;;    ("\\l" "\\lambda $0")
;;    ("d" "\\frac{d}{d${1:var}} $0")
;;    ;; ("\\label" "\\label{${1:`(gpb-get-next-reftex-label)`}}$0")
;;    ;;("label" "\\label{${1:`(gpb-get-next-reftex-label)`}}$0")
;;    ("\\section"
;;     ,(concat "\\section{${1:title}}"))
;;    ;;               " ${2:\\label{${3:`(gpb-get-next-reftex-label)`}}}"))
;;    ;;       (concat
;;    ;;        "\\section{${1}}"))
;;    ;; ;       "${2: \\label{${3:`(gpb-get-next-reftex-label)`}}}\n$0"))
;;    ("align*"
;;     ,(concat "\\begin{align*}\n"
;;              "$0\n"
;;              "\\end{align*}"))
;;    ("alignat*"
;;     ,(concat "\\begin{alignat*}{${1:cols}}\n"
;;              "$0\n"
;;              "\\end{alignat*}"))
;;    ("alignat"
;;     ,(concat "\\begin{alignat}{${1:cols}}\n"
;;              "$0\n"
;;              "\\end{alignat}"))
;;    ("align"
;;     ,(concat "\\begin{align}\n"
;;              "${1:\\label{${2:`(gpb-get-next-reftex-label)`}}}\n"
;;              ;;"${1:\\lefteqn{$2}\\qquad}\n"
;;              "$0\n"
;;              "\\end{align}"))
;;    ;;("document" "\\documentclass{article}\n")
;;    ("split"
;;     ,(concat "\\begin{split}\n"
;;              "$0\n"
;;              "\\end{split}"))
;;    ("eqn*"
;;     ,(concat "\\begin{equation*}\n"
;;              "$0\n"
;;              "\\end{equation*}"))
;;    ("seq" "(${1:X}_1, $1_2, \\dots)")
;;    ("seqn" "(${1:X}_1, $1_2, \\dots, $1_n)")
;;    ("seq0" "(${1:X}_0, $1_1, \\dots)")
;;    ("seq0n" "(${1:X}_0, $1_1, \\dots, $1_n)")
;;    ("eqn"
;;     ,(concat "\\begin{equation} "
;;              "${1:\\label{${2:`(gpb-get-next-reftex-label)`}}}\n"
;;              "$0\n"
;;              "\\end{equation}"))
;;    ;; ("eqnl"
;;    ;;  ,(concat "\\begin{equation} ${1:\\label{"
;;    ;;           "${2:`(\"name\")`}}}\n"
;;    ;;           "$0\n"
;;    ;;           "\\end{equation}"))
;;    ("eqna"
;;     ,(concat "\\begin{eqnarray*}\n"
;;              "$0\n"
;;              "\\end{eqnarray*}"))
;;    ("eqnal"
;;     ,(concat "\\begin{eqnarray} ${1:\\label{"
;;              "${2:`(gpb-get-next-reftex-label)`}}}\n"
;;              "$0\n"
;;              "\\end{eqnarray}"))
;;    ("proc" "$${1:X} = ($1_t)_{${2:0}\\leq t<${3:\\infty}}$ $0")
;;    ("filt" "$\\filt ${1:F} = (\\filt $1_t)_{${2:0}\\leq t<${3:\\infty}}$ $0")
;;    ("documentclass"
;;     "\\documentclass{${1:article}}

;; \\usepackage{geometry}
;; \\usepackage{amsfonts, amsthm, amssymb, amsmath, mathtools}
;; \\usepackage{mathrsfs, dsfont}
;; \\usepackage{hyperref, paralist, fancyhdr, lastpage}

;; \\let \\bb \\mathbb
;; \\let \\scr \\mathscr
;; \\def \\ind{{\\mathds 1}}
;; \\let \\maxx \\vee
;; \\let \\minn \\wedge

;; \\let \\cond \\mid
;; \\def \\bigcond {\\bigm\\vert}
;; \\def \\Bigcond {\\Bigm\\vert}
;; \\def \\biggcond {\\biggm\\vert}
;; \\def \\Biggcond {\\Biggm\\vert}

;; \\theoremstyle
;; \\newtheorem{example}

;; \\begin{document}

;; $0

;; \\end{document}
;; ")))

(defun gpb-latex-save-local-variables-close-properties ()
  (let ((tex-buffer (current-buffer)))
    (gpb-settings:write-local-variables
     "%" 'TeX-PDF-mode 'TeX-master 'TeX-debug-warnings
     'TeX-debug-bad-boxes)
    (gpb-settings:close-property-sheet)
    (pop-to-buffer tex-buffer))
  (message "Wrote local variables")
)

(defvar gpb-latex-current-version nil
  "Current verion of latex file.

  If there are no versions, this variable is nil.  Another
  possible value is 'all-version.")
(make-variable-buffer-local 'gpb-latex-current-version)

(defun gpb-latex--get-TeX-debug-warnings ()
  (with-current-buffer (gpb-latex--get-master-buffer)
    TeX-debug-warnings))

(defun gpb-latex--toggle-TeX-debug-warnings ()
  (with-current-buffer (gpb-latex--get-master-buffer)
    (TeX-toggle-debug-warnings))
  (setq TeX-debug-warnings (gpb-latex--get-TeX-debug-warnings)))

(defun gpb-latex--get-TeX-debug-bad-boxes ()
  (with-current-buffer (gpb-latex--get-master-buffer)
    TeX-debug-bad-boxes))

(defun gpb-latex--toggle-TeX-debug-bad-boxes ()
  (with-current-buffer (gpb-latex--get-master-buffer)
    (TeX-toggle-debug-bad-boxes))
  (setq TeX-debug-bad-boxes (gpb-latex--get-TeX-debug-bad-boxes)))


(defun gpb-latex-setup-settings-buffer ()
  ;; (logval '(current-buffer))
  ;; (logval 'gpb-settings:property-buffer)
  ;; (logval 'TeX-master)
  ;; (logval 'TeX-PDF-mode)
  ;; (logval 'gpb-latex-remove-margins)
  ;; (logval 'gpb-latex-current-version)

  (gpb-settings:insert-text "Type s to save values and close properties\n\n")
  (gpb-settings:define-key "s" 'gpb-latex-save-local-variables-close-properties)
  (gpb-settings:insert-text "  Boolean variables:\n    ")
  (gpb-settings:insert-checkbox TeX-PDF-mode 'TeX-PDF-mode)
  (gpb-settings:insert-text " Produce PDF output?\n    ")
  (gpb-settings:insert-checkbox (not (gpb-latex--get-TeX-debug-warnings))
                          'gpb-latex--toggle-TeX-debug-warnings)
  (gpb-settings:insert-text " Ignore compilation warnings?\n    ")
  (gpb-settings:insert-checkbox (not (gpb-latex--get-TeX-debug-bad-boxes))
                          'gpb-latex--toggle-TeX-debug-bad-boxes)
  (gpb-settings:insert-text " Ignore bad boxes?\n    ")
  (gpb-settings:insert-checkbox gpb-latex-remove-margins
                          (lambda (val) (setq gpb-latex-remove-margins val)))
  (gpb-settings:insert-text " Remove margins?\n    ")
  (gpb-settings:insert-checkbox gpb-latex-auto-start-viewer
                          (lambda (val) (setq gpb-latex-auto-start-viewer val)))
  (gpb-settings:insert-text " Show viewer after compilation?\n")

  (let ((versions (gpb-latex-read-versions)))
    (when versions
      ;; (unless (and (boundp 'gpb-latex-current-version)
      ;;              (member gpb-latex-current-version versions))
      ;;   (setq gpb-latex-current-version 'all-versions))
      (gpb-settings:insert-text "\n  Select version:\n    ")
      (gpb-settings:insert-choice (gpb-latex-get-current-version)
                            (cons '("all versions" . all-versions)
                                  (mapcar (lambda (name) (cons name name))
                                                 versions))
                            (lambda (val)
                              (message "New version: %S" val)
                              (gpb-latex-set-current-version val))
                                   )))
  (gpb-settings:insert-text "\n  Master file:\n    ")
  (gpb-settings:insert-file-chooser
   TeX-master
   (lambda (val)
      (if val (progn
                (message "Setting master file to %s" val)
                (setq TeX-master val))
        (message "Clearing master file%s" val)
        (setq TeX-master t)))
    t t)
  (gpb-settings:insert-text "\n\n  Save to \"Local Variables:\" ")
  (gpb-settings:insert-button "Save"
                        'gpb-latex-save-local-variables-close-properties)
  )

  ;; (gpb-settings:insert-file-chooser TeX-master "\nTeX-master:\n"
  ;;                             (lambda (val)
  ;;                               (message "Setting master file to %s" val)
  ;;                               (setq TeX-master val)))
  ;; )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Compilation and error parsing
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun gpb-tex-run-compile (name command file)
;;   ;;(setq next-error-function 'compilation-next-error-function)
;;   (let* ((compilation-ask-about-save nil)
;;          (buffer-name "*latex*")
;;          (compilation-buffer (get-buffer-create buffer-name))
;;          ;; The next line tells compilation mode to use this buffer
;;          (compilation-buffer-name-function (lambda (mode) buffer-name))
;;          (dir default-directory)
;;          (output-file (concat file "." (TeX-output-extension)))
;;          (line (TeX-current-line))
;;          (source-file (TeX-current-file-name-master-relative))
;;          (source-buffer (current-buffer))
;;          reset-config
;;          )
;;     (with-current-buffer buffer-name
;;       ;; In its infinite wisdom, compile (really compilation-mode)
;;       ;; kills all local variables, so we have to mark locals as
;;       ;; permanent.
;;       (put (make-local-variable 'compilation-auto-jump-to-first-error)
;;            'permanent-local nil)
;;       (put (make-local-variable 'gpb-compilation-auto-jump-to-first-error)
;;            'permanent-local t)
;;       (setq compilation-auto-jump-to-first-error nil
;;             gpb-compilation-auto-jump-to-first-error nil)
;;       (put (make-variable-buffer-local 'gpb-close-compilation-on-success)
;;            'permanent-local t)
;;       ;; (put (make-variable-buffer-local 'comint-scroll-to-bottom-on-output)
;;       ;;      'permanent-local t)
;;       ;; (put (make-variable-buffer-local 'comint-scroll-show-maximum-output)
;;       ;;      'permanent-local t)
;;       (setq ;; The follow would but nice, but doesn't really work
;;             ;; because compile-auto-jump gets called twice, so it
;;             ;; actually goes to the second error.
;;             ;; compilation-auto-jump-to-first-error t
;;             ;; compilation-scroll-output 'first-error
;;             ;; Compilation usually saves the old directory
;;             gpb-compilation-auto-jump-to-first-error t
;;             default-directory dir
;;             ;; These buffer local comint settings seem to confuse
;;             ;; compilation-scroll-output
;;             ;; comint-scroll-to-bottom-on-output nil
;;             ;; comint-scroll-show-maximum-output nil
;;             ;; If the compilation buffer is not currently visible,
;;             ;; we hide it after success.
;;             gpb-close-compilation-on-success
;;             (unless
;;                 (get-buffer-window buffer-name)
;;               (current-window-configuration)))
;;       (compile command t)
;;       ;; Save information to call viewer on success
;;       (set (make-local-variable 'gpb-latex-output-file) output-file)
;;       (set (make-local-variable 'gpb-latex-line) line)
;;       (set (make-local-variable 'gpb-latex-source-file) source-file)
;;       (set (make-local-variable 'gpb-latex-source-buffer) source-buffer)

;;       ;;(setq gpb-close-compilation-on-success reset-config)
;;       ;; (message "gpb-tex-run-compile2 gpb-close-compilation-on-success: %s"
;;       ;;          gpb-close-compilation-on-success)
;;       )))

(defun recenter-error-message nil
  (when next-error-last-buffer
    (let ((win (get-buffer-window next-error-last-buffer)))
      (when win
        (with-selected-window win
          (recenter))))))

(add-hook 'next-error-hook 'recenter-error-message)


;; (add-hook 'compilation-finish-functions
;;           'gpb-latex-on-compilation-finish)
;; (defun gpb-latex-on-compilation-finish (buffer status)
;;   "Update the okular if compilation succeeds."
;;   (when (and gpb-latex-auto-start-viewer
;;              (string= (buffer-name buffer) "*latex*")
;;              (string= status "finished\n"))
;;     (with-current-buffer gpb-latex-source-buffer
;;       (gpb-latex-view-version-output t)))
;;   ;; The text properties get set asyncronously, so we need to make
;;   ;; sure everything alread fontified before we call next error.
;;   (with-current-buffer buffer
;;     (font-lock-fontify-region (point-min) (point-max)
;;                               ;;(next-single-property-change 1 'message)
;;                               ))
;;   (with-current-buffer gpb-latex-source-buffer
;;     (condition-case exc
;;         (next-error nil t)
;;       ('error
;;        (let ((win (get-buffer-window buffer)))
;;          (when win
;;            (with-selected-window win
;;              (goto-char (point-max))
;;              (recenter -1))))))))
;;     ;; (shell-command (format "refresh-okular %s %s %s"
;;     ;;                        gpb-latex-output-file
;;     ;;                        gpb-latex-line
;;     ;;                        gpb-latex-source-file))))

(defun gpb-latex-compile-document ()
  (interactive)
  (if (buffer-base-buffer)
      (with-current-buffer (buffer-base-buffer)
        (gpb-latex-compile-document))
    (when (or (gpb-util-ends-with buffer-file-name ".sty")
              (gpb-util-ends-with buffer-file-name ".cls"))
      (error "You can't compile this file."))
    (save-buffer)
    (gpb-compile (format "pdflatex -interaction=nonstopmode -halt-on-error %s"
                         (if (eq TeX-master t)
                             (file-name-nondirectory (buffer-file-name))
                           TeX-master))
                 :buffer-name "*pdflatex*"
                 ;; :error-alist '(gpb-tex-error gpb-tex-warning gpb-tex-bad-box)
                 ;; :hide-on-success t
                 :jump-to-first-error t)
    (run-at-time 3 nil 'shell-command
                 (format "start C:\\PROGRA~2\\Adobe\\READER~1.0\\Reader\\AcroRd32.exe %s.pdf"
                         (file-name-sans-extension (buffer-file-name))))))

;; (defun gpb-latex--init-compile ()
;;   (insert "Setting up compilation buffer for latex...\n")
;;   (set (make-local-variable 'compilation-auto-jump-to-first-error) t)
;;   ;; I can't tell what the following does?
;;   ;; (set (make-local-variable 'compilation-auto-jump-to-next) t)
;;   (set (make-local-variable 'compilation-auto-jump-to-next) t)
;;   (next-error-follow-minor-mode 1))

  ;; (TeX-command "run-tex" 'TeX-master-file -1))

(defun gpb-latex-toggle-remove-margins ()
  (interactive)
  (setq gpb-latex-remove-margins (not gpb-latex-remove-margins))
  (if gpb-latex-remove-margins
      (message "Removing margins.")
    (message "Leaving margins intact.")))

(defun gpb-latex-show-log ()
  (interactive)
  (let ((log-filename (gpb-latex-get-version-file "log")))
    (when (file-exists-p log-filename)
      (switch-to-buffer-other-window (find-file-noselect log-filename 'no-warn))
      (revert-buffer t t)
      (toggle-read-only 1))))


;; (push '("run-tex" "run-tex %(run-tex-options)%t"
;;         gpb-tex-run-compile nil (latex-mode doctex-mode)
;;         :help "Use run-tex script")
;;       TeX-command-list)

(defun gpb-latex-make-run-tex-options ()
  (concat
   (if (and (not TeX-Omega-mode)
            (or TeX-PDF-mode TeX-DVI-via-PDFTeX))
       "-o pdf "
     "-o dvi ")
   ;; (if TeX-source-correlate-mode
   ;;     "--source-specials "
   ;;   "")
   (if (gpb-latex--get-TeX-debug-bad-boxes) "" "--no-bad-boxes ")
   (if (gpb-latex--get-TeX-debug-warnings) "" "--no-warnings ")
   (let ((version (gpb-latex-get-current-version)))
     (if (and version (not (eq version 'all-versions)))
         (format "-v %s " version)
       ""))
   (if (and (boundp 'gpb-latex-remove-margins)
            gpb-latex-remove-margins)
       "--no-margins "
     "")))

(add-to-list 'TeX-expand-list '("%(run-tex-options)"
                                gpb-latex-make-run-tex-options))

;; (aput 'TeX-expand-list
;;       'gpb-latex-make-run-tex-options)
      ;; (list (lambda nil
      ;;         )))

;; (defadvice TeX-LaTeX-sentinel (after check-for-pytex-warnings
;;                                      (process name))
;;   "Also check for pytex errors.."
;;   ;(message "I was called from %s %s." (buffer-name) (point))
;;   ;(re-search-forward regex bound no-errors)
;;   (if (re-search-forward "^LaTeX Warning: Python" nil t)
;;       (progn (message (concat "You should run LaTeX again "
;;                               "to get the python definitions right, "
;;                               (TeX-current-pages)))
;;              (setq TeX-command-next TeX-command-default))))
;; (ad-activate 'TeX-LaTeX-sentinel)

;; (defadvice TeX-next-error (around allow-popup-errors (reparse))
;;   "Allow TeX-next-error to popup windows."
;;   (let ((pop-up-windows t))
;;     ad-do-it))
;; (ad-activate 'TeX-next-error)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Misc functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The standard version of this function includes way too much path
(defun TeX-process-buffer-name (name)
  "Return name of AUCTeX buffer associated with the document NAME."
  (concat "*" (file-name-nondirectory name) " output*"))

(defun gpb-latex--get-master-filename ()
  (if (and (boundp 'TeX-master) (not (eq TeX-master t)))
      (setq TeX-master (concat (file-name-sans-extension TeX-master)
                                 ".tex"))
    (buffer-file-name)))

(defun gpb-latex--get-master-buffer ()
  (if (and (boundp 'TeX-master) (not (eq TeX-master t)))
      (progn
        (setq TeX-master (concat (file-name-sans-extension TeX-master)
                                 ".tex"))
        (find-file-noselect TeX-master 'nowarn))
    (current-buffer)))

(defun gpb-latex-electric-dollar-sign ()
  (interactive)
  (TeX-insert-dollar)
  (when (texmathp)
    (save-excursion (insert "$"))))

(defun gpb-latex-is-matched ()
  (save-excursion
    (backward-char)
    (condition-case exception
        (progn
          (forward-sexp)
          t)
      ('error nil))))

(defun gpb-latex-electric-parenthesis ()
  (interactive)
  (insert "(")
  (unless (gpb-latex-is-matched)
    (insert ")")
    (backward-char)))

(defun gpb-environment-delimiter-around-point (&optional pos)
  "Returns true if the point is strictly inside an environment
delimiter and sets the match data to the delimiter.  An
environment delimter is a string of the form \"\\begin{...}\" or
\"\\end{...}\"."
  (setq pos (or pos (point)))
  (save-excursion
    (goto-char pos)
    (forward-line 0)
    (while (and (re-search-forward
                 "\\\\\\(?:begin\\|end\\){[a-zA-Z*]+}"
                 (line-end-position) t)
                (> pos (match-end 0))))
    (if (and (match-beginning 0)
             (< (match-beginning 0) pos)
             (< pos (match-end 0)))
        t
      (set-match-data '(nil nil))
      nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Support for python code in latex
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar gpb-latex-python-envs "\\(?:python.*?\\|pyplot.*\\)"
  "A regular expression matching python code environments")

(defvar gpb-latex-python-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'gpb-latex-python-tab-command)
    (define-key map "\"" 'self-insert-command)
    ;;(define-key map [(control ?\;)] 'gpb-latex-python-comment-dwim)
    map)
  "Keymap for python minor mode.")

(defun gpb-latex-python-tab-command ()
  (interactive)
  (let ((indent-line-function 'python-indent-line)
        gpb-python-base-indentation-level)
    (save-excursion
      (re-search-backward (concat "\\\\begin[[:space:]]*{"
                                  gpb-latex-python-envs "}"))
      (setq gpb-python-base-indentation-level
            (+ (current-indentation) LaTeX-indent-level)))
    (save-restriction
      (gpb-chunks-narrow-to-chunk)
      (gpb-py-tab-command))))


(define-minor-mode gpb-latex-python-minor-mode
  "A minor mode for editing python code chunks in a latex document."
  nil " Py" gpb-latex-python-minor-mode-map
  (if gpb-latex-python-minor-mode
      (setq comment-start "# ")
    (setq comment-start "%")))


(defun gpb-latex-add-python-support ()
  "Mode for editing python in LaTeX conventions."
  (interactive)
  (require 'gpb-python)
  (save-excursion
    (goto-char (point-min))
    ;; The next call is needed to get auctex installed properly
    ;;(font-lock-fontify-buffer)
    (gpb-chunks-minor-mode 1)
    (gpb-chunks-add-chunk 'python
                          (concat "\\\\begin{" gpb-latex-python-envs "}")
                          (concat "\\\\end{" gpb-latex-python-envs "}")
                          'gpb-py-mode
                          'gpb-latex-python-minor-mode)))


(defun gpb-latex-process-local-style-files ()
  "Parse any style files in the same directory as the tex document.

For some reason Auctex does not seem to do this by default.  It
looks for style files at the global and user level, but not the
the directory level."
  (interactive)
  (dolist (f (directory-files "." t ".*\.sty"))
    (TeX-auto-generate f TeX-auto-local)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Code to iterate through delimiters and environments
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar gpb-temp-overlays nil
  "Temporary overlay used to highlight text that has just
  changed.")

(defun gpb-find-next-in-list (item list &optional find-before)
  "Returns position of item in list"
  (when find-before (setq list (reverse list)))
  (let ((found (member item list)))
    (when found (cadr found))))

;; (defun gpb-find-previous-in-list (item list)
;;   "Returns position of item in list"
;;   (let ((found (member item (reverse list))))
;;     (when found (cadr found))))

(defun gpb-remove-temp-overlays-hook ()
  (dolist (ov gpb-temp-overlays)
    ;;(message "overlay: %S" ov)
    (delete-overlay ov))
  (setq gpb-temp-overlays nil)
  (remove-hook 'pre-command-hook 'gpb-remove-temp-overlays-hook))

(defun gpb-make-temp-overlay (beg end &rest properties)
  (setq properties (or properties
                       '(face region)))
  (let ((ov (make-overlay beg end))
        prop value)
    (while properties
      (setq prop (pop properties)
            value (pop properties))
      (overlay-put ov prop value))
    (push ov gpb-temp-overlays)
    (run-at-time 0.2 nil 'gpb-remove-temp-overlays-hook)
    ov))

(defun gpb-insert-with-temp-overlay (string &rest props)
  (let ((point (point)))
    (insert string)
    (apply 'gpb-make-temp-overlay point (point) props)))

(defun gpb-try-to-grow-delimiter-size ()
  (interactive)
  (gpb-try-change-delimiter-size))

(defun gpb-try-to-shrink-delimiter-size ()
  (interactive)
  (gpb-try-change-delimiter-size 'smaller))

(defun gpb-latex-beginning-of-delimiter ()
  (backward-char)
  (if (looking-back "\\\\")
      (backward-char)
    (unless (looking-at "]\\|)\\||")
      (while (not (looking-back "\\\\"))
        (backward-char))
      (backward-char))))

(defun gpb-try-change-delimiter-size (&optional smaller)
  (interactive)
  (when (looking-at "\\s(\\|\\s$")
    (forward-sexp))
  ;; Must be looking at closing delimiter or paired delimiter
  (when (looking-back "\\s)\\|\\s$")
    ;;nil ; (progn (message "Not looking back at delimiter.") nil)
    (let (next-size)
      (save-excursion
        (gpb-latex-beginning-of-delimiter)
        (let ((size-match
               (search-backward-regexp
                (concat "\\(\\(:?\\\\[Bb]ig+r\\)\\|"
                        "\\(:?\\\\right\\)\\)\\=")
                nil t)))
          (if (not size-match)
              (setq next-size (if smaller "\\right"
                                "\\bigr"))
                     ;; (gpb-insert-with-temp-overlay
                     ;;  "\\bigr"
                     ;;  'face `((background-color . ,gpb-blue))))
          (setq next-size (gpb-find-next-in-list
                             (match-string 1)
                             '("\\bigr" "\\Bigr" "\\biggr" "\\Biggr"
                               "\\right" "")
                             smaller))
            (delete-region (match-beginning 1) (match-end 1)))
          (when next-size
            (gpb-make-temp-overlay (point)
                                   (progn (insert next-size)
                                          (forward-char)
                                          (point))))))
            ;; (gpb-insert-with-temp-overlay
            ;;  next-size ;'face `((background-color . ,gpb-blue))
            ;;  ))))

      (save-excursion
        (backward-sexp)
        (setq next-size
              (aget '((nil . "") ("" . "") ("\\bigr" . "\\bigl")
                      ("\\Bigr" . "\\Bigl")
                      ("\\biggr" . "\\biggl") ("\\Biggr" . "\\Biggl")
                      ("\\right" . "\\left")) next-size))

        (let ((end (point-marker))
              begin)
          (if (search-backward-regexp
               "\\(\\(:?\\\\[Bb]ig+l\\)\\|\\(:?\\\\left\\)\\)\\\\?\\="
               nil t)
              (delete-region (match-beginning 1) (match-end 1))
            (when (looking-back "\\\\") (backward-char)))
          (gpb-make-temp-overlay (point) (progn (insert next-size) (point)))
          ;; (gpb-insert-with-temp-overlay
          ;;  next-size            ;'face
          ;; `((background-color . ,gpb-blue))
          )))
            ;; (setq next-size (gpb-find-next-in-list
            ;;                  (match-string 1)
            ;;                  '("\\bigl" "\\Bigl" "\\biggl" "\\Biggl"
            ;;                    "\\left" "")))
      t))

(defun gpb-try-change-delimiter-type-reverse ()
  (interactive)
  (gpb-try-change-delimiter-type 'reverse))

(defun gpb-try-change-delimiter-type (&optional reverse)
  (interactive)
  (if (not (looking-back "\\s)\\|\\s$"))
      nil     ; (progn (message "Not looking back at delimiter.") nil)
    (let (opener closer closer-match)
      (save-excursion
        (backward-char)
        (when (looking-back "\\\\") (backward-char))
        (setq closer (search-forward-regexp "\\\\?[])}|]" nil t)
              closer (gpb-find-next-in-list
                              (match-string 0)
                              '(")" "]" "\\}" "|" "\\|" "}" ")") reverse)
              opener (aget '((")" . "(")     ("]" . "[")
                             ("\\}" . "\\{") ("}" . "{")
                             ("|" . "|")     ("\\|" . "\\|")
                             ) closer)
              closer-match (match-data)))
      (backward-sexp)
      (when (looking-back "\\\\") (backward-char))
      (when (search-forward-regexp "\\\\?[[({|]" nil t)
        (delete-region (match-beginning 0) (match-end 0)))
      (gpb-insert-with-temp-overlay opener)
      (set-match-data closer-match)
      (goto-char (match-end 0))
      (delete-region (match-beginning 0) (match-end 0))
      (gpb-insert-with-temp-overlay closer)
      )))

(defun gpb-try-to-change-environment (&optional reverse-direction)
  (interactive)
  (save-excursion
    (when (and (looking-at "[[:word:]*]*}")
               (looking-back "\\(\\\\begin\\|\\\\end\\){[[:word:]*]*"))
      (goto-char (match-beginning 0))
      (forward-char)
      (LaTeX-find-matching-begin)
      (search-forward-regexp "\\=\\\\begin{\\([[:word:]*]*\\)}")
      (let ((next-env (gpb-latex--next-environment
                       (match-string 1) reverse-direction)))
        (if (not next-env)
            (message "Can't change environment.")
          (goto-char (match-beginning 1))
          (replace-match "" nil nil nil 1)
          (gpb-insert-with-temp-overlay
           next-env) ; 'face `((background-color . ,gpb-blue)))

          (forward-char)
          (LaTeX-find-matching-end)
          (search-backward-regexp "\\\\end{\\([[:word:]*]*\\)}\\=")
          (replace-match "" nil nil nil 1)
          (goto-char (match-beginning 1))
          (gpb-insert-with-temp-overlay
           next-env) ; 'face `((background-color . ,gpb-blue)))
          ))
      t)))

(defun gpb-try-to-change-environment-backwards ()
  (interactive)
  (gpb-try-to-change-environment -1))

(defun gpb-latex--next-environment (cur-env &optional reverse-direction)
  (let ((offset (if reverse-direction -1 1)))
    (or (gpb-util-get-next-item cur-env
                                '("enumerate" "itemize" "description")
                                offset)
        (gpb-util-get-next-item cur-env
                                '("equation" "equation*"
                                  "align" "align*"
                                  "split" "split*"
                                  "gather" "gather*"
                                  "alignat" "alignat*")
                                offset)
        (gpb-util-get-next-item cur-env
                                '("lemma" "theorem" "proposition"
                                  "corollary" "definition"
                                  "notation" "setting")
                                offset))))
;; (defun gpb-latex-meta-up ()
;;   (interactive)
;;   (unless (gpb-try-change-delimiter-size)
;;     (backward-up-list)))

;; (defun gpb-latex-meta-down ()
;;   (interactive)
;;   (unless (gpb-try-change-delimiter-size 'smaller)
;;     (backward-up-list)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; We overload tab to everything under the sun here...
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defvar gpb-latex-tab-command-chain
;;   '(gpb-try-to-yas/next-field
;;     gpb-latex-try-to-add-label
;;     gpb-latex-try-to-get-ref
;;     yas/expand
;;     gpb-latex-try-to-expand-environment
;;     gpb-latex-try-completion
;;     ;; gpb-latex-try-insert-braces
;;     gpb-latex-try-to-insert-document-template
;;     ;;gpb-latex-try-skip-closing-delimiter
;;     gpb-latex-indent-according-to-mode)
;;   "This is a list of functions that are tried in turn when tab is
;;   pressed.")

(defun gpb-latex-tab-command ()
  (interactive)
  (let ((yas/fallback-behavior 'return-nil))
    (or (gpb-try-to-yas/next-field)
        (gpb-latex-try-to-add-label)
        ;; (gpb-latex-try-to-get-ref)
        (gpb-latex-try-completion)
        (yas/expand)
        (gpb-latex-try-to-expand-environment)
        ;; gpb-latex-try-insert-braces)
        (gpb-latex-try-to-insert-document-template)
        ;;gpb-latex-try-skip-closing-delimiter)
        (gpb-latex-indent-according-to-mode))))

    ;; (gpb-execute-command-chain gpb-latex-tab-command-chain)))

(defun gpb-latex-try-completion ()
  "Try to do completion but fail if the symbol is already complete."
  (interactive)
  (cond
   ((looking-back "eqref")
    (delete-region (match-beginning 0) (match-end 0))
    (delete-horizontal-space t)
    (yas/expand-snippet (point) (point) "~\\eqref{eq:${1:label}} $0"))
   ((looking-back "_")
    (yas/expand-snippet (match-beginning 0) (match-end 0) "_{${1:sub}}$0")
    t)
   ((looking-back "\\^")
    (yas/expand-snippet (match-beginning 0) (match-end 0) "^{${1:sup}}$0")
    t)
   ((looking-back "\\^\\([mnijk]\\)")
    (let ((letter (buffer-substring (match-beginning 1) (match-end 1))))
      (delete-region (match-beginning 0) (match-end 0))
      (insert "^{(" letter ")}")
      t))
   ((looking-at "\\_>")
      (let* ((prefix (gpb-latex-get-completion-prefix))
             (completions (gpb-latex-get-completion-candidates prefix))
             ;;(match (try-completion prefix completions))
             )
        ;; Where there are completions and the current prefix is not
        ;; already complete.
        (when (not (member prefix completions))
          ;;(and match (not (eq match t))) ;; (not (string= match prefix)))
          (condition-case exception
              (progn
                (company-begin-backend 'gpb-latex-dabbrev-backend)
                (company-complete-common)
                t)
            ('error nil)))))))

(defun gpb-latex-do-completion ()
  (interactive)
  (company-begin-backend 'gpb-latex-dabbrev-backend)
  (company-complete-common))


;; (defun gpb-latex-get-eqref ()
;;   (reftex-reference "e" 'no-insert))


(defun gpb-latex-try-insert-braces ()
  (interactive)
  (let ((prebrace-commands '("\\scr" "\\bb" "\\cal" "_" "^"
                             "\\mathscr" "\\mathbb" "\\mathcal"
                             "\\textbf")))
    (when (looking-back (gpb-util-join-strings
                         (mapcar 'regexp-quote prebrace-commands)
                         "\\|"))
      (insert "{}")
      (backward-char)
      t)))

;; (defun gpb-latex-try-skip-closing-delimiter ()
;;   (interactive)
;;   (cond
;;    ((looking-at "\\}\\|)\\|\\]\\|\\\\\\}")
;;     (goto-char (match-end 0))
;;     t)
;;    ((looking-at "\\$")
;;     (forward-char)
;;     (gpb-latex-entering-math-mode -1)
;;     t)))


;; (defun gpb-latex-try-to-get-eqref ()
;;   "We define some macros that can be run by tab expansion."
;;   (interactive)
;;   (when (looking-back "\\b\\\\?eqref")
;;     (replace-match (regexp-quote "\\eqref{"))
;;     (let ((ov-beg (point))
;;           ov-end temp-ov reference)
;;       (unwind-protect
;;           (progn
;;             (save-excursion
;;               (insert "choose equation}")
;;               (backward-char)
;;               (setq ov-end (point)
;;                     temp-ov (make-overlay ov-beg ov-end))
;;               (overlay-put temp-ov 'face
;;                            `((background-color . ,gpb-blue)))
;;               (overlay-put temp-ov 'evaporate t))
;;             (setq reference (gpb-latex-get-eqref))
;;             (delete-region ov-beg ov-end)
;;             (insert reference)
;;             (forward-char)
;;             ;(insert " ")
;;             )
;;         (delete-overlay temp-ov)))
;;     t))

;; (defun gpb-latex-try-to-get-eqref ()
;;   "We define some macros that can be run by tab expansion."
;;   (interactive)
;;   (reftex-access-scan-info 'rescan)
;;   (when (or (looking-back "\\\\eqref{?")
;;             (looking-back "\\beqref{?"))
;;     (replace-match (regexp-quote "\\eqref{"))
;;     (let ((eqrefs (gpb-reftex-get-eqrefs)))
;;       (if eqrefs
;;           (gpb-show-completion-list eqrefs 'gpb-latex-close-braces)
;;         (message "No labeled equations in document."))
;;       t)))

(defun gpb-latex-try-to-add-label ()
  (interactive)
  (when (looking-back "\\\\label")
    (delete-region (match-beginning 0) (match-end 0))
    (reftex-label)
    (insert " % ")))

(defun gpb-latex-try-to-get-ref ()
  "If we are looking at a latex crosss reference when tab is hit,
we use reftex to generate a list of possibilities."
  (interactive)
  ;;(reftex-access-scan-info)
  ;; Remove rescan if this gets too slow.
  (let ((gpb-enable-record-file-on-visit nil)
        (reftex-access-scan-info 'rescan)
        (use-autoref nil)
        (gpb-modal-mode nil))
    (let (cmd ;; The command to eval
          keys) ;; initial keystrokes to send to command
      (cond
       ((looking-back "\\(\\\\\\| \\|\\b\\)cite{?" nil 'greedy)
        (setq cmd '(let ((beg (match-beginning 0))
                         (end (match-end 0))
                         (key (reftex-citation 'no-insert)))
                     (delete-region beg end)
                     (insert (concat "\\cite{" key "}")))
              initial-key-strokes nil))
       ((looking-back "\\( *~?\\)\\\\eqref{?" nil 'greedy)
        (setq cmd '(reftex-reference "e" t)
              initial-key-strokes '(?c ?f)))
       ((looking-back "\\( *~?\\)\\\\ref{?" nil 'greedy)
        (setq cmd '(reftex-reference nil t)
              initial-key-strokes '(? ?c ?f)
              ))
       ((looking-back "\\\\autoref{?" nil 'greedy)
        (setq cmd '(reftex-reference t)
              initial-key-strokes '(? ?c ?f)
              use-autoref t
              )))
      (when cmd
        (setq unread-command-events
              (append unread-command-events initial-key-strokes))
        (let (label)
          (save-window-excursion
            (save-match-data
              (flet ((delete-other-windows ()))
                (setq label (eval cmd)))))
          (when label
            (save-excursion
              (when (not (string-equal (match-string 1) ""))
                (replace-match "~" nil nil nil 1)))
            (insert "{")
            (insert label)
            (insert "}")))
        ;; Make sure we return t to stop the tab command chain
        t))))

(defun gpb-latex-close-braces (&optional arg)
  (insert "}"))
  "We define some macros that can be run by tab expansion."

(defvar gpb-latex-template-dir
  (expand-file-name "~/usr-common/tex/templates"))

(defun gpb-latex-try-to-insert-document-template ()
  (interactive)
  (when (looking-back "\\bnewdoc")
    (let* ((templates (mapcar
                       'file-name-nondirectory
                       (mapcar
                        'file-name-sans-extension
                        (file-expand-wildcards (concat gpb-latex-template-dir
                                                       "/*.tex"))))))

      (gpb-show-completion-list templates
                                'gpb-latex-insert-template))))

(defvar gpb-latex-inline-environments
  '("matrix" "bmatrix" "array")
  "Environments that should be inserted on a single line.")

(defun gpb-latex-try-to-expand-environment ()
  (interactive)
  (let* ((start (save-excursion (skip-chars-backward "-*a-zA-Z")
                                (point)))
         (env-name-before-point (buffer-substring start (point))))
    (when (and env-name-before-point
               (member env-name-before-point
                       (mapcar 'car (LaTeX-environment-list))))
      (delete-region start (point))
      (let ((col (current-column)))
        (if (member env-name-before-point gpb-latex-inline-environments)
            (progn
              (insert (format "\\begin{%s} " env-name-before-point))
              (save-excursion
                (insert (format " \\end{%s} " env-name-before-point))))
          ;; (insert (format "\\begin{%s}\n%s"
          ;;                 env-name-before-point
          ;;                 (make-string (+ col 2) ?\ )))
          ;; (save-excursion
          ;;   (insert (format "\n%s\\end{%s}\n"
          ;;                   (make-string col ?\ )
          ;;                   env-name-before-point))))
          (LaTeX-environment-menu env-name-before-point)))
      t)))

(defun gpb-latex-insert-template (filename-sans-extension)
  (let ((filename (concat gpb-latex-template-dir
                          "/"
                          filename-sans-extension
                          ".tex"))
        (file-contents)
        (match-beg (match-beginning 0)))
    ;; Remove the completion text
    (when gpb-company-list--initial-point
      (delete-region gpb-company-list--initial-point (point)))
    ;; Remove "newdoc"
    (replace-match "")
    (assert (file-exists-p filename))
    (with-temp-buffer
      (insert-file-contents filename)
      (setq file-contents (buffer-substring-no-properties (point-min)
                                                          (point-max))))
    (yas/expand-snippet match-beg match-beg file-contents)))


;; (defun gpb-tex-try-to-complete ()
;;   "Calls TeX-complete-symbol if sitting at the end of a word;
;; otherwise calls indent-according-to-mode."
;;   (interactive "*")
;;   (if (and (not (bobp))  ; Not beginning of buffer, and
;; 	   (= (char-syntax (char-before)) ?w) ; point follows word
;;            ;; point preceeds space or comment-ender
;;            (or (= (char-syntax (char-after))  ? )
;;                (= (char-syntax (char-after))  ?>)))
;;       (TeX-complete-symbol)
;;     (indent-according-to-mode))
;;   )


(defun gpb-latex-make-inserter (text &optional after-text)
  `(lambda ()
      (interactive)
      (insert ,text)
      (when ,after-text
        (save-excursion (insert ,after-text)))))

(defun gpb-latex-insert-sub-or-add-brace ()
  (interactive)
  (if (not (looking-back "_"))
      (insert "_")
    (insert "{}")
    (backward-char)))

(defun gpb-latex-insert-sup-or-add-brace ()
  (interactive)
  (if (not (looking-back "\\^"))
      (insert "^")
    (insert "{}")
    (backward-char)))

;; (defun gpb-latex-toggle-entering-math-mode ()
;;   (interactive)
;;   (if gpb-latex-entering-math-mode
;;       (gpb-latex-entering-math-mode -1)
;;     (unless (texmathp)
;;       (insert "$$")
;;       (backward-char))
;;     (gpb-latex-entering-math-mode 1)))

;; (let ((map (make-sparse-keymap)))
;;   (define-key map "(" (gpb-latex-make-inserter "(" ")"))
  ;; ;; Symbol short cuts.  All start with \C-j
  ;; (define-key map [(control ?j)(?u)] 'gpb-latex-insert-sup-or-add-brace)
  ;; (define-key map [(control ?j)(?j)] 'gpb-latex-insert-sub-or-add-brace)
  ;; (define-key map [(control ?j)(control ?j)] 'gpb-latex-insert-sub-or-add-brace)
  ;; (define-key map [(control ?j)(?p)] (gpb-latex-make-inserter "+"))
  ;; (define-key map [(control ?j)(?m)] (gpb-latex-make-inserter "-"))
  ;; (define-key map [(control ?j)(?n)] (gpb-latex-make-inserter "-"))
  ;; (define-key map [(control ?j)(?e)]  (gpb-latex-make-inserter "="))
  ;; (define-key map [(control ?j)(?a)]  (gpb-latex-make-inserter "&"))
  ;; (define-key map [(control ?j)(?<)]  (gpb-latex-make-inserter "\\geq"))
  ;; (define-key map [(control ?j)(?>)]  (gpb-latex-make-inserter "\\leq"))
  ;; (define-key map [(control ?j)(?\ )]  (gpb-latex-make-inserter "\\,"))
  ;; (define-key map [(control ?j)(?b)]  (gpb-latex-make-inserter "[" "]"))
  ;; (define-key map [(control ?j)(?c)]  (gpb-latex-make-inserter "\\{" "\\}"))
  ;; (define-key map [(control ?j)(return)]  'LaTeX-insert-item)

  ;; ;; Greek short cuts.  All start with \C-k
  ;; (define-key map [(control ?k)(?a)] (gpb-latex-make-inserter "\\alpha"))
  ;; (define-key map [(control ?k)(?b)] (gpb-latex-make-inserter "\\beta"))
  ;; (define-key map [(control ?k)(?e)] (gpb-latex-make-inserter "\\epsilon"))
  ;; (define-key map [(control ?k)(?p)] (gpb-latex-make-inserter "\\pi"))
  ;; (define-key map [(control ?k)(?s)] (gpb-latex-make-inserter "\\sigma"))
  ;; (define-key map [(control ?k)(?t)] (gpb-latex-make-inserter "\\tau"))
  ;; (define-key map [(control ?k)(?m)] (gpb-latex-make-inserter "\\mu"))
  ;; (fset 'gpb-latex-entering-math-map map))

;; (let ((map (make-sparse-keymap)))
;;   (define-key map "(" (gpb-latex-make-inserter "(" ")"))
;;   ;; Symbol short cuts.  All start with \C-j
;;   (define-key map [(control ?u)] 'gpb-latex-insert-sup-or-add-brace)
;;   (define-key map [(control ?j)] 'gpb-latex-insert-sub-or-add-brace)
;;   (define-key map [(control ?p)] (gpb-latex-make-inserter "+"))
;;   (define-key map [(control ?m)] (gpb-latex-make-inserter "-"))
;;   (define-key map [(control ?n)] (gpb-latex-make-inserter "-"))
;;   (define-key map [(control ?e)]  (gpb-latex-make-inserter "="))
;;   (define-key map [(control ?a)]  (gpb-latex-make-inserter "&"))
;;   ;; (define-key map [(control ?j)(?<)]  (gpb-latex-make-inserter "\\geq"))
;;   ;; (define-key map [(control ?j)(?>)]  (gpb-latex-make-inserter "\\leq"))
;;   ;; (define-key map [(control ?j)(?\ )]  (gpb-latex-make-inserter "\\,"))
;;   (define-key map [(control ?b)]  (gpb-latex-make-inserter "[" "]"))
;;   (define-key map [(control ?c)]  (gpb-latex-make-inserter "\\{" "\\}")) ;
;; ;;  (define-key map [(control ?j)(return)]  'LaTeX-insert-item)
;;   (fset 'gpb-latex-entering-math-map map))

;; (define-minor-mode gpb-latex-entering-math-mode
;;   "A minor mode to speed entry of mathematics"
;;   nil
;;   " Math"
;;   'gpb-latex-entering-math-map)


(defun gpb-latex-insert-article-reference ()
  (interactive)
  (let ((yas/indent-line nil))
    (yas/expand-snippet (point) (point)
                        (concat
                         "%\n"
                         "\\bibitem{${1:tag}}\n"
                         "{\\sc ${2:authors}} (${3:year})\n"
                         "\\marginpar{$1}\n"
                         "${4:title}\n"
                         "{\\em ${5:journal}}\n"
                         "{\\bf ${6:volumes}}, ${7:pages}.\n"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Company mode interface
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gpb-latex-dabbrev-backend (command &optional arg &rest ignored)
  "A dabbrev-like `company-mode' back-end for latex."
  (interactive (list 'interactive))
  (case command
    ('interactive (company-begin-backend 'company-dabbrev-code))
    ('prefix (gpb-latex-get-completion-prefix))
    ('candidates (gpb-latex-get-completion-candidates arg))
    ('duplicates t)
    ('ignore-case nil)
    ))

(defun gpb-latex-get-completion-prefix ()
  (if (looking-at "\\_>")
      (buffer-substring-no-properties
       (save-excursion (skip-syntax-backward "w_")
                       (when (and (not (bobp))
                                  (= (char-before) ?\\))
                         (backward-char))
                       (point))
       (point))))

(defun gpb-latex-get-completion-candidates (prefix)
  (let ((completion-ignore-case nil))
    (remove-duplicates
     (company-dabbrev--search
      (concat (when (equal prefix "")
                "\\\\?\\_<\\([a-zA-Z]\\|\\s_\\)")
              (when (not (= (elt prefix 0) ?\\))
                "\\_<")
              (regexp-quote prefix)
              "\\(\\sw\\|\\s_\\|\\\\.\\)*\\_>")
      nil ;;company-dabbrev-code-time-limit
      nil t) ;; Don't look in other buffer & ignore comments
     :test 'string=)
    ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Tools to work with run-tex options
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gpb-latex-add-run-tex-support ()
  "Call to enable run-tex version support."
  ;; (set (make-variable-buffer-local 'gpb-all-versions) nil)
  ;; (gpb-latex-read-versions)

  ;; gpb-latex-current-version can be nil, a string, or 'all-versions
  (make-variable-buffer-local 'gpb-latex-current-version)
  (if (gpb-latex-read-versions)
      (setq gpb-latex-current-version 'all-versions)
    (setq gpb-latex-current-version nil))
       ;; (car gpb-all-versions))
  (set (make-variable-buffer-local 'gpb-latex-remove-margins) nil)
  ;; (local-set-key [(control c)(control t)(control v)]
  ;;                'gpb-latex-next-version)
  (local-set-key [(control c)(control t)(control m)]
                 'gpb-latex-toggle-remove-margins)
  ;; (local-set-key ""
  ;;                'gpb-latex-toggle-remove-margins)
  (local-set-key [(control c)(control v)]
                 'gpb-latex-view-version-output)
  (local-set-key [(control c)(v)]
                 'gpb-latex-view-version-output)
  )

(defun gpb-latex-read-versions ()
  "Returns a list of all versions of the document."
  (let ((all-versions nil))
    (with-current-buffer (gpb-latex--get-master-buffer)
      (save-excursion
        (goto-char (point-min))
        (when (search-forward-regexp "\s*%\s*<versions>\s*\n" nil t)
          (goto-char (match-end 0))
          (let ((end-of-versions
                 (save-excursion
                   (search-forward-regexp "\s*%\s*</versions>\s*\n")
                   (match-end 0))))
            (while (search-forward-regexp "\s*%\s*\\([a-zA-Z0-9]+\\)\s*\n"
                                          end-of-versions t)
              (push (buffer-substring-no-properties (match-beginning 1)
                                                    (match-end 1))
                    all-versions)))
          )))
    (nreverse all-versions)))

(defun gpb-latex-get-current-version ()
  "Get the current active version.

  This is smart enough to look at the master file buffer to get
  the value of the variable if necessary."
  (with-current-buffer (gpb-latex--get-master-buffer)
    (or gpb-latex-current-version
        (if (gpb-latex-read-versions) 'all-versions))))
    ;; (unless (boundp 'gpb-latex-current-version)
    ;;   (let ((versions (gpb-latex-read-versions)))
    ;;     (setq gpb-latex-current-version
    ;;           (if versions 'all-version nil))))
    ;; gpb-latex-current-version))

(defun gpb-latex-set-current-version (version-name)
  "Get the current active version.

  This is smart enough to look at the master file buffer to get
  the value of the variable if necessary."
  (with-current-buffer (gpb-latex--get-master-buffer)
    (setq gpb-latex-current-version version-name)))

;; (defun gpb-latex-next-version ()
;;   (interactive)
;;   (let ((versions (gpb-latex-read-versions)))
;;     (if versions
;;         (cond
;;          ((null gpb-latex-current-version)
;;           (setq gpb-latex-current-version 'all-versions)
;;           (message "Compiling all versions."))
;;          ((and (stringp gpb-latex-current-version)
;;                (string= gpb-latex-current-version
;;                         (car (last versions))))
;;           (setq gpb-latex-current-version 'all-versions)
;;           (message "Compiling all versions."))
;;          ((eq gpb-latex-current-version 'all-versions)
;;           (setq gpb-latex-current-version (car versions))
;;           (message "Compiling version: %s" gpb-latex-current-version))
;;          (t
;;           (setq gpb-latex-current-version
;;                 (gpb-util-get-next-item gpb-latex-current-version versions))
;;           (message "Compiling version: %s" gpb-latex-current-version)))
;;       (setq gpb-latex-current-version nil)
;;       (message "No versions found."))))

(defun gpb-latex-get-version-file (&optional extension)
  (setq extension (or extension (TeX-output-extension)))
  (when (equal (elt extension 0) ?.)
    (setq extension (substring extension 1)))
  (let ((master (TeX-active-master))
        (version (gpb-latex-get-current-version)))
    (cond
     ((eq version 'all-versions)
      (error "You must select a version."))
     (version
      (format "%s-%s/%s.%s" master version master extension))
     (t
      (format "%s/%s.%s" master master extension)))))

;; (defun gpb-latex-get-version-file (extension)
;;   (when (equal (elt extension 0) ?.)
;;     (setq extension (substring extension 1)))
;;   (let ((master (TeX-active-master)))
;;     (if gpb-latex-current-version
;;         (format "%s-%s/%s.%s" master gpb-latex-current-version master extension)
;;       (format "%s/%s.%s" master master extension))))

(defun gpb-latex-get-version-output-file (&optional extension arg)
  (setq extension (or extension (TeX-output-extension)))
  (when (equal (elt extension 0) ?.)
    (setq extension (substring extension 1)))
  (let ((master (TeX-active-master))
        (version (gpb-latex-get-current-version)))
    (if version
        (if (eq version 'all-versions)
            (format "%s-%s.%s" master
                    (gpb-popup-menu-of-choices
                     (mapcar (lambda (x) (cons x x))
                             (gpb-latex-read-versions))
                     "Select version:")
                    extension)
          (format "%s-%s.%s" master version extension))
      (format "%s.%s" master extension))))

(defun gpb-temp (x)
  (cons x x))
;; (defun gpb-latex-view-version-output ()
;;   (interactive)
;;   (let* ((output-file ))
;;     (cond
;;      ((eq gpb-latex-current-version 'all-versions)
;;       (error "You must select a version."))
;;      (gpb-latex-current-version
;;       (format "%s-%s/%s.%s" master gpb-latex-current-version master extension))
;;      (t
;;       (format "%s/%s.%s" master master extension)))))

(defun gpb-latex-view-version-output (&optional ignored-option)
  (interactive)
  (let* ((output-file (gpb-latex-get-version-output-file
                       (TeX-output-extension)))
         (point (point)))
    ;; Okular doesn't like lines after end document
    (save-excursion
      (if (search-backward "\\end{document}" nil t)
          (setq point (min point (point)))))
    (save-excursion
      (if (search-forward "\\begin{document}" nil t)
          (setq point (max point (point)))))
    (if (file-exists-p output-file)
        (save-excursion
          (goto-char point)
          (TeX-command "View" (lambda (&optional x y) output-file) -1))
      ;; (if no-prompt -1 1))
                                        ;'gpb-latex-get-version-output-file)
      (message "Output file %S does not exist." output-file))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Filling and formatting in equations
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gpb-latex-indent-according-to-mode ()
  (interactive)
  (indent-according-to-mode)
  ;;(message "Indenting...")
  ;; (font-lock-mode -1)
  ;; (font-lock-mode 1)
)

(defun gpb-latex-do-auto-fill ()
  "Don't auto-fill in math environments"
  (unless (member (LaTeX-current-environment)
                  '("equation*" "equation**" "align*"
                    "align" "matrix" "split"))
    (do-auto-fill)))

(defun gpb-latex-fill-paragraph-function (&optional justify region)
  (cond
   ((member (LaTeX-current-environment)
            '("equation*" "equation**" "align*"
              "align" "matrix" "split"))
    ;; Do nothing
    t)
   ((member (LaTeX-current-environment)
            '("bmatrix" "tabular"))
    (align
     (save-excursion (LaTeX-find-matching-begin) (point))
     (save-excursion (LaTeX-find-matching-end) (point)))
    ;; Don't do anything else
    t)
   (t
    (save-excursion
      (skip-chars-backward " \t\n")
      (LaTeX-fill-paragraph)))))

(defun gpb-latex-base-indentation ()
  (save-excursion (LaTeX-find-matching-begin)
                  (current-column)))

(defun gpb-latex-insert-item ()
  "Override the auctex insert item stuff in some cases."
  (interactive)
  (cond
   ((member (LaTeX-current-environment)
            '("equation" "equation*" "align" "align*" "aligned"
              "gather" "gather*"))
    (let ((col (gpb-latex-base-indentation)))
      (if (looking-at-p "[[:space:]]*$")
          (progn
            (insert "\n")
            (insert (make-string (+ col LaTeX-indent-level) ?\ ))
            (insert "\\\\\n")
            (insert (make-string (+ col LaTeX-indent-level) ?\ )))
        (beginning-of-line)
        (insert (make-string (+ col LaTeX-indent-level) ?\ ))
        (insert "\\\\\n")
        (delete-horizontal-space)
        (insert (make-string (+ col LaTeX-indent-level) ?\ )))
      ))
   ((member (LaTeX-current-environment)
            '("bmatrix"))
    (let ((col (save-excursion
                 (re-search-backward
                  (rx "matrix}" (zero-or-more " ")))
                 (goto-char (match-end 0))
                 (current-column))))
      (just-one-space)
      (insert "\\\\\n")
      (insert (make-string col ?\ ))))
   (t
    (when (looking-back "^[ \t]*")
      (beginning-of-line)
      (ignore-errors (backward-char)))
    (LaTeX-insert-item))))

(defun gpb-latex-indent-line ()
  "Override the auctex indentation in some cases."
  (interactive)
  (setq this-command 'indent-according-to-mode)
  (let ((LaTeX-syntactic-comments nil))
    (cond
     ((member (save-excursion (beginning-of-line)
                              (LaTeX-current-environment))
              '("equation" "equation*" "align" "align*" "aligned"
                "gather" "gather*" "bmatrix" "split"))
      (if (save-excursion
            (beginning-of-line)
            (looking-at "^\\s-*\\\\end{"))
          ;; If we are closing an environment, match the begin
          (let ((col (save-excursion (beginning-of-line)
                                     (LaTeX-find-matching-begin)
                                     (current-column))))
            (save-excursion
              (beginning-of-line)
              (delete-horizontal-space)
              (insert (make-string col ?\ )))
            (when (looking-back "^\\s-*")
              (skip-chars-forward " \t")))
        (let* ( ;; Start of current environment
               (start (save-excursion
                        ;; Move out of matrix
                        (when (member (LaTeX-current-environment)
                                      '("bmatrix"))
                          (LaTeX-find-matching-begin))
                        ;; Move to the start of the environment.
                        (LaTeX-find-matching-begin)
                        ;; Move to the end of the begin statement
                        (search-forward "}")
                        (point)))
               (base (save-excursion
                       (forward-line -1)
                       (end-of-line)
                       (+ (gpb-latex-base-indentation) LaTeX-indent-level)))
               (prev-start (save-excursion
                             (previous-line)
                             (beginning-of-line)
                             (re-search-forward "\\s-*")
                             (when (> (point) start)
                               (current-indentation))))
               (prev-eq (save-excursion
                          (forward-line -1)
                          (end-of-line)
                          (ignore-errors
                            (re-search-backward
                             (rx (or "=" "<" ">" "\\\\leq" "\\\\geq")
                                 (zero-or-more " ")))
                            (goto-char (match-end 0))
                            (when (> (point) start)
                              (current-column)))))
               (prev-ampersand (when (save-excursion
                                       (forward-line 0)
                                       (looking-at (rx bol (0+ (char space))
                                                       "&")))
                                 (ignore-errors
                                   (save-excursion
                                     (forward-line 0)
                                     (search-backward "&")
                                     (forward-line 0)
                                     (search-forward "&")
                                     (backward-char)
                                     (when (> (point) start)
                                       (current-column))))))
               (prev-matrix (when (member (LaTeX-current-environment)
                                          '("bmatrix"))
                              (save-excursion
                                (forward-line -1)
                                (end-of-line)
                                (ignore-errors
                                  (re-search-backward
                                   (rx (group "matrix}"
                                              (one-or-more (syntax whitespace)))
                                       not-newline))
                                  (goto-char (match-end 1))
                                  (when (> (point) start)
                                    (current-column))))))
               (prev-groupings (let ((x nil))
                                 (save-excursion
                                   (beginning-of-line)
                                   (ignore-errors
                                     (while t
                                       (backward-up-list)
                                       (when (< (point) start)
                                         (error "break"))
                                       (save-excursion
                                         (down-list)
                                         (dolist (s '("{" "langle"))
                                           (when (looking-at (regexp-quote s))
                                             (search-forward s)))
                                         (skip-chars-forward " \t")
                                         (add-to-list 'x (current-column))))))
                                 x))
               (all-tab-stops
                (reverse
                 (sort (remove-duplicates
                        (remq nil (list* base prev-start prev-eq prev-ampersand
                                         prev-matrix prev-groupings))) '<)))
               (next-tab-stop (if (member last-command
                                      '(gpb-latex-indent-according-to-mode
                                        indent-according-to-mode))
                                  (progn
                                    (message "Cycling indentation...")
                                    (or (gpb-util-get-next-item
                                         (current-indentation) all-tab-stops)
                                        (car all-tab-stops)))
                                (car all-tab-stops))))
          (save-excursion
            (back-to-indentation)
            (delete-region (line-beginning-position) (point))
            ;; I would prefer to use indent-to but this seems to
            ;; conflict with yas/expand.  I'm not sure why I need to do
            ;; the following as other indentation code doesn't seem to
            ;; use insert-before-markers.
            (insert-before-markers (make-string next-tab-stop ?\ )))
          (when (looking-back "^\\s-*")
            (skip-chars-forward " \t"))
          )))
     (t
      (LaTeX-indent-line)))
    ;; (next-line)
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  reftex interaction
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar gpb-latex-label-prefix nil)
(make-variable-buffer-local 'gpb-latex-label-prefix)

(dolist (env '(("proposition" ?p "thm:"  "\\ref{%s}" t    nil -2)
               ("definition"  ?p "thm:"  "\\ref{%s}" t    nil -2)
               ("remark"      ?p "thm:"  "\\ref{%s}" t    nil -2)
               ("setting"     ?p "thm:"  "\\ref{%s}" t    nil -2)
               ("lemma"       ?p "thm:"  "\\ref{%s}" t    nil -2)
               ("theorem"     ?p "thm:"  "\\ref{%s}" t    nil -2)
               ("corollary"   ?p "thm:"  "\\ref{%s}" t    nil -2)
               ("example"     ?p "thm:"  "\\ref{%s}" t    nil -2)
               ("Proposition" ?p "thm:"  "\\ref{%s}" t    nil -2)
               ("Definition"  ?p "thm:"  "\\ref{%s}" t    nil -2)
               ("Remark"      ?p "thm:"  "\\ref{%s}" t    nil -2)
               ("Setting"     ?p "thm:"  "\\ref{%s}" t    nil -2)
               ("Lemma"       ?p "thm:"  "\\ref{%s}" t    nil -2)
               ("Theorem"     ?p "thm:"  "\\ref{%s}" t    nil -2)
               ("Corollary"   ?p "thm:"  "\\ref{%s}" t    nil -2)
               ("Example"     ?p "thm:"  "\\ref{%s}" t    nil -2)
               ("thmenum"     ?i "item:" "\\ref{%s}" item
 (regexp "items?" "Punkte?"))))
  (adelete 'reftex-label-alist (car env))
  (add-to-list 'reftex-label-alist env))


(defun gpb-get-next-reftex-label ()
  ;;(reftex-access-scan-info 'rescan)
  (let ((gpb-enable-record-file-on-visit nil))
  ;(reftex-reset-mode)
  (let ((reftex-insert-label-flags '(nil nil)))
    (reftex-label nil 'no-insert))))
    ;;     (prefix ""))
    ;; (save-excursion
    ;;   (beginning-of-line)
    ;;   (when (re-search-backward "\\label{\\([^}:-]+[:-]\\)" nil t)
    ;;     (setq prefix (match-string 1))))
    ;; (concat prefix (reftex-label nil 'no-insert))))

(defadvice reftex-replace-prefix-escapes (after gpb-latex-add-label-prefix ()
                                                activate)
  (setq ad-return-value
        (concat gpb-latex-label-prefix ad-return-value)))
(ad-deactivate 'reftex-replace-prefix-escapes)

(defun gpb-latex-set-label-prefix ()
  """Setup a prefix for all labels.

  Works by advicing `reftex-replace-prefix-escapes'.
  I'm actually not using this at the moment.
  """
  (let ((filename (buffer-file-name)))
    (setq gpb-latex-label-prefix
        (if (null filename)
            "noname"
          (apply 'concat (mapcar
                          (lambda (x) (downcase (substring x 0 1)))
                          (split-string (file-name-sans-extension
                                         (file-name-nondirectory
                                          (buffer-file-name))) "[- _]" t)))
          gpb-latex-label-prefix (concat gpb-latex-label-prefix ":")))))

(defun gpb-reftex-get-eqrefs ()
  ;;(reftex-reparse-document)
  (when reftex-docstruct-symbol
    (delq nil
          (mapcar
           (lambda (item)
             (when (and (listp (cdr item))
                        (stringp (cadr item))
                        (string= (cadr item) "e"))
               (cons (car item) (caddr item))))
           (copy-list (symbol-value reftex-docstruct-symbol))))))

(defun gpb-latex-relabel-using-auxfile (label-prefix aux-file-dir)
  "Renumber labels to match the document.

  Uses GPL code by Ulrik Vieth <vieth@thphy.uni-duesseldorf.de>"
  (interactive
   (list (read-string "Label prefix (eq:): " nil nil "eq:")
         (read-directory-name "Directory containing aux files: ")))
  (let ((aux-file-list (directory-files aux-file-dir t ".*\\.aux"))
        aux-match-string equation-number remap-alist)
    (save-excursion
      (setq lab-buffer (get-buffer-create "*aux-scratch*"))
      (set-buffer lab-buffer)
      (erase-buffer)		; erase left-over from previous run
      (goto-char (point-min))
      (while aux-file-list
        (let ((file-name (car aux-file-list)))
          (if (file-readable-p file-name)
              (insert-file-contents-literally file-name)
            (error "I can't find file \"%s\"" file-name)))
        (goto-char (point-max))
        (setq aux-file-list (cdr aux-file-list)))
      ;; extract all `\newlabel' commands from the ".aux" files
      ;; and discard irrelevant lines for those kinds of labels
      ;; that shouldn't be modified (e.g. sections, figures, notes)
      (goto-char (point-min))
      (delete-non-matching-lines (format "\\\\newlabel{%s"
                                         (regexp-quote label-prefix)))
      (goto-char (point-min))
      (while (re-search-forward
              "\\\\newlabel{\\(.*\\)}{{\\(.*\\)}{\\(.*\\)}}" nil t)
        (push (cons (match-string 1) (concat label-prefix (match-string 2)))
              remap-alist)))
    (gpb-latex--remap-labels remap-alist)))

(defun gpb-latex-relabel-consecutively (label-prefix)
  "Renumber all labels that start with label-prefix consecutively.

   Also corrects are references."
  (interactive (list (read-string "Label prefix (eq:): " nil nil "eq:")))
  (let ((count 1) remap-alist)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward (format "\\\\label{\\(%s.*\\)}"
                                      (regexp-quote label-prefix)) nil t)
      (push (cons (match-string 1)
                  (concat label-prefix (int-to-string count)))
              remap-alist)
      (setq count (1+ count))))
  (gpb-latex--remap-labels remap-alist)))


(defun gpb-latex-relabel (from to dont-wrap)
  "Change the label named FROM to be TO and fixup are references."
  (interactive
   (let ((this-label (save-excursion
                       (buffer-substring-no-properties
                        (progn (search-forward "}")
                               (1+ (search-backward "{")))
                        (1- (search-forward "}")))))
         (new-label (save-excursion
                      (search-forward "}")
                      (search-backward "{")
                      (gpb-get-next-reftex-label))))
     (list (read-string (format "Old label (%s): " this-label)
                        nil nil this-label)
           (read-string (format "New label (%s): " new-label)
                        nil nil new-label)
           (not (y-or-n-p "Wrap to beginning of document? "))
           )))
  (gpb-latex--remap-labels `((,from . ,to)) dont-wrap))

(defun gpb-latex--remap-labels (alist &optional dont-wrap)
  (let (new-label)
  (save-excursion
    (if dont-wrap
        (forward-line 0)
      (goto-char (point-min)))
    (while (re-search-forward
            "\\\\\\(label\\|ref\\|eqref\\)[ ]*{\\([^}]+\\)}" nil t)
      (setq new-label (aget alist (match-string 2)))
      (when new-label
        (replace-match "\\\\\\1" nil nil)
        (insert "{" new-label "}"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Context menu generators
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gpb-latex-make-environment-changer (new-env)
  `(lambda ()
      (interactive)
      (assert (looking-back "\\(\\\\begin\\|\\\\end\\){[[:word:]*]*"))
      (save-excursion
        (goto-char (match-beginning 0))
        (forward-char)
        (LaTeX-find-matching-begin)
        (search-forward-regexp "\\=\\\\begin{\\([[:word:]*]*\\)}")
        ;;(replace-match ,new-env nil nil nil 1)
        (replace-match "" nil nil nil 1)
        (gpb-insert-with-temp-overlay ,new-env)
        (forward-char)
        (LaTeX-find-matching-end)
        (search-backward-regexp "\\\\end{\\([[:word:]*]*\\)}\\=")
        (replace-match "" nil nil nil 1)
        (goto-char (match-beginning 1))
        (gpb-insert-with-temp-overlay ,new-env))))


(defun gpb-latex-generate-environment-items (&optional reverse-direction)
  (interactive)
  (save-excursion
    (when (and (looking-at "[[:word:]*]*}")
               (looking-back "\\(\\\\begin\\|\\\\end\\){[[:word:]*]*"))
      (goto-char (match-beginning 0))
      (forward-char)
      (LaTeX-find-matching-begin)
      (search-forward-regexp "\\=\\\\begin{\\([[:word:]*]*\\)}")
      (let ((env (match-string 1))
            (lists '("enumerate" "itemize" "description"
                     "compactenum" "compactitem"))
            (eqns '("equation" "equation*"
                    "align" "align*"
                    "split" "split*"
                    "gather" "gather*"
                    "alignat" "alignat*"))
            (thms '("lemma" "theorem" "proposition" "corollary" "definition"
                    "notation" "setting")))
        (cond
         ((member env lists)
          (setq lists (remove env lists))
          (cons "Change Environment"
                (mapcar (lambda (x)
                          `(,x ,(gpb-latex-make-environment-changer x)))
                        lists)))
         ((member env eqns)
          (setq eqns (remove env eqns))
          (cons "Change Environment"
                (mapcar (lambda (x)
                          `(,x ,(gpb-latex-make-environment-changer x)))
                        eqns)))
         ((member env thms)
          (setq thms (remove env thms))
          (cons "Change Environment"
                (mapcar (lambda (x)
                          `(,x ,(gpb-latex-make-environment-changer x)))
                        thms))))))))

(defun gpb-latex-generate-delimiter-items ()
  (interactive)
  (when (and (region-active-p)
             (reduce (lambda (prev open-close)
                       (or prev
                           (and (save-excursion
                                  (goto-char (region-beginning))
                                  (looking-at (car open-close))))
                           (save-excursion
                             (goto-char (region-end))
                             (looking-back (car open-close)))))
                     `(("(" . ")")
                       ("\\[" . "]")
                       ("\\\\{" . "\\\\}"))
                     ))
    `(("change delimiter size"
       `(("smaller" test2)
         ("larger" test2)
         )))))

(defun gpb-latex-generate-region-items ()
  (interactive)
  (when (region-active-p)
    `(("Wrap in environment" LaTeX-environment))))

  ;;                              ) dolist
  ;;             (save-excursion
  ;;                  (goto-char (region-beginning))
  ;;                  (looking-at (rx "\\\\
  ;; ;; Must be looking at closing delimiter or paired delimiter
  ;; (if (not (looking-back "\\s)\\|\\s$"))
  ;;     nil ; (progn (message "Not looking back at delimiter.") nil)
  ;;   (let (next-size)
  ;;     (save-excursion
  ;;       (gpb-latex-beginning-of-delimiter)
  ;;       (let ((size-match
  ;;              (search-backward-regexp
  ;;               (concat "\\(\\(:?\\\\[Bb]ig+r\\)\\|"
  ;;                       "\\(:?\\\\right\\)\\)\\=")
  ;;               nil t)))
  ;;         (if (not size-match)
  ;;             (setq next-size (if smaller "\\right"
  ;;                               "\\bigr"))
  ;;                    ;; (gpb-insert-with-temp-overlay
  ;;                    ;;  "\\bigr"
  ;;                    ;;  'face `((background-color . ,gpb-blue))))
  ;;         (setq next-size (gpb-find-next-in-list
  ;;                            (match-string 1)
  ;;                            '("\\bigr" "\\Bigr" "\\biggr" "\\Biggr"
  ;;                              "\\right" "")
  ;;                            smaller))
  ;;           (delete-region (match-beginning 1) (match-end 1)))
  ;;         (when next-size
  ;;           (gpb-make-temp-overlay (point)
  ;;                                  (progn (insert next-size)
  ;;                                         (forward-char)
  ;;                                         (point))))))
  ;;           ;; (gpb-insert-with-temp-overlay
  ;;           ;;  next-size ;'face `((background-color . ,gpb-blue))
  ;;           ;;  ))))

  ;;     (save-excursion
  ;;       (backward-sexp)
  ;;       (setq next-size
  ;;             (aget '((nil . "") ("" . "") ("\\bigr" . "\\bigl")
  ;;                     ("\\Bigr" . "\\Bigl")
  ;;                     ("\\biggr" . "\\biggl") ("\\Biggr" . "\\Biggl")
  ;;                     ("\\right" . "\\left")) next-size))

  ;;       (let ((end (point-marker))
  ;;             begin)
  ;;         (if (search-backward-regexp
  ;;              "\\(\\(:?\\\\[Bb]ig+l\\)\\|\\(:?\\\\left\\)\\)\\\\?\\="
  ;;              nil t)
  ;;             (delete-region (match-beginning 1) (match-end 1))
  ;;           (when (looking-back "\\\\") (backward-char)))
  ;;         (gpb-make-temp-overlay (point) (progn (insert next-size) (point)))
  ;;         ;; (gpb-insert-with-temp-overlay
  ;;         ;;  next-size            ;'face
  ;;         ;; `((background-color . ,gpb-blue))
  ;;         )))
  ;;           ;; (setq next-size (gpb-find-next-in-list
  ;;           ;;                  (match-string 1)
  ;;           ;;                  '("\\bigl" "\\Bigl" "\\biggl" "\\Biggl"
  ;;           ;;                    "\\left" "")))
  ;;     t))

;; (defun gpb-latex-improve-equation-items ()
;;   (message "gpb-latex-improve-equation-items")
;;   (add-to-list 'LaTeX-item-list
;;                '("align" (function (lambda () gpb-latex-item-equation))))
;;   (add-to-list 'LaTeX-item-list
;;                '("align*" (function (lambda () gpb-latex-item-equation)))))

;; (TeX-add-style-hook "amsmath" 'gpb-latex-improve-equation-items)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; Obsolete movement commands
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun gpb-latex-find-matching-begin ()
;;   (interactive)
;;   (beginning-of-line)
;;   (LaTeX-find-matching-begin))

;; (defun gpb-latex-find-matching-end ()
;;   (interactive)
;;   (end-of-line)
;;   (LaTeX-find-matching-end))

;; (defun gpb-latex-next-environment ()
;;   (interactive)
;;   (unless (eq last-command 'gpb-latex-next-environment)
;;     (push-mark))
;;   (when (looking-at-p "\\\\begin{")
;;     (forward-char 7)
;;     (LaTeX-find-matching-end))
;;   ;; (ignore-errors
;;   ;;   (end-of-line)
;;   ;;   (LaTeX-find-matching-begin)
;;   ;;   (LaTeX-find-matching-end))
;;   (search-forward "\\begin{")
;;   (recenter (round (* 0.25 (window-height))))
;;   (goto-char (match-beginning 0))
;;   ;;(backward-char 7)
;;   )
