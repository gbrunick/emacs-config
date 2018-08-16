;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Customization of Lisp-related modes
;;

(require 'easymenu)
(require 'edebug)

(define-key lisp-mode-shared-map "\t" 'gpb-lisp-tab-command)
(define-key lisp-mode-shared-map [(meta tab)] 'company-manual-begin)
(define-key lisp-mode-shared-map [(control meta tab)] 'lisp-complete-symbol)
(define-key emacs-lisp-mode-map "\C-c\C-c" 'eval-buffer)
(define-key emacs-lisp-mode-map "\C-c\C-u" 'gpb-lisp-run-unit-tests)
(define-key edebug-mode-map [(control c)(w)] 'gpb-watch-variable-at-point)


(defun gpb-lisp-eval-line ()
  (interactive)
  (end-of-line)
  (eval-last-sexp nil)
  (forward-sexp))

(defun gpb-eval-prev-defun (arg)
  (interactive "p")
  (save-excursion
    (beginning-of-defun)
    (eval-defun arg)))

(defun gpb-lisp-tab-command (arg)
  "Try to complete the symbol, otherwise do a tab."
  (interactive "P")
  (cond
   ;; If we just indented this line, then move down and indent the
   ;; next line
   ((eq last-command 'indent-for-tab-command)
    (setq this-command 'indent-for-tab-command)
    (forward-line)
    (indent-for-tab-command arg))

   ;; If the region is active or the point is in front of all the
   ;; nonwhitespace characters in a line, then we indent.
   ((or (use-region-p) (looking-back "^\\s *"))
    (setq this-command 'indent-for-tab-command)
    (indent-for-tab-command arg)
    (deactivate-mark))

   ;; When the point is after a word or symbol
   ((and (> (point) 1)
         (looking-back "\\w\\|\\s_" (1- (point)))
         ;;(looking-at "[[:space:]]\\|)\\|$")
         )
    (unless (looking-at "[[:space:]]\\|)\\|$")
      (save-excursion (insert " ")))
    (completion-at-point)
    ;; (gpb-company-try-to-complete-common)
    ;; (company-manual-begin)
    ;; (company-complete-common)
    )))

(defun gpb-lisp-end-of-sexp-p ()
  (let ((current-point (point)))
    (save-excursion
      (backward-sexp)
      (forward-sexp)
      (= (point) current-point))))

(defun gpb-lisp-top-form-name ()
  (save-excursion
    (condition-case exception
        (progn
          (end-of-defun)
          (beginning-of-defun)
          (re-search-forward
           (concat
            "^[[:space:]]*(\\(defun\\|defvar\\)[[:space:]]+"
            "\\(\\(\\s_\\|\\sw\\)+\\)"))
          (buffer-substring-no-properties
           (match-beginning 2) (match-end 2)))
      (error (message "No top form") nil)
      (scan-error (message "No top form") nil))))


(defun gpb-eval-last-or-new-sexp (arg)
  (interactive
   (list (let ((minibuffer-completing-symbol t))
           (read-from-minibuffer "Eval: "
				 (prin1-to-string (preceding-sexp))
                                 read-expression-map t
				 'read-expression-history
                                 ))
	 current-prefix-arg))
  (eval-expression arg))

(defun gpb-eval-line ()
  (interactive)
  (end-of-line)
  (eval-last-sexp nil)
  (skip-chars-forward " \t\n"))

(defun eval-text-object (obj beg end)
  (cond
   ((eq obj 'defun)
    (save-excursion
      (goto-char beg)
      (eval-defun (not (null current-prefix-arg)))))
   (t
    (eval-region beg end))))

(defun gpb-common-lisp-init ()
  (interactive)
  (eldoc-mode 1)
  (when (require 'gpb-modal nil t)
    (gpb-modal--define-command-key
     "\t" 'gpb-back-to-indentation-or-indent-according-to-mode t)
    (setq execute-text-object-function 'eval-text-object)))

(defun gpb-lisp-eval-something (arg)
  (interactive "P")
  (cond
   ((use-region-p)
    (eval-region (region-beginning) (region-end)
                 (if (eq major-mode 'lisp-interaction-mode)
                     (current-buffer)
                   t)))
   ((eq major-mode 'lisp-interaction-mode)
    (unless (bolp) (insert "\n"))
    (pp-eval-last-sexp t))
   ((save-excursion
      (backward-sexp)
      (and (looking-at "(defun[ \t]")
           (call-interactively 'eval-defun))))
   (t
    (eval-last-sexp nil))))

(defun gpb-emacs-lisp-mode-init ()
  (interactive)
  (gpb-common-lisp-init)
  ;;(imenu-add-to-menubar "Index")
  (local-set-key [(control return)] 'gpb-lisp-eval-something))

(add-hook 'emacs-lisp-mode-hook 'gpb-emacs-lisp-mode-init)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Misc functions
;;

(defun gpb-lisp-run-unit-tests ()
  (interactive)
  (require 'elk-test)
  (find-file-other-window
   (gpb-util-reduce-string (buffer-file-name) ".el$" ".elk"))
  (elk-test-run-buffer nil t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; edebug-mode
;;
;; Edebug doesn't seem to call a nice hook, so we have to write into
;; the local key map.
;;

(defun gpb-watch-previous-sexp ()
  (interactive)
  (if (not (and (boundp 'edebug-active) edebug-active))
      (message "Edebug not active.")
    (push (preceding-sexp) edebug-eval-list)
    (save-window-excursion (edebug-eval-redisplay))))

(defun gpb-watch-variable-at-point ()
  (interactive)
  (if (not (and (boundp 'edebug-active) edebug-active))
      (message "Edebug not active.")
    (let ((symbol (gpb-symbol-at-point)))
      (message "var %S" symbol)
      (push symbol edebug-eval-list)
      (save-window-excursion (edebug-eval-redisplay)))))

(defun gpb-lisp-make-edebug-context-menu ()
  (append
     (list
      ;; If there is a symbol at the point
      (let ((symbol (gpb-symbol-at-point)))
        (when symbol
          `[,(format "Watch %s" symbol)
            gpb-watch-variable-at-point t]))

      ["Watch sexp" gpb-watch-previous-sexp t]
      ["Run to here" edebug-goto-here t]
      ["Set breakpoint" edebug-set-breakpoint t]
      ["edebug-items-separator-2" nil :label "--"])

     ;; edebug submenu
     (list (lookup-key (current-local-map) [(menu-bar)(Edebug)]))

     (gpb-add-file-items)
     (gpb-make-index-menu)
     (gpb-add-window-items)
     (gpb-add-main-menu-item)))

 ;; (gpb-context-menu-install-menu 'gpb-lisp-make-edebug-context-menu
 ;;                                edebug-mode-map)


;; Don't save window configurations when debugging.
(setq edebug-save-windows nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; inferior-emacs-lisp-mode
;;
;; IELM runs lisp in a comint buffer.
;;

(require 'ielm)

(defun run-elisp nil
  "Interactively evaluate Emacs Lisp expressions.
Switches to the buffer `*ielm*', or creates it if it does not exist."
  (interactive)
  (let (old-point (this-buffer (current-buffer)))
    (unless (comint-check-proc "*ielm*")
      (with-current-buffer (get-buffer-create "*ielm*")
	(unless (zerop (buffer-size)) (setq old-point (point)))
	(inferior-emacs-lisp-mode)))
    (switch-to-buffer-other-window "*ielm*")
    (when old-point (push-mark old-point))
    (ielm-change-working-buffer this-buffer)))


(defun gpb-ielm-copy-or-eval ()
  (interactive)
  (if (comint-after-pmark-p)
      (let ((inhibit-read-only t)
            (ielm-dynamic-return t))
        (ielm-return))
    (let ((old-input (funcall comint-get-old-input)))
      (goto-char (point-max))
      (while (looking-back "\n")
        (backward-char))
      (insert old-input))))


(defun gpb-inferior-emacs-lisp-mode-init ()
  (interactive)
  (gpb-common-lisp-init)
  (local-set-key [(tab)] 'lisp-complete-symbol)
  (local-set-key [(control tab)] nil)
  (local-set-key "\C-m" 'gpb-ielm-copy-or-eval))


(add-hook 'inferior-emacs-lisp-mode-hook 'gpb-inferior-emacs-lisp-mode-init)


(defun gpb-lisp-goto-definition (symbol &optional arg)
  "Goto the definition of SYMBOL.
When called interactively with a prefix argument, we show the
definition in other window."
  (interactive
   (let* ((fn (symbol-at-point))
          (enable-recursive-minibuffers t)
          (val (completing-read
                (if fn
                    (format "Goto symbol definition (default %s): " fn)
                  "Goto symbol definition: ")
                obarray nil t nil nil (and fn (symbol-name fn)))))
     (list (if (equal val "") fn (intern val)) current-prefix-arg)))
  (push-mark nil t nil)
  (cond
   ((or (and (not (boundp symbol)) (fboundp symbol))
        (and (boundp symbol) (fboundp symbol)
             (y-or-n-p "Goto function definition? ")))
    (if arg
        (find-function-other-window symbol)
      (find-function symbol)))
   ((boundp symbol)
    (if arg
        (find-variable-other-window symbol)
      (find-variable symbol)))
   (t
    (error "Undefined symbol %s" symbol))))

