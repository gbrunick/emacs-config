;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; We customize:
;;     emacs-lisp-mode
;;     lisp-interaction-mode

;;(require 'imenu)
(require 'easymenu)
(require 'edebug)
(require 'company)


;; (require 'completion-ui)
;;(require 'auto-complete)
;; (require 'gpb-context-menu)
(require 'gpb-logging)

(setq company-elisp-detect-function-context nil)

(global-set-key [remap eval-buffer] 'gpb-eval-buffer)
(global-set-key [(meta e)(b)] 'eval-buffer)
;;(global-set-key [remap ielm] 'gpb-open-ielm-here)

(define-key lisp-mode-shared-map "\t" 'gpb-lisp-tab-command)
(define-key lisp-mode-shared-map [(meta tab)] 'company-manual-begin)
(define-key lisp-mode-shared-map [(control meta tab)] 'lisp-complete-symbol)
(define-key lisp-mode-shared-map [(meta e)(l)] 'gpb-lisp-eval-line)
(define-key lisp-mode-shared-map [(meta e)(d)] 'gpb-eval-prev-defun)
(define-key lisp-mode-shared-map [(meta e)(s)] 'eval-last-sexp)
(define-key lisp-mode-shared-map [(hyper g)(d)] 'gpb-lisp-goto-definition)
(define-key lisp-mode-shared-map [(control c)(h)]
  'gpb-get-information-about-symbol)

(define-key emacs-lisp-mode-map "\C-c\C-c" 'eval-buffer)
(define-key emacs-lisp-mode-map "\C-c\C-u" 'gpb-lisp-run-unit-tests)

(define-key edebug-mode-map [(control c)(w)] 'gpb-watch-variable-at-point)


; For easy re-execution of changes to emacs lisp files

(defvar gpb-eval-buffer-function nil
  "`gpb-eval-buffer' calls this function if non-nil.")
(put 'gpb-eval-buffer-function 'safe-local-variable
     '(lambda (arg) t))


(defun gpb-eval-buffer ()
  "Eval the buffer and say that you did it."
  (interactive)
  (if gpb-eval-buffer-function
      (funcall gpb-eval-buffer-function)
    (save-buffer)
    (eval-buffer)
    (message "Saved and executed %s" (buffer-file-name))))

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

(defun gpb-lisp-make-eval-menu ()
  (interactive)
  (let* ((top-form-symbol (gpb-lisp-top-form-name)))
    (list
     ["lisp-eval-separator-1" nil :label "--"]
     (when top-form-symbol
       `[,(format "Define         %s" top-form-symbol) eval-defun t])

     (when top-form-symbol
       `[,(format "Instrument %s" top-form-symbol) edebug-defun
         :keys "C-u C-M-x"])

     ;; (when top-form-symbol
     ;;   `[,(format "Instrumentf %s" top-form-symbol)
     ;;     (lambda () (interactive)
     ;;       (edebug-eval-top-level-form)
     ;;       (message "Edebug: %s" (gpb-lisp-top-form-name)))
     ;;     ::keys "C-u C-M-x"])
     (if (gpb-lisp-end-of-sexp-p)
         ["Eval last sexp" eval-last-sexp t]
       ["Eval expression" eval-expression t])
     ["Eval buffer" gpb-eval-buffer]
     ["lisp-eval-separator-2" nil :label "--"])))

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

;; (defun gpb-lisp-make-context-menu ()
;;   "Make the lisp context menu"
;;   (let ((result (append (gpb-lisp-add-symbol-items)
;;                         (gpb-add-file-items)
;;                         (gpb-lisp-make-eval-menu)
;;                         (gpb-make-index-menu)
;;                         (gpb-add-window-items)
;;                         (gpb-add-main-menu-item))))
;;     result))

(defun gpb-eval-line ()
  (interactive)
  (end-of-line)
  (eval-last-sexp nil)
  (skip-chars-forward " \t\n"))

(defun gpb-common-lisp-init ()
  (interactive)
  (when (member 'company features)
    (set (make-variable-buffer-local 'company-backends) '(company-elisp))
    (company-mode))
  ;; (make-local-variable 'ac-sources)
  ;; (setq ac-sources '(ac-source-symbols))
  ;; (auto-complete-mode 1)
  (when (member 'gpb-context-menu features)
    (gpb-cm:add-context-menu-items 'gpb-lisp:generate-context-menu-items t))
  (when (member 'gpb-modal features)
    (gpb-modal--define-command-key
     "\t" 'gpb-back-to-indentation-or-indent-according-to-mode t))
  (add-to-list 'imenu-generic-expression
               '("Section" ";;;;;;+ *\n\\(;+ *\n\\)?;+ *\\(.*\\)" 2) t)
  (eldoc-mode 1)
  (set (make-variable-buffer-local 'outline-regexp) "(\\|;;")
  (setq-local execute-text-object-function 'eval-region))

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

 ;; (defadvice ielm (around open-ielm-here nil activate)
 ;;   (let* ((this-buffer (current-buffer)))
 ;;     ad-do-it
 ;;     (ielm-change-working-buffer this-buffer)))


(defun gpb-lisp-define-ielm-context-menu ()
  (append (gpb-lisp-add-symbol-items)
          (gpb-add-file-items)
	  (gpb-lisp-make-eval-menu)
          (gpb-add-main-menu-item)
          (gpb-add-window-items)))

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
  ;;(message "gpb-inferior-emacs-lisp-mode-init...")
  (gpb-common-lisp-init)
  (local-set-key [(tab)] 'lisp-complete-symbol)
  (local-set-key [(control tab)] nil)
  (local-set-key "\C-m" 'gpb-ielm-copy-or-eval)
  ;;(setq ielm-dynamic-return nil)
  ;;(local-set-key "\C-m" 'gpb-comint-goto-error-or-send)
  ;;(gpb-context-menu-install-menu 'gpb-lisp-define-ielm-context-menu)
  )

(add-hook 'inferior-emacs-lisp-mode-hook 'gpb-inferior-emacs-lisp-mode-init)
 ;;(message "Added hook.")

 ;; (setq initial-major-mode 'lisp-interaction-mode)
 ;; (with-current-buffer (get-buffer-create "*scratch*")
 ;;   (lisp-interaction-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   Context menu code
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 ;; (defun gpb-lisp-generate-defun-items ()
 ;;   ;;(when (looking-at "(defun \\(\\(\\w\\|\\s_\\)+\\)")
 ;;   (list `("Eval defun"
 ;;             ;; ,(format "Eval defun" (gpb-util-truncate-string
 ;;             ;;                           (match-string-no-properties 1)
 ;;             ;;                           'right 20))
 ;;             eval-defun :acts-on-point)
 ;;           `("Instrument defun"
 ;;             ;; ,(format "Instrument %s" (gpb-util-truncate-string
 ;;             ;;                           (match-string-no-properties 1)
 ;;             ;;                           'right 20))
;;             eval-defun :acts-on-point (:use-args t))))

(defun gpb-lisp:generate-context-menu-items ()
  "Add menu items if there is a symbol at the mouse click."
  (let ((symbol (symbol-at-point)))
    (when symbol
      (let* ((symbol (symbol-at-point))

             (symbol-name (format "%s" symbol))
             (name-width 22)
             (extra-space "")
             (function-instrumented (and (fboundp symbol)
                                         (consp (get symbol 'edebug))))
             (function-logged (gpb-log--function-is-begin-logged-p symbol))
             (menu-items (delq nil (list
                 (when (fboundp symbol)
                   `("Describe function" (describe-function ',symbol)
                     :key-binding describe-function))
                 (when (boundp symbol)
                   `("Describe variable" (describe-variable ',symbol)
                     :key-binding describe-variable))
                 (when (fboundp symbol)
                   `("Goto function definition"
                     (find-function-other-window ',symbol)
                     :key-binding gpb-lisp-goto-definition))
                 (when (boundp symbol)
                   `("Goto variable definition"
                     (find-variable-other-window ',symbol)
                     :key-binding gpb-lisp-goto-definition))
                 (when (and (fboundp symbol) (not function-instrumented))
                   `("Instrument function"
                     (edebug-instrument-function ',symbol)))
                 (when (and (fboundp symbol) function-instrumented)
                   `("Uninstrument function"
                     gpb-lisp-edebug-uninstrument-function-at-point))
                 (when (and (fboundp symbol) (not function-logged))
                   `("Enable logging"
                     (gpb-log--enable-logging ',symbol)))
                 (when (and (fboundp symbol) function-logged)
                   `("Disable logging"
                     (gpb-log--disable-logging ',symbol)))))))
        (when menu-items
          `(separator
            ,(format "%s" symbol)
            ,@menu-items
            separator))))))

(defun gpb-lisp-goto-definition (symbol &optional arg)
  (interactive
   (let ((fn (symbol-at-point))
	 (enable-recursive-minibuffers t)
	 val)
     (setq val (completing-read
                (if fn
                    (format "Goto symbol definition (default %s): " fn)
                  "Goto symbol definition: ")
                obarray nil t nil nil
                (and fn (symbol-name fn))))
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
      (find-variable symbol)))))

(defun gpb-get-information-about-symbol ()
  (interactive)
  (require 'anything-config)
  (when (and (boundp gpb-modal-mode) gpb-modal-mode)
    (gpb-modal--enter-insert-mode))
  (anything
   :prompt "Info about: "
   ;;:candidate-number-limit 5
   :sources '(anything-c-source-emacs-commands
              anything-c-source-emacs-functions
              anything-c-source-emacs-variables
              anything-c-source-info-emacs
              anything-c-source-info-elisp
              anything-c-source-info-gnus
              anything-c-source-info-org
              anything-c-source-info-cl
              anything-c-source-emacs-source-defun)))

 ;; (defun gpb-lisp-describe-function-at-point ()
 ;;   (interactive)
 ;;   (describe-function (symbol-at-point)))

 ;; (defun gpb-lisp-describe-variable-at-point ()
 ;;   (interactive)
 ;;   (describe-variable (symbol-at-point)))

 ;; (defun gpb-lisp-find-function (symbol)
 ;;   (interactive
 ;;    (list (let* ((fn (symbol-at-point))
 ;;                 (fn-name (completing-read "Find function: " obarray
 ;;                                           'fboundp t
 ;;                                           (and fn (symbol-name fn)))))
 ;;            (intern fn-name))))
 ;;   (find-function-do-it symbol nil 'pop-to-buffer))

 ;; (defun gpb-lisp-find-variable (symbol)
 ;;   (interactive
 ;;    (list (let* ((fn (symbol-at-point))
 ;;                 (fn-name (completing-read "Find variable: " obarray
 ;;                                           'boundp t
 ;;                                           (and fn (symbol-name fn)))))
 ;;            (intern fn-name))))
 ;;   (find-function-do-it symbol 'defvar 'pop-to-buffer))

 ;; (defun gpb-lisp-find-variable-at-point ()
 ;;   (interactive)
 ;;   (find-function-do-it (symbol-at-point) 'defvar 'pop-to-buffer))

 ;; (defun gpb-lisp-edebug-instrument-function-at-point ()
 ;;   (interactive)
 ;;   (edebug-instrument-function (symbol-at-point)))

(defun gpb-lisp-edebug-uninstrument-function-at-point ()
  (interactive)
  (let ((edebug-info (get (symbol-at-point) 'edebug))
        (edebug-all-defs nil)
        func-marker)
    (assert (and (consp edebug-info)
                 (markerp (setq func-marker (car edebug-info)))))
    (with-current-buffer (marker-buffer func-marker)
      (goto-char func-marker)
      (eval-defun nil))))

;; Very slow...
(defun gpb-lisp-symbol-has-info-p (symbol)
  (and (commandp symbol)
       (save-window-excursion
	 (Info-find-emacs-command-nodes symbol))))

(defun gpb-lisp-info-for-function-at-point ()
  (interactive)
  (Info-goto-emacs-command-node (symbol-at-point)))
