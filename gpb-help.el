(define-key help-mode-map [(control c)(b)] 'help-go-back)
(define-key help-mode-map [(control c)(f)] 'help-go-forward)
(setq help-window-select t)

;; Override the default help-function-def so that is open the function
;; in the same window.

(defun gpb-help-function-def (fun file)
  (require 'find-func)
  (when (eq file 'C-source)
    (setq file
          (help-C-file-name (indirect-function fun) 'fun)))
  ;; Don't use find-function-noselect because it follows
  ;; aliases (which fails for built-in functions).
  (let ((location
         (find-function-search-for-symbol fun nil file)))
    (switch-to-buffer (car location))
    (if (cdr location)
        (goto-char (cdr location))
      (message "Unable to find location in file"))))

(defun gpb-help-variable-def (var &optional file)
  (when (eq file 'C-source)
    (setq file (help-C-file-name var 'var)))
  (let ((location (find-variable-noselect var file)))
    (switch-to-buffer (car location))
    (if (cdr location)
        (goto-char (cdr location))
      (message "Unable to find location in file"))))

(define-button-type 'help-function-def
  :supertype 'help-xref
  'help-function 'gpb-help-function-def
  'help-echo (purecopy "mouse-2, RET: find function's definition"))

(define-button-type 'help-variable-def
  :supertype 'help-xref
  'help-function 'gpb-help-variable-def
  'help-echo (purecopy "mouse-2, RET: find variable's definition"))

(defun gpb-help-add-context-menu ()
  (gpb-cm:add-context-menu-items 'gpb-generate-lisp-symbol-items t))
(add-hook 'help-mode-hook 'gpb-help-add-context-menu)
