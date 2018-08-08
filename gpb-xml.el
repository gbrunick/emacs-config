(eval-after-load "sgml-mode"
  '(define-key sgml-mode-map "\C-c]" 'sgml-close-tag))
(autoload 'jinja2-mode "jinja2-mode" nil t)

;; sgml-mode
(defun gpb-init-sgml-mode ()
  (put 'sgml-skip-tag-backward 'CUA 'move)
  (put 'sgml-skip-tag-forward 'CUA 'move)
  (define-key sgml-mode-map [(meta left)] 'sgml-skip-tag-backward)
  (define-key sgml-mode-map [(meta right)] 'sgml-skip-tag-forward)
  (define-key sgml-mode-map [(meta up)] 'gpb-backward-indent)
  (define-key sgml-mode-map [(meta down)] 'gpb-forward-indent))
(eval-after-load "sgml-mode" '(gpb-init-sgml-mode))

(defun gpb-nxml-mark-element ()
  (interactive)
  (nxml-backward-up-element)
  (nxml-forward-element)
  (push-mark nil t t)
  (nxml-backward-element))

(defun gpb-nxml-indent-line ()
  "Indent current line as XML."
  (let ((indent (nxml-compute-indent))
        (from-end (- (point-max) (point))))
    (cond
     ((and indent (/= indent (current-indentation)))
      (beginning-of-line)
      (let ((bol (point)))
        (skip-chars-forward " \t")
        (delete-region bol (point)))
      (indent-to indent)
      (when (> (- (point-max) from-end) (point))
        (goto-char (- (point-max) from-end))))
     ((looking-back "^[ \t]*")
      (skip-chars-forward " \t")))))

(defun gpb-nxml-tab-command ()
  (interactive)
  (if (and (looking-back "\\sw\\|\\s_") (eolp))
    (progn
      (save-excursion (skip-syntax-backward "w_")
                      (insert "<"))
      (nxml-balanced-close-start-tag-inline))
    (indent-for-tab-command)))

(eval-after-load "nxml-mode"
  '(progn
     (define-key nxml-mode-map "\C-c]" 'nxml-finish-element)
     (define-key nxml-mode-map "\C-c." 'gpb-nxml-mark-element)
     (define-key nxml-mode-map "\C-i" 'gpb-nxml-tab-command)
     (add-hook 'nxml-mode-hook (lambda ()
                                 (setq indent-line-function
                                       'gpb-nxml-indent-line)))))
