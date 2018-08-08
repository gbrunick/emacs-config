(require 'ispell)

;; Uncomment to debug
;; (ad-activate 'ispell-skip-region)
;; (ad-deactivate 'ispell-skip-region)


(defvar gpb-ispell-skip-strings
  '("Gy\\\"ongy" "S\\^{\\i}rbu" "Jane\\v{c}ek"
    "S{\\^{\\i}}rbu" "Jane{\\v{c}}ek"
    "It\\^o" "c\\`adl\\`ag" "et al\." "Ph.D.")
  "String that ispell should skip.")

(defvar gpb-ispell-skip-regexs
  '(;;"^\\\\def .*$"
    "\\\\cite{[^}]*}"
    "\\$[^$]+\\$"
    ;; "\\\\\\[\\(.*?\n?\\)*?\\\\\\]" ;;"\\[^]]+\\]"
    )
  "Regular expressions that ispell should skip.")

(defvar gpb-ispell-skip-tex-macros
  '("thispagestyle" "eqref" "cite" "ref" "sageplot" "sageplotcentered"))

(defvar gpb-ispell-skip-environments
  '("equation", "align", "gather", "alignat", "tikzpicture",
    "python" "sage" "sagesilent"))

(defvar gpb-ispell-bad-words
  '("spacial" "are lead")
  "A list of words that are in the dictionary, but I want to treat
as mispelled.")

;; (mapcar 'regexp-quote gpb-ispell-words-with-accents)

(defun gpb-ispell ()
  (interactive)
  (let* ((beg (if mark-active (region-beginning) (beginning-of-buffer)))
         (end (if mark-active (region-end) (beginning-of-buffer)))
         (regex (gpb-util-join-strings gpb-ispell-bad-words "\\|")))
    (when (save-excursion
            (save-restriction
              (when (region-active-p) (widen) (narrow-to-region beg end))
              (goto-char (point-min))
              (re-search-forward regex nil t)))

      (goto-char (match-beginning 0))
      (error "Don't use the word \"%s\"" (match-string 0))))
  (call-interactively 'ispell t))

(defun gpb-ispell-skip-tex-def ()
  (re-search-forward "{\\|\n")
  (when (looking-back "{")
    (let ((safe-table (make-char-table 'syntax-table nil)))
      (modify-syntax-entry ?{ "(" safe-table)
      (modify-syntax-entry ?} ")" safe-table)
      (with-syntax-table safe-table
        (backward-up-list)
        (forward-sexp)))))

(defun gpb-ispell-add-tex-skip-alists ()
  (let ((macros (nth 0 ispell-tex-skip-alists))
        (environments (nth 1 ispell-tex-skip-alists)))
    (mapcar (lambda (str)
              (add-to-list 'macros (list (regexp-quote str))))
            gpb-ispell-skip-strings)
    (mapcar (lambda (regex) (add-to-list 'macros (list regex)))
            gpb-ispell-skip-regexs)
    (mapcar (lambda (macro-name)
              (add-to-list 'macros (list
                                    (concat "\\\\" (regexp-quote macro-name)
                                            "\\_>")
                                    'ispell-tex-arg-end 1)))
            gpb-ispell-skip-tex-macros)
    (add-to-list 'macros (list "\\\\def[ \\\t]" 'gpb-ispell-skip-tex-def))
    (add-to-list 'macros (cons "[^\\]\\\\\\[" "\\\\]"))
    ;; It seems to be better to just ignore the environments macro
    (mapcar (lambda (str)
              (add-to-list
               'macros
               (cons (concat "\\\\begin[ \t\n]*{[ \t\n]*"
                             (format "%s\\*?" str)
                             "[ \t\n]*}")
                     ;; This argument is where to stop the skipping
                     (format "\\\\end[ 	\n]*{[ 	\n]*%s\\*?[ 	\n]*}"
                             str))))
            gpb-ispell-skip-environments)
    ;; (add-to-list 'environments
    ;;              '("align\\*?"
    ;;                . "\\\\end[ 	\n]*{[ 	\n]*align\\*?[ 	\n]*}"))
    ;; (add-to-list 'environments
    ;;              '("gather\\*?"
    ;;                . "\\\\end[ 	\n]*{[ 	\n]*gather\\*?[ 	\n]*}"))
    (setq ispell-tex-skip-alists (list macros environments))))

(gpb-ispell-add-tex-skip-alists)

(defadvice ispell-skip-region (around gpb-debug-skip-region)
  (let ((pt (point)) text)
    ad-do-it
    (setq text (buffer-substring pt (point)))
    (with-current-buffer (get-buffer-create "*skipped-stuff*")
      (insert text)
      (insert "\n--------------------------\n"))))


;; (ad-disable-advice 'ispell-region 'before 'check-for-bad-words)

;; (defadvice ispell-region (before check-for-bad-words (beg end) activate)
;;   (let* ((regex (gpb-util-join-strings gpb-ispell-bad-words "\\|"))
;;          (bad-word (save-excursion
;;                      (save-restriction
;;                        (widen)
;;                        (narrow-to-region beg end)
;;                        (goto-char (point-min))
;;                        (when (re-search-forward regex nil t)
;;                          (match-beginning 0))))))
;;     (when bad-word
;;       (goto-char bad-word)
;;       (error "Don't use \"%s\"" (match-string 0)))))


(provide 'gpb-ispell)
