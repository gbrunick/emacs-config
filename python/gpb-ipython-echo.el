;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  gpb-ipython-echo
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(assert (= emacs-major-version 24))

(define-minor-mode gpb-ipython-echo-mode
  "This is global minor mode that modifies shell interaction to show input.

This minor mode advices `python-shell-send-string' and adds a
filter to `comint-output-filter-functions' in order to provide
some indication that code has been sent to the python interpreter
behind the scenes."
  :global t
  (cond
   (gpb-ipython-echo-mode
    (assert (eq emacs-major-version 24))
    (ad-enable-advice 'python-shell-send-string 'before
                      'gpb-py:echo-input-advice)
    (ad-enable-advice 'python-shell-send-setup-code 'before
                      'gpb-py:echo-input-advice)
    (ad-enable-advice 'python-shell-send-buffer 'before
                      'gpb-py:echo-input-advice)
    (ad-enable-advice 'python-shell-send-region 'before
                      'gpb-py:echo-input-advice)
    (add-hook 'comint-output-filter-functions
              'gpb-py:ipython-echo-output-filter t))

   (t
    (ad-disable-advice 'python-shell-send-string 'before
                       'gpb-py:echo-input-advice)
    (ad-disable-advice 'python-shell-send-setup-code 'before
                       'gpb-py:echo-input-advice)
    (ad-disable-advice 'python-shell-send-buffer 'before
                       'gpb-py:echo-input-advice)
    (ad-disable-advice 'python-shell-send-region 'before
                       'gpb-py:echo-input-advice)
    (remove-hook 'comint-output-filter-functions
                 'gpb-py:ipython-echo-output-filter t)))
  ;; According to the emacs lisp manual, `ad-enable-advice' and
  ;; `ad-enable-advice' only set flags and you must call `ad-activate' to
  ;; actually update the function defintion.
  (ad-activate 'python-shell-send-string)
  (ad-activate 'python-shell-send-setup-code)
  (ad-activate 'python-shell-send-buffer)
  (ad-activate 'python-shell-send-region))


(defun gpb-py:current-place-holder (&optional offset)
  (unless (boundp 'gpb-py:current-place-holder:count)
    (setq gpb-py:current-place-holder:count 1))
  (format "***python-mode command %i***"
          (+ gpb-py:current-place-holder:count (or offset 0))))


(defun gpb-py:increment-place-holder ()
  (unless (boundp 'gpb-py:current-place-holder:count)
    (setq gpb-py:current-place-holder:count 1))
  (incf gpb-py:current-place-holder:count))


(defun gpb-py:register-replacement (string &optional offset)
  (unless (boundp 'gpb-py:register-replacement:replacement-alist)
    (setq gpb-py:register-replacement:replacement-alist nil))
  (unless (aget gpb-py:register-replacement:replacement-alist
                (gpb-py:current-place-holder offset))
    (aput 'gpb-py:register-replacement:replacement-alist
          (gpb-py:current-place-holder offset) string)))


(defun gpb-py:get-replacement-alist ()
  (when (boundp 'gpb-py:register-replacement:replacement-alist)
    gpb-py:register-replacement:replacement-alist))


(defun gpb-py:ipython-echo-output-filter (str)
  (gpb-log-forms 'gpb-py:ipython-echo-output-filter
                 'comint-last-output-start
                 '(point))
  (save-excursion
    (let ((replacements (reverse (gpb-py:get-replacement-alist))))
      (when (marker-position comint-last-output-start)
        (goto-char comint-last-output-start)
        (while replacements
          (gpb-log-forms 'gpb-py:ipython-echo-output-filter
                         '(mapcar 'car replacements))
          (let ((place-holder (car (car replacements)))
                (replacement (cdr (car replacements))))
            (setq replacements (cdr replacements))
            (when (search-forward place-holder nil t)
              (delete-region (match-beginning 0) (match-end 0))
              (with-silent-modifications
                (unless comint-use-prompt-regexp
                  (add-text-properties comint-last-output-start (point)
                                       '(front-sticky
                                         (field
                                          inhibit-line-move-field-capture)
                                         rear-nonsticky t
                                         field output
                                         inhibit-line-move-field-capture t)))
                (add-text-properties
                 (save-excursion (forward-line 0) (point)) (point)
                 '(font-lock-face comint-highlight-prompt)))
              (insert replacement)
              (set-marker comint-last-output-start (point))
              (adelete 'gpb-py:register-replacement:replacement-alist
                       place-holder))))))))


(defadvice python-shell-send-string (before gpb-py:echo-input-advice
                                            (string &optional process msg))
  (when (= (length (split-string string "\n" t)) 1)
    (gpb-py:register-replacement (propertize string
                                  'front-sticky t
                                  'font-lock-face 'comint-highlight-input))
    (setq string (format "print '%s'; %s"
                         (gpb-py:current-place-holder) string))
    (gpb-py:increment-place-holder)
    (comint-snapshot-last-prompt)))

(defun gpb-py:describe-setup-code (symbol)
  (describe-variable symbol)
  (with-current-buffer (help-buffer)
    (let ((inhibit-read-only t))
      (save-excursion
        (while (search-forward "\\n" nil t)
          (replace-match "\n" nil t))))))

(defadvice python-shell-send-setup-code (before gpb-py:echo-input-advice)
  (let ((offset 0))
    (dolist (code python-shell-setup-codes)
      (let ((keymap (make-sparse-keymap))
            (func `(lambda () (interactive)
                     (gpb-py:describe-setup-code ',code))))
        (define-key keymap "\C-m" func)
        (define-key keymap "[mouse-2]" func)
        (gpb-py:register-replacement
         (concat "Setup code: "
                 (propertize (format "%S" code)
                             'front-sticky t
                             'rear-nonsticky t
                             'face 'button
                             'font-lock-face 'button
                             'mouse-face 'highlight
                             'keymap keymap
                             'help-echo "mouse-2, RET: describe variable"))
         offset)
        (incf offset)))))

(defadvice python-shell-send-buffer (before gpb-py:echo-input-advice)
  (let ((keymap (make-sparse-keymap))
        (func `(lambda () (interactive)
                 ,(if (buffer-file-name)
                      `(find-file-other-window ,(buffer-file-name))
                    `(pop-to-buffer ,(buffer-name))))))
    (define-key keymap "\C-m" func)
    (define-key keymap "[mouse-2]" func)
    (gpb-py:register-replacement
     (concat "Execute buffer: "
             (propertize (buffer-name)
                         'front-sticky t
                         'rear-nonsticky t
                         'face 'button
                         'font-lock-face 'button
                         'mouse-face 'highlight
                         'keymap keymap
                         'help-echo "mouse-2, RET: jump to buffer")))))


(defadvice python-shell-send-region (before gpb-py:echo-input-advice
                                            (start end))
  (let ((keymap (make-sparse-keymap))
        (func `(lambda () (interactive)
                 ,(if (buffer-file-name)
                      `(find-file-other-window ,(buffer-file-name))
                    `(pop-to-buffer ,(buffer-name)))
                 (goto-char ,start)
                 (set-mark ,end))))
    (define-key keymap "\C-m" func)
    (define-key keymap "[mouse-2]" func)
    (gpb-py:register-replacement
     (concat "Execute region: "
             (propertize (format "%s, lines %s-%s" (buffer-name)
                                 (line-number-at-pos start)
                                 (line-number-at-pos end))
                         'front-sticky t
                         'rear-nonsticky t
                         'face 'button
                         'font-lock-face 'button
                         'mouse-face 'highlight
                         'keymap keymap
                         'help-echo "mouse-2, RET: jump to buffer")))))


(provide 'gpb-ipython-echo)
