;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Define the commands and text objects.
;;
;;  Uses the text object infrastructure provided by
;;  gpb-text-objects-base.el and the utility functions from
;;  gpb-text-objects-util.el.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)
(require 'gpb-text-objects-base)
(require 'gpb-text-objects-util)

(defun gpb-tobj--reload ()
  "Reload the source files."
  (gpb-tobj--reset-all-keymaps)
  (let ((gpb-tobj--enable-warnings nil))
    (dolist (filename '("gpb-text-objects-base.el"
                        "gpb-text-objects-util.el"
                        "gpb-text-objects.el"))
      (ignore-errors
        (with-current-buffer (find-buffer-visiting filename)
          (save-buffer)))
      (load filename))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Setup the keymap that store the text objects
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(gpb-tobj--declare-keymap 'root "The main text object keymap")
(gpb-tobj--set-default-keymap 'root)

;; Update global text object keymap to define text object modifiers
(gpb-tobj--global-set-key "1" 'gpb-tobj--update-count)
(gpb-tobj--global-set-key "2" 'gpb-tobj--update-count)
(gpb-tobj--global-set-key "3" 'gpb-tobj--update-count)
(gpb-tobj--global-set-key "4" 'gpb-tobj--update-count)
(gpb-tobj--global-set-key "5" 'gpb-tobj--update-count)
(gpb-tobj--global-set-key "6" 'gpb-tobj--update-count)
(gpb-tobj--global-set-key "7" 'gpb-tobj--update-count)
(gpb-tobj--global-set-key "8" 'gpb-tobj--update-count)
(gpb-tobj--global-set-key "9" 'gpb-tobj--update-count)
(gpb-tobj--global-set-key "-" 'gpb-tobj--toogle-backwards-option)
(gpb-tobj--global-set-key "n" 'gpb-tobj--set-next-flag)
(gpb-tobj--global-set-key "i" 'gpb-tobj--set-inner-flag)
(gpb-tobj--global-set-key "o" 'gpb-tobj--set-outer-flag)

(defvar execute-text-object-function nil
  "The function that is used to execute code in a buffer.
When this buffer local variable should be bound to a function
that accepts two arguments and execute the reion defined by thow
points.  Used by `execute-text-object'.")
(make-variable-buffer-local 'execute-text-object-function)

(defun gpb-tobj--set-inner-flag ()
  "Set inner flag"
  (interactive)
  (gpb-tobj--set-text-object-modifier :inner t))

(defun gpb-tobj--set-outer-flag ()
  (interactive)
  (gpb-tobj--set-text-object-modifier :outer t))

(defun gpb-tobj--toogle-backwards-option ()
  (interactive)
  (gpb-tobj--set-text-object-modifier
   :backwards (not (gpb-tobj--get-text-object-modifier :backwards))))

(defun gpb-tobj--set-next-flag ()
  (interactive)
  (gpb-tobj--set-text-object-modifier :next t))

(defun gpb-tobj--update-count ()
  "Update the number of text objects that should be acted upon."
  (interactive)
  (let ((count (gpb-tobj--get-text-object-modifier :count))
        (key-sequence (this-command-keys)))
    (assert (= (length key-sequence) 1))
    (let ((digit (- (elt key-sequence 0) ?0)))
      (gpb-log-forms 'gpb-tobj--update-count 'count 'key-sequence 'digit)
      (cond
       ((null count)
        (gpb-tobj--set-text-object-modifier :count digit))
       ((integerp count)
        (gpb-tobj--set-text-object-modifier :count (+ (* count 10) digit)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Commands that act on text objects
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar mark-text-object-hook nil "Hook called after `mark-text-object'")

;; The special case logic in `mark-text-object' for extending the region
;; doesn't seem to generalize, so we handle this command individually
;; rather than making `gpb-tobj--define-command' more complicated.  The
;; drawback to this approach is that `mark-text-object' is much more
;; sensitive to implementation changes in gpb-text-objects-base.el than the
;; other text object commands.
(defun mark-text-object (arg)
  "Mark a text object or extend the current region"
  (interactive "p")
  (unless (repeat-is-really-this-command) (gpb-tobj--read-text-object 'root))
  (unless (gpb-tobj--get-text-object-modifier :count)
    (gpb-tobj--set-text-object-modifier :count arg))
  (let* ((obj gpb-tobj--current-text-object)
         (count (gpb-tobj--get-text-object-modifier :count 1))
         (dir (if (gpb-tobj--get-text-object-modifier :backwards) -1 1))
         ;; (find-func (gpb-tobj--get-text-object-property obj :find-func))
         (extend-region-func (gpb-tobj--get-text-object-property
                              obj :extend-region-func))
         beg end)
    (assert (member dir '(1 -1)))
    (multiple-value-bind (beg end)
        (cond
         ;; If the region is active and :extend-region-func is defined,
         ;; use this function to extend the region.
         ((and (use-region-p) extend-region-func)
          (apply extend-region-func (region-beginning) (region-end)
                 gpb-tobj--current-text-object-modifiers))
         ;; If the region is active and we don't have :extend-region-func,
         ;; should just mark some stuff?
         ((use-region-p)
          (error "Not implemented"))
         ;; Default case
         (t
          (apply 'gpb-tobj--find-text-object
                 obj (point)
                 gpb-tobj--current-text-object-modifiers)))
      (push-mark (point) t nil)
      (case dir
        (1
         (push-mark beg nil t)
         (goto-char end))
        (-1
         (push-mark end nil t)
         (goto-char beg))
        (otherwise
         (error "Runtime error"))))))

(gpb-tobj--define-command delete-text-object (obj beg end)
  "Delete the text object and don't save in kill ring."
  :keymap root
  (let* ((text (gpb-tobj--region-string beg end))
         (beg (min beg (point)))
         (end (max end (point))))
    ;; (goto-char end)
    (unless mark-active (gpb-tobj--flash-region beg end))
    (delete-region beg end)
    (unless (window-minibuffer-p)
      (message "Deleted \"%s\"" text))))

(gpb-tobj--define-command kill-text-object (obj beg end)
  "Delete the text object and save it in kill ring."
  :keymap root
  (let ((text (gpb-tobj--region-string beg end))
        (beg (min beg (point)))
        (end (max end (point))))
    ;; (goto-char end)
    (unless mark-active (gpb-tobj--flash-region beg end))
    (kill-region beg end)
    (unless (window-minibuffer-p)
      (message "Cut \"%s\"" text))))

(gpb-tobj--define-command kill-and-append-text-object (obj beg end)
  "Delete the text object and append it to the kill ring."
  :keymap root
  (let ((text (gpb-tobj--region-string
               )))
    (setq last-command 'kill-region)
    (kill-region (min beg (point)) (max end (point)))
    (unless (minibuffer-window-active-p (selected-window))
      (message "Cut \"%s\"" text))))

(gpb-tobj--define-command replace-text-object (obj beg end)
  :keymap root
  (goto-char beg)
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'face 'region)
    (setq cursor-type 'bar)
    (unwind-protect
        (let ((replacement (read-string "Replacement: ")))
          (delete-overlay ov)
          (delete-region beg end)
          (insert replacement))
      (delete-overlay ov))))


(gpb-tobj--define-command replace-text-object-with-clipboard  (obj beg end)
  "Delete the current text object and insert item from kill ring."
  :keymap root
  ;; Maybe it is not a good idea to try and indent here?
  (let ((indent (or ""
                    (and (save-excursion
                           (goto-char beg)
                           (and (looking-at "^\\([ \t]*\\)[^ \t\n]")
                                (match-string 1))))
                    "")))
    (goto-char beg)
    (delete-region beg end)
    (insert indent)
    (yank)))

(gpb-tobj--define-command copy-text-object (obj beg end)
  "Copy text item to kill ring."
  :keymap root
  (gpb-tobj--flash-region beg end)
  (copy-region-as-kill beg end)
  (unless (minibuffer-window-active-p (selected-window))
    (message "Copied \"%s\"" (gpb-tobj--region-string beg end))))

(gpb-tobj--define-command copy-append-text-object (obj beg end)
  "Append text object to the first item in the kill ring."
  :keymap root
  (gpb-tobj--flash-region beg end)
  (let ((last-command 'kill-region))
    (copy-region-as-kill beg end))
  (unless (minibuffer-window-active-p (selected-window))
    (message "Appended \"%s\"" (gpb-tobj--region-string beg end))))

(gpb-tobj--define-command goto-beginning-of-text-object (obj beg end)
  "Move point to the beginning of text object"
  :keymap root :ignore-region t :object-modifiers (:backwards t)
  (cond
   ((and (minibuffer-window-active-p (selected-window))
         (eq obj 'line))
    (move-beginning-of-line 1))
   (t
    (goto-char beg))))

(gpb-tobj--define-command goto-end-of-text-object (obj beg end &rest modifiers)
  "Position point after the last non-whitespace character in text object"
  :keymap root :ignore-region t
  (if (eq obj 'buffer)
      (end-of-buffer)
    (goto-char
     (save-excursion
       (let ((pt (point)))
         (goto-char end)
         (skip-chars-backward " \t\n")
         (unless (> (point) pt)
           (goto-char (cadr (gpb-tobj--find-text-object obj end modifiers)))
           (skip-chars-backward " \t\n")
           (when (<= (point) pt) (error "Search failed"))))
       (point)))))

(gpb-tobj--define-command goto-next-text-object (obj beg end &rest modifiers)
  "Move the point to the beginning of the next text object."
  :keymap root :ignore-region t ;; :object-modifiers (:next t)
  (while (<= beg (point))
    (multiple-value-setq (beg end)
      (gpb-tobj--find-text-object obj end modifiers)))
  (goto-char beg))

(gpb-tobj--define-command indent-text-object (obj beg end)
  "Indent the text object"
  :keymap root
  (gpb-tobj--flash-region beg end)
  (indent-region beg end))

(gpb-tobj--define-command comment/uncomment-text-object (obj beg end)
  "Indent the text object"
  :keymap root
  (gpb-tobj--flash-region beg end)
  (comment-or-uncomment-region beg end))

(gpb-tobj--define-command isearch-for-text-object (obj beg end)
  "Indent the text object"
  :keymap root
  (let ((text (buffer-substring-no-properties beg end)))
    (goto-char beg)
    (isearch-forward nil t)
    (isearch-yank-string text)
    (isearch-search-and-update)))

(gpb-tobj--define-command execute-text-object (obj beg end)
   "Execute the text object as python code."
   (when (null execute-text-object-function)
     (error "execute-text-object-function is not defined in this buffer"))
   (let* ((number-of-args (length (cadr (symbol-function
                                         execute-text-object-function))))
          (args (cond
                 ((= number-of-args 3) (list obj beg end))
                 ((= number-of-args 2)
                  (warn "Deprecated call signature: %s"
                        execute-text-object-function)
                  (list beg end))
                 (t
                  (error "Function %S should accept 3 arguments."
                         execute-text-object-function)))))
     (gpb-tobj--flash-region beg end)
     (apply execute-text-object-function args)
     (setq deactivate-mark t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Flat text objects (objects which cannot be nested)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro gpb-tobj--define-flat-text-object (symbol doc &rest kwargs)
  "Define a forward function based text object that may not be nested.
See `gpb-tobj--define-text-object' for more info."
  (declare (indent 1) (doc-string 2))
  (let* ((forward-func (gpb-tobj--remove-keyword-arg kwargs :forward-func t))
         (extend-region-func
          `(lambda (beg end &rest modifiers)
             (let ((dir (if (plist-get modifiers :backwards) -1 1))
                   (count (or (plist-get modifiers :count) 1)))
               (case dir
                 (1
                  (list beg
                        (cadr (gpb-tobj--find-flat-text-object
                               end dir ',forward-func count))))
                 (-1
                  (list (car (gpb-tobj--find-flat-text-object
                              beg dir ',forward-func count))
                        end)))))))
    `(gpb-tobj--define-text-object ,symbol (pos &rest modifiers)
       ,doc
       ,@kwargs
       :extend-region-func ,extend-region-func
       (let ((dir (if (plist-get modifiers :backwards) -1 1))
             (count (or (plist-get modifiers :count) 1))
             (next (plist-get modifiers :next))
             (outer (plist-get modifiers :outer)))
         (condition-case exc
             (cond
              (next
               (gpb-tobj--find-next-flat-text-object pos dir
                                                     ',forward-func count))
              (t
               (gpb-tobj--find-flat-text-object pos dir ',forward-func count)))
           (search-failed (error "No %ss %s point" ',symbol
                                  (if (eq dir 1) "after" "before"))))))))

(defun gpb-tobj--find-flat-text-object (pos dir forward-func count)
  "Signals 'search-failed on failure."
  (let ((pos (or pos (point)))
        (dir (or dir 1))
        (count (or count 1))
        beg end)
    (assert (and (integerp count) (> count 0)))
        (save-excursion
          (goto-char pos)
          (case dir
            (1
             (funcall forward-func 1)
             (when (eq (point) pos) (signal 'search-failed "Forward function did not move point"))
             (setq beg (save-excursion (funcall forward-func -1) (point)))
             (while (> count 1) (funcall forward-func 1) (decf count))
             (list beg (point)))
            (-1
             (funcall forward-func -1)
             (when (eq (point) pos) (signal 'search-failed "Forward function did not move point"))
             (setq end (save-excursion (funcall forward-func 1) (point)))
             (while (> count 1) (funcall forward-func -1) (decf count))
             (list (point) end))
            (t
             (error "Runtime error"))))))


(defun gpb-tobj--find-next-flat-text-object (pos dir forward-func count)
  "Find the first text object which does not contain the point."
  (multiple-value-bind (beg end)
      (gpb-tobj--find-flat-text-object pos dir forward-func 1)
    (case dir
      (1
       (if (< pos beg)
           (gpb-tobj--find-flat-text-object pos 1 forward-func count)
         (gpb-tobj--find-flat-text-object end 1 forward-func count)))
      (-1
       (if (< end pos)
           (gpb-tobj--find-flat-text-object pos -1 forward-func count)
         (gpb-tobj--find-flat-text-object beg -1 forward-func count)))
      (t
       (error "Runtime error")))))


(font-lock-add-keywords 'emacs-lisp-mode
 `(( ,(concat "(\\(gpb-tobj--define-flat-text-object\\)"
             "[ \t]+\\(\\(\\sw\\|\\s_\\)+\\)")
    (1 font-lock-keyword-face)
    (2 font-lock-variable-name-face))))

;; (font-lock-add-keywords 'emacs-lisp-mode
;;  `(( ,(concat "(\\(gpb-tobj--define-next-flat-text-object\\)"
;;              "[ \t]+\\(\\(\\sw\\|\\s_\\)+\\)")
;;     (1 font-lock-keyword-face)
;;     (2 font-lock-variable-name-face))))

;; (font-lock-add-keywords 'emacs-lisp-mode
;;  `(( ,(concat "(\\(gpb-tobj--define-nested-text-object\\)"
;;              "[ \t]+\\(\\(\\sw\\|\\s_\\)+\\)")
;;     (1 font-lock-keyword-face)
;;     (2 font-lock-variable-name-face))))


(gpb-tobj--define-flat-text-object word
  "the current word"
  :forward-func gpb-tobj--forward-word
  :key-binding (root "w")
  :key-binding (root "W" :backwards t))

;; (gpb-tobj--define-next-flat-text-object next-word
;;   "the next word"
;;   :forward-func gpb-tobj--forward-word
;;   :key-binding (next "w")
;;   :key-binding (next "W" :backwards t))

(defun gpb-tobj--forward-word (arg)
  (interactive "p")
  (case arg
    (1 (re-search-forward "\\w\\b"))
    (-1 (re-search-backward "\\b\\w"))
    (t (error "Runtime error"))))


(gpb-tobj--define-flat-text-object sentence
  "the current sentence"
  :forward-func forward-sentence
  :key-binding (root ".")
  :key-binding (root "," :backwards t))


(gpb-tobj--define-flat-text-object symbol
  "the current symbol"
  :forward-func gpb-tobj--forward-symbol
  :key-binding (root "s")
  :key-binding (root "S" :backwards t))

;; (gpb-tobj--define-next-flat-text-object next-symbol
;;   "the next symbol"
;;   :forward-func gpb-tobj--forward-symbol
;;   :key-binding (next "s")
;;   :key-binding (next "S" :backwards t))

(defun gpb-tobj--in-string-p ()
  "Return non-nil if point is in a string.
This is a modified version of the function from thing-at-pt."
  (let ((orig (point)))
    (save-excursion
      (end-of-line)
      (beginning-of-defun)
      (nth 3 (parse-partial-sexp (point) orig)))))

(defun gpb-tobj--forward-symbol (arg)
  (interactive "p")
  (let ((in-string (ignore-errors (gpb-tobj--in-string-p))))
    (while (> arg 0)
      (re-search-forward "\\sw\\|\\s_")
      (skip-syntax-forward "w_")
      (decf arg))
    (while (< arg 0)
      (re-search-backward "\\sw\\|\\s_")
      (skip-syntax-backward "w_")
      (incf arg))))

    ;; (while (not (= arg 0))
    ;;   (forward-thing 'symbol (signum arg))
    ;;   ;; If we moved into a string, move out
    ;;   (while (and (not in-string) (in-string-p))
    ;;     (forward-thing 'symbol (signum arg)))
    ;;   (when (and (boundp 'outer) outer (< arg 0))
    ;;     (skip-syntax-backward "'"))
    ;;   (setq arg (- arg (signum arg))))))


(gpb-tobj--define-text-object buffer (pos &rest modifiers)
  "the entire buffer"
  :key-binding (root "b")
  :key-binding (root "B" :backwards t)
  (list (point-min) (point-max)))


(defvar gpb-tobj--last-region nil)
(make-variable-buffer-local 'gpb-tobj--last-region)

(defun gpb-tobj--save-last-region ()
  (interactive)
  (condition-case exc
      (let ((mark-even-if-inactive t))
        (unless gpb-tobj--last-region
          (setq gpb-tobj--last-region (list (make-marker) (make-marker))))
        (set-marker (elt gpb-tobj--last-region 0) (region-beginning))
        (set-marker (elt gpb-tobj--last-region 1) (region-end)))
    ('error (setq gpb-tobj--last-region nil))))

(add-hook 'deactivate-mark-hook 'gpb-tobj--save-last-region)

(gpb-tobj--define-text-object last-region (pos &rest modifiers)
  "the last regions that was active"
  :key-binding (root "!")
  (unless gpb-reg--last-region (error "No previous region"))
  gpb-tobj--last-region)


(gpb-tobj--define-flat-text-object string
  "A string delimited by \" or \'"
  :forward-func gpb-tobj--forward-string
  :key-binding (root "'")
  :key-binding (root "\"" :backwards t))

;; (gpb-tobj--define-next-flat-text-object next-string
;;   "The next string delimited by \" or \'"
;;   :forward-func gpb-tobj--forward-string
;;   :key-binding (next "'")
;;   :key-binding (next "\"" :backwards t))

(defun gpb-tobj--forward-string (arg)
  (interactive "p")
  (cond
   ((> arg 0)
    (while (> arg 0)
      ;; Move past the first quote
      (skip-syntax-forward "^\"|")
      (skip-syntax-forward "\"|")
      ;; If we are now sitting after the first quote of a string, we
      ;; move again to the end of the string.
      (when (nth 3 (syntax-ppss))
        (skip-syntax-forward "^\"|")
        (skip-syntax-forward "\"|"))
      (decf arg)))
   ((< arg 0)
    (while (< arg 0)
      (skip-syntax-backward "^\"|")
      (skip-syntax-backward "\"|")
      (when (nth 3 (syntax-ppss))
        (skip-syntax-backward "^\"|")
        (skip-syntax-backward "\"|"))
      (incf arg)))))


(gpb-tobj--define-text-object line (pos &rest modifiers)
  "A line of text"
  :key-binding (root "l")
  :key-binding (root "L" :backwards t)
  (save-excursion
    (goto-char pos)
    (let ((count (or (plist-get modifiers :count) 1))
          (backwards (plist-get modifiers :backwards))
          (inner (plist-get modifiers :inner))
          (outer (plist-get modifiers :outer)))
      (cond
       ((and backwards inner)
        (list (save-excursion (forward-line (1+ (- count)))
                              (back-to-indentation)
                              (point))
              (progn (move-end-of-line nil)
                     (skip-chars-backward " \t")
                     (point))))
       ((and backwards outer)
        (list (save-excursion (forward-line (1+ (- count)))
                              (point))
              (progn (forward-line 1)
                     (point))))
       ((and backwards)
        (list (save-excursion (move-beginning-of-line (+ 2 (- count)))
                              (point))
              (progn (forward-line 1)
                     ;; (move-end-of-line nil)
                     (point))))
       ((and inner)
        (list (save-excursion (back-to-indentation)
                              (point))
              (progn (forward-line (1- count))
                     (move-end-of-line nil)
                     (skip-chars-backward " \t")
                     (point))))
       ((and outer)
        (list (save-excursion (forward-line 0)
                              (point))
              (progn (forward-line count)
                     (point))))
       (t
        (list (save-excursion (move-beginning-of-line 1)
                              (point))
              (progn (forward-line count)
                     ;; (forward-line (1- count))
                     ;; (move-end-of-line nil)
                     (point))))))))


;; (gpb-tobj--define-flat-text-object line
;;   "A line of text including the trailing newline"
;;   :forward-func gpb-tobj--forward-line
;;   :key-binding (root "l")
;;   :key-binding (root "L" :backwards t))

;; (gpb-tobj--define-next-flat-text-object next-line
;;   "The next line of text including the trailing newline"
;;   :forward-func gpb-tobj--forward-line
;;   :key-binding (next "l")
;;   :key-binding (next "L" :backwards t))

(defun gpb-tobj--forward-line (arg)
  (interactive "p")
  (cond
   ((and (< arg 0) (bolp))
    (forward-line arg))
   ((and (< arg 0) (not (bolp)))
    (forward-line (1+ arg)))
   (t
    (forward-line arg))))


(gpb-tobj--define-flat-text-object paragraph
  "A paragraph"
  :forward-func gpb-tobj--forward-paragraph
  :key-binding (root "p")
  :key-binding (root "P" :backwards t))

;; (gpb-tobj--define-next-flat-text-object next-paragraph
;;   "The next paragraph"
;;   :forward-func gpb-tobj--forward-paragraph
;;   :key-binding (next "p")
;;   :key-binding (next "P" :backwards t))

(defun gpb-tobj--forward-paragraph (arg)
  (interactive "p")
  (when (< arg 0)
    (unless (bolp) (forward-line 1))
    (while (< arg 0)
      (let ((pt (point)))
        ;; skip backwards over blank lines
        (while (and (not (bobp)) (looking-back "^[ \t]*\n"))
          (forward-line -1))
        (forward-line 0)
        ;; skip backwards over nonblank lines
        (while (not (or (bobp) (looking-back "^[ \t]*\n")))
          (forward-line -1))
        (when (>= (point) pt) (signal 'search-failed "First paragraphs"))
        (incf arg))))
  (when (> arg 0)
    (let ((pt (point)))
      (forward-line 0)
      (while (> arg 0)
        ;; skip forwards over blank lines
        (while (and (not (eobp)) (looking-at "[ \t]*\n")) (forward-line))
        ;; skip forwards over nonblank lines
        (while (not (or (eobp) (looking-at "[ \t]*\n"))) (forward-line))
        (when (<= (point) pt) (signal 'search-failed "Last paragraphs"))
        (decf arg)))))


(gpb-tobj--define-flat-text-object VIM-WORD
  "A continguous run of non-whitespace characters"
  :forward-func gpb-tobj--forward-VIM-WORD
  :key-binding (root " ")
  :key-binding (root [(shift ?\ )] :backwards t))

;; (gpb-tobj--define-next-flat-text-object next-VIM-WORD
;;   "A continguous run of non-whitespace characters"
;;   :forward-func gpb-tobj--forward-VIM-WORD
;;   :key-binding (next " ")
;;   :key-binding (next [(shift ?\ )] :backwards t))

(defun gpb-tobj--forward-VIM-WORD (arg)
  (interactive "p")
  (let ((pt (point)))
    (cond
     ((> arg 0)
      (while (> arg 0)
        ;; skip over space
        (skip-chars-forward " \t\n")
        ;; skip to end of WORD
        (skip-chars-forward "^ \t\n")
        (when (eq (point) pt) (signal 'search-failed "End of buffer"))
        (decf arg)))
     ((< arg 0)
      (while (< arg 0)
        ;; skip over space
        (skip-chars-backward " \t\n")
        ;; skip to end of WORD
        (skip-chars-backward "^ \t\n")
        (when (eq (point) pt) (signal 'search-failed "Beginning of buffer"))
        (incf arg))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    Nested text objects
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro gpb-tobj--define-nested-text-object (symbol doc &rest properties)
  "Define a nested, forward function-based text object.

The PROPERTIES plist must contain a :forward-func key/value pair.

If PROPERTIES plist contains an :inner-forward-func key/value
pair, then this function is used when the :inner text object
modifier is set.

If PROPERTIES plist contains a :predicate key/value pair...

See also `gpb-tobj--define-text-object' for more info.
"
  (declare (indent 1) (doc-string 2))
  (assert (stringp doc))
  (let* ((forward-func (plist-get properties :forward-func))
         (inner-forward-func (plist-get properties :inner-forward-func))
         (pred (plist-get properties :predicate)))
    (assert (functionp forward-func))
    `(gpb-tobj--define-text-object ,symbol (pos &rest modifiers)
       ,doc
       ,@properties
       :extend-region-func
         (lambda (beg end &rest modifiers)
           (condition-case exc
               ,(if inner-forward-func
                    `(if (plist-get modifiers :inner)
                         (apply 'gpb-tobj--extend-nested-text-object
                                beg end ',inner-forward-func modifiers)
                       (apply 'gpb-tobj--extend-nested-text-object
                              beg end ',forward-func modifiers))
                  `(apply 'gpb-tobj--extend-nested-text-object
                          beg end ',forward-func modifiers))
             (search-failed (error "Search failed"))))

       ;;(message "Searching for nested text object...")
       (condition-case exc
           ,(if inner-forward-func
                `(if (plist-get modifiers :inner)
                     (apply 'gpb-tobj--find-nested-text-object
                            pos ',inner-forward-func nil modifiers)
                   (apply 'gpb-tobj--find-nested-text-object
                          pos ',forward-func ,pred modifiers))
              `(apply 'gpb-tobj--find-nested-text-object
                      pos ',forward-func ,pred modifiers))
         (search-failed (error "Search failed"))))))


(defun gpb-tobj--find-nested-text-object (pos forward-func pred &rest modifiers)
  "Find the first text object that begins at or after point.

The default algorithm for finding a text object.  When DIR=1
(resp. DIR=-1), we:

1. Place the point at POS.

2. Temporarily skip backwards (resp. forwards) over any spaces or
   tabs.  If the point is now sitting at the beginning
   (resp. end) of a text object, we immediately return the
   smallest text object that begins (resp. ends) at the point.
   Otherwise, return the point to POS.

3. Scan strictly forward (resp. backward) until we reach the
   first text object boundary and consider the set of all text
   objects that begin or end at this point.

4. If there are text objects that begin at the point and text
   objects that end at the point, we discard all the text objects
   that begin at the point (resp. end at the point).

5. Return the smallest remaining text object.
"
  (assert (number-or-marker-p pos))
  (assert (functionp forward-func))
  (let ((dir (if (plist-get modifiers :backwards) -1 1))
        (count (or (plist-get modifiers :count) 1))
        (next (plist-get modifiers :next))
        (outer (plist-get modifiers :outer)))
    (assert (member dir '(1 -1)))
    (assert (integerp count))
    (multiple-value-bind (beg end)
        (cond
         (outer
          ;; Find the smallest text object that contains the character
          ;; following (resp. preceding) the point.
          (car (gpb-tobj--find-minimal-nested-text-object
                pos dir forward-func
                (cond
                 ((and pred (> dir 0))
                  `(lambda (beg end) (and (funcall ,pred beg end)
                                          (and (<= beg pos)
                                               (>= end (+ pos 1))))))
                 ((> dir 0)
                  `(lambda (beg end) (and (<= beg pos) (>= end (+ pos 1)))))
                 ((and pred (< dir 0))
                  `(lambda (beg end) (and (funcall ,pred beg end)
                                          (and (<= beg (- pos 1))
                                               (>= end pos)))))
                 ((< dir 0)
                  `(lambda (beg end) (and (<= beg (- pos 1)) (>= end pos))))
                 (t (error "Runtime error"))))))

         (t
          (save-excursion
            (if (> dir 0) (skip-chars-backward " \t")
              (skip-chars-forward " \t"))
            (catch 'done
              (while t
                ;; Scan forward (resp. backwards) and then backtrack
                ;; to find the first object boundary.
                (let ((pos (point))
                      (side (- dir))
                      (obj-count (catch 'multiple-boundaries
                                   (funcall forward-func dir) 1))
                      boundaries boundary)
                  (save-excursion
                    (while (>= (* (- (point) pos) dir) 0)
                      (push (list (point) side obj-count) boundaries)
                      (setq side      dir
                            obj-count (catch 'multiple-boundaries
                                        (funcall forward-func (- dir)) 1)))
                    (while boundaries
                      (multiple-value-setq (boundary side obj-count) (car boundaries))
                      (setq boundaries (cdr boundaries))
                      (save-excursion
                        (goto-char boundary)
                        (dotimes (x obj-count)
                          (gpb-tobj--move-to-boundary-of-nested-text-object
                           side forward-func (> x 0))
                          (let ((beg (min boundary (point)))
                                (end (max boundary (point))))
                            (when (and (or (null next) (= dir side))
                                       (or (null pred) (funcall pred beg end)))
                              (throw 'done (list beg end))))))))))))))

      (gpb-tobj--extend-nested-text-object
       beg end forward-func :dir dir :count (1- count) :outer outer))))


(defun gpb-tobj--extend-nested-text-object (beg end forward-func &rest modifiers)
  "Returns a new (beg end) list."
  (assert (number-or-marker-p beg))
  (assert (number-or-marker-p end))
  (assert (functionp forward-func))
  (let ((dir (if (plist-get modifiers :backwards) -1 1))
        (count (or (plist-get modifiers :count) 1))
        (next (plist-get modifiers :next))
        (outer (plist-get modifiers :outer)))
    (assert (member dir '(1 -1)))
    (assert (integerp count))
    (cond
     ((<= count 0)
      (list beg end))
     (outer
      (let ((pred `(lambda (beg end)
                     (or (and (< beg ,beg) (<= ,end end))
                         (and (<= beg ,beg) (< ,end end))))))
        ;; Find the smallest text object that contains COUNT text objects
        ;; that each strictly contain the region delimited by BEG and END.
        ;; A text object contains itself for the purposes of this
        ;; calculation.
        (car
         (gpb-tobj--find-minimal-nested-text-object
          beg 1 forward-func pred count))))
     (t
      (multiple-value-bind (beg2 end2)
          (case dir
            (1  (apply 'gpb-tobj--find-nested-text-object
                       end forward-func nil (plist-put modifiers :count 1)))
            (-1 (apply 'gpb-tobj--find-nested-text-object
                       beg forward-func nil (plist-put modifiers :count 1))))
        (apply 'gpb-tobj--extend-nested-text-object
               (min beg beg2) (max end end2) forward-func
               (plist-put modifiers :count (1- count))))))))


;; Add syntax highlighting for macros
(font-lock-add-keywords 'emacs-lisp-mode
 `((,(concat "(\\(gpb-tobj--define-nested-text-object\\)"
             "[ \t]+\\(\\(\\sw\\|\\s_\\)+\\)")
    (1 font-lock-keyword-face)
    (2 font-lock-variable-name-face))))

(gpb-tobj--define-nested-text-object indented-block
  "An indented block of text"
  :forward-func gpb-tobj--forward-indented-text-block
  :key-binding (root ">")
  :key-binding (root "<" :backwards t))

(gpb-tobj--define-nested-text-object python-class
  "A Python class"
  :forward-func gpb-tobj--forward-indented-text-block
  :predicate (lambda (beg end) (save-excursion
                                 (goto-char beg)
                                 (looking-at-p "[ \t]*class\\_>")))
  :key-binding (root "c")
  :key-binding (root "C" :backwards t))

(defun gpb-tobj--forward-defun (arg)
  (interactive "p")
  (cond
   ((> arg 0)
    (end-of-defun arg))
   ((< arg 0)
    (beginning-of-defun (- arg)))))

(gpb-tobj--define-nested-text-object defun
  "A function definition"
  :forward-func gpb-tobj--forward-defun
  :key-binding (root "d")
  :key-binding (root "D" :backwards t))


;; (gpb-tobj--define-nested-text-object indented-block
;;   "An indented text block"
;;   :forward-func gpb-tobj--forward-block
;;   :key-binding (root ".")
;;   :key-binding (root "," :backwards t))

;; (gpb-tobj--define-nested-text-object outer-indented-block
;;   "An indented text block"
;;   :forward-func gpb-tobj--forward-block :outer t
;;   :key-binding (outer ".")
;;   :key-binding (outer "," :backwards t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Regular expression delimited, nested text objects
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro gpb-tobj--define-regex-text-object (symbol doc &rest kwargs)
  "Define a forward function based text object."
  (declare (indent 1) (doc-string 2))
  (assert (stringp doc))
  (let ((begin-regex (gpb-tobj--remove-keyword-arg kwargs :begin-regex t))
        (end-regex (gpb-tobj--remove-keyword-arg kwargs :end-regex t))
        (predicate (gpb-tobj--remove-keyword-arg kwargs :predicate)))
    `(gpb-tobj--define-nested-text-object ,symbol
       ,doc
       :forward-func (lambda (arg)
                       (gpb-tobj--forward-regex
                        arg ',begin-regex ',end-regex ',predicate))
       ,@kwargs)))

(gpb-tobj--define-regex-text-object parenthesis
  "Text object delimited by \"(\" and \")\""
  :begin-regex "(" :end-regex ")"
  :key-binding (root ")")
  :key-binding (root "(" :backwards t))

;; (gpb-tobj--define-regex-text-object inner-parenthesis
;;   "Text inside \"(\" and \")\""
;;   :begin-regex ("(\\(\\)" . 1) :end-regex ("\\(\\))" . 1)
;;   :key-binding (inner ")")
;;   :key-binding (inner "(" :backwards t))

;; (gpb-tobj--define-regex-text-object outer-parenthesis
;;   "Text object delimited by \"(\" and \")\" which contains the point"
;;   :outer t
;;   :begin-regex "(" :end-regex ")"
;;   :key-binding (outer ")")
;;   :key-binding (root "W" :backwards t))

(gpb-tobj--define-regex-text-object square-braces
  "Text between \"[\" and \"[\""
  :begin-regex "\\[" :end-regex "]"
  :key-binding (root "]")
  :key-binding (root "[" :backwards t))

(gpb-tobj--define-regex-text-object curly-braces
  "Text between \"(\" and \")\""
  :begin-regex "{" :end-regex "}"
  :key-binding (root "}")
  :key-binding (root "{" :backwards t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Lisp mode customization
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; TODO



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  LaTeX mode customization
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun gpb-tobj--forward-latex-item (arg)
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

(gpb-tobj--define-regex-text-object latex-environment
  "Text object delimited by \"(\" and \")\""
  :begin-regex "\\(^[ \t]*\\)?\\\\begin{[^{]*}"
  :end-regex "\\\\end{[^}]*}\\([ \t]*\\)?"
  :key-binding (root "e")
  :key-binding (root "E" :backwards t))

;; (gpb-tobj--define-regex-text-object outer-latex-environment
;;   "Text object delimited by \"(\" and \")\""
;;   :begin-regex "\\(^[ \t]*\\)?\\\\begin{[^{]*}"
;;   :end-regex "\\\\end{[^}]*}\\([ \t]*\\)?"
;;   :predicate TeX-in-comment
;;   :outer t
;;   :key-binding (outer "e")
;;   :key-binding (outer "E" :backwards t))

;; (defun gpb-tobj--latex-mode-hook ()
  ;; (gpb-tobj--define-nested-text-object item
  ;;     ([(hyper i)] . [(hyper shift i)])
  ;;   :keymap root :local t
  ;;   (gpb-tobj--forward-latex-item arg))

  ;; (gpb-tobj--define-nested-text-object inline-math ("$" . "#")
  ;;   :keymap root :local t
  ;;   (while (> arg 0)
  ;;     (re-search-forward "\\$")
  ;;     (when (font-latex-faces-present-p '(font-latex-math-face))
  ;;       (re-search-forward "\\$"))
  ;;     (decf arg))
  ;;   (while (< arg 0)
  ;;     (re-search-backward "\\$")
  ;;     (when (save-excursion (backward-char)
  ;;                           (font-latex-faces-present-p '(font-latex-math-face)))
  ;;       (re-search-backward "\\$"))
  ;;     (incf arg)))

  ;; (gpb-tobj--define-nested-text-object inner-inline-math "$"
  ;;   :keymap inner :local t
  ;;   (when (> arg 0)
  ;;     (while (> arg 0)
  ;;       (forward-char)
  ;;       (re-search-forward "\\$")
  ;;       (when (font-latex-faces-present-p '(font-latex-math-face))
  ;;         (re-search-forward "\\$"))
  ;;       (decf arg))
  ;;     (goto-char (match-beginning 0)))

  ;;   (when (< arg 0)
  ;;     (while (< arg 0)
  ;;       (backward-char)
  ;;       (re-search-backward "\\$")
  ;;       (when (save-excursion
  ;;               (backward-char)
  ;;               (font-latex-faces-present-p '(font-latex-math-face)))
  ;;         (re-search-backward "\\$"))
  ;;       (incf arg))
  ;;       (goto-char (match-end 0))))

  ;; (gpb-tobj--define-regex-text-object environment
  ;;   "a LaTeX environment"
  ;;   :keymap root :local t
  ;;   :open-regex "\\(^[ \t]*\\)?\\\\begin{[^{]*}"
  ;;   :close-regex "\\\\end{[^}]*}\\([ \t]*\n\\)?"
  ;;   :forward-key "e" :backward-key "E")

;; (defmacro gpb-tobj--define-next-nested-text-object (symbol doc &rest kwargs)
;;   "Define a forward function based text object that may not be nested."
;;   (declare (indent 1) (doc-string 2))
;;   (let* ((forward-func (gpb-tobj--remove-keyword-arg kwargs :forward-func t))
;;          (extend-region-func
;;           `(lambda (beg end &rest modifiers)
;;              (let ((dir (if (plist-get modifiers :backwards) -1 1))
;;                    (count (or (plist-get modifiers :count) 1)))
;;                (case dir
;;                  (1
;;                   (list beg
;;                         (cadr (gpb-tobj--find-next-nested-text-object
;;                                end dir ',forward-func count))))
;;                  (-1
;;                   (list (car (gpb-tobj--find-next-nested-text-object
;;                               beg dir ',forward-func count))
;;                         end)))))))
;;     `(gpb-tobj--define-text-object ,symbol (pos &rest modifiers)
;;        ,doc
;;        ,@kwargs
;;        :extend-region-func ,extend-region-func
;;        (let ((dir (if (plist-get modifiers :backwards) -1 1))
;;              (count (or (plist-get modifiers :count) 1))
;;              (next (plist-get modifiers :next)))
;;          (condition-case exc
;;              (cond
;;               (next
;;                (gpb-tobj--find-next-flat-text-object (point) dir
;;                                                      ',forward-func count))

;;               (t
;;                (gpb-tobj--find-next-nested-text-object (point) dir
;;                                                        ',forward-func count)))
;;            (search-failed (error "No %ss %s point" ',symbol
;;                                   (if (eq dir 1) "after" "before"))))))))

;; (add-hook 'LaTeX-mode-hook 'gpb-tobj--latex-mode-hook)
;; (remove-hook 'LaTeX-mode-hook 'gpb-tobj--latex-mode-hook)

(provide 'gpb-text-objects)
