;; Various utility functions that I find useful.
;;
;; To run unit tests:  "\C-c\C-u"
;;

(eval-when-compile '(require cl))
(require 'assoc)

(defvar gpb-debug-flag nil
  "Set to enable debugging info messages.")

(defun gpb-toggle-debug-flag ()
  (interactive)
  (setq gpb-debug-flag (not gpb-debug-flag)))

 ;; (defvar gpb-debug-filter nil
 ;;   "Set to enable debugging info messages.")

 ;; (defun gpb-util-debug-message (flag&rest args)
 ;;   (when gpb-debug-flag
 ;;     (with-current-buffer (get-buffer-create "*GPB-Debug*")
 ;;       (save-excursion
 ;;         (goto-char (point-max))
 ;;         (insert (apply 'format args) "\n")))))

(defun logval (form &optional label do-it)
  "Evaluate form and write value to \"*Debug Log*\" buffer

  Obsolete.  Use `log-forms'."
  ;(error "logval is now obsolete")
  (when do-it
    (let* ((value (eval form))
           (win (get-buffer-window "*Debug Log*"))
           (at-point-max (when win
                           (with-selected-window win
                             (equal (point) (point-max))))))
      (when (stringp value)
        (setq value (gpb-util-truncate-string value 'center 250)))
      (with-current-buffer (get-buffer-create "*Debug Log*")
        (save-excursion
          (goto-char (point-max))
          (insert (if label
                      (format "%s: %S=%S\n" label form value)
                    (format "%S=%S\n" form value)))))
      (when at-point-max
        (with-selected-window win
          (goto-char (point-max)))))))

 ;; (defun log-message (&rest args)
 ;;   "Write message to \"*Debug Log*\" buffer"
 ;;   (error "obsolete")
 ;;   (when (car (last args))
 ;;     (with-current-buffer (get-buffer-create "*Debug Log*")
 ;;       (save-excursion
 ;;         (goto-char (point-max))
 ;;         (insert (apply 'format (butlast args)))
 ;;         (insert "\n")))))

 ;; (make-obsolete 'log-message 'gpb-log-message)

 ;; (defmacro logval2 (form &optional label)
 ;;   (list 'let `((value (eval ,form)))
 ;;         '(when (stringp value)
 ;;            (setq value (gpb-util-truncate-string value 'center 250)))
 ;;         (if label
 ;;              `(message " %s: %S=%S" ,label ,form value)
 ;;           `(message " %S=%S" ,form value))))

(defun gpb-blank-line-p ()
  "Return non-nil if and only if current line is blank."
  (save-excursion
    (beginning-of-line)
    (looking-at "\\s-*$")))

(defun gpb-comment-line-p ()
  "Return non-nil if and only if current line has only a comment."
  (save-excursion
    (end-of-line)
    (when (eq 'comment (syntax-ppss-context (syntax-ppss)))
      (back-to-indentation)
      (looking-at (rx (or (syntax comment-start) line-end))))))

(defun gpb-in-comment-p ()
  (let ((orig (point)))
    (save-excursion
      (beginning-of-defun)
      (nth 4 (parse-partial-sexp (point) orig)))))


(defun gpb-util:add-global-compilation-errors (list)
  (dolist (x list)
    (add-to-list 'compilation-error-regexp-alist (car x))
    (setq compilation-error-regexp-alist-alist
          (cons x
                (assq-delete-all (car x)
                                 compilation-error-regexp-alist-alist)))))


(defun gpb-util-debug-message (&rest args)
  (error "obsolete")
  (let (deactivate-mark)
    (when gpb-debug-flag
      (with-current-buffer (get-buffer-create "*GPB-Debug*")
        (save-excursion
          (goto-char (point-max))
          (insert (apply 'format args) "\n"))))))

 ;; (make-obsolete 'log-message 'gpb-log-message)

 ;; (defun gpb-util-show-form (form)
 ;;   (gpb-util-debug-message "%s: %s" form (eval form)))

;; Little macro from http://www.ece.cmu.edu/~ryanjohn/linux-hacks.html
(defmacro gpb-bind-args (function &rest args)
  "Bind arguments to function to get a parameter free function."
  (list 'lambda nil (list 'interactive nil) (cons function args)))

(defmacro gpb-bind-command-arguments (function &rest args)
  "Bind arguments to function to get a parameter free function."
  `(lambda () (interactive) (apply ,function ,args)))

(defun gpb-bury-buffer-on-process-exit (proc event)
  "This is a process sentinel that buries the buffer on process exit."
  (when (or (string-equal event "finished\n")
            (string-match "exited abnormally.*\n" event))
    (let ((proc-buf (process-buffer proc)))
      (other-window -1)
      ;;(delete-windows-on proc-buf)
      (message "Burying buffer %s." proc event proc-buf)
      (bury-buffer proc-buf)
      (replace-buffer-in-windows proc-buf))))

(defun gpb-compile (cmd &rest args)
  "Extended `compile' command.

Keyword arguments are:
:comint               use comint flag in `compile' command
:buffer-name          compilation buffer name
:jump-to-first-error  value for `compilation-auto-jump-to-first-error'
:error-alist          value for `compilation-error-regexp-alist'"
  ;; (setq gpb-compile--args nil)
  (let ((error-alist :use-global)
        (keywords   ;; trimmed down from compile.el
                    `(("^Compilation \\(finished\\).*"
                     (0 '(face nil compilation-message nil
                               help-echo nil mouse-face nil) t)
                     (1 compilation-info-face))
                    (,(concat "^Compilation \\(exited abnormally\\|interrupt\\|"
                              "killed\\|terminated\\|segmentation fault\\)"
                              "\\(?:.*with code \\([0-9]+\\)\\)?.*")
                     (0 '(face nil compilation-message nil help-echo nil
                               mouse-face nil) t)
                     (1 compilation-error-face)
                     (2 compilation-error-face nil t))))
        comint buf-name jump-to-error scroll-output)
    (while args
      (let ((key (pop args)) (value (pop args)))
        (case key
          (:comint (setq comint t))
          (:buffer-name (setq buf-name value))
          (:jump-to-first-error (setq jump-to-error value))
          (:error-alist (setq error-alist value))
          ;; (:hide-on-success (setq jump-to-error value))
          (t (error "gpb-compile: bad arg: %S %S" key value)))))

    (let ((compilation-auto-jump-to-first-error jump-to-error)
          (compilation-error-regexp-alist (case error-alist
                                            (:use-global
                                             compilation-error-regexp-alist)
                                            (t
                                             error-alist)))
          (compilation-mode-font-lock-keywords keywords)
          compile-buf)
      (setq-default compilation-directory default-directory)
      (setq compile-buf (compilation-start cmd comint
                                           (if buf-name `(lambda (arg)
                                                           ,buf-name))))
      (with-current-buffer compile-buf
        (set (make-local-variable 'compilation-auto-jump-to-first-error)
              jump-to-error)
        (set (make-local-variable 'compilation-mode-font-lock-keywords) keywords)
        (unless (eq error-alist :use-global)
          (set (make-local-variable 'compilation-error-regexp-alist)
                error-alist))))))


(defun gpb-kill-buffer-on-process-exit (proc event)
  "This is a process sentinel that kills the buffer on process exit."
  (when (or (string-equal event "finished\n")
            (string-match "exited abnormally.*\n" event))
    (let ((proc-buf (process-buffer proc)))
      (other-window -1)
      ;;(delete-windows-on proc-buf)
      (message "Killing buffer %s." proc event proc-buf)
      (kill-buffer proc-buf))))

(defun gpb-util-remove-backspaces (string)
  "Remove backspaces from a string.
For each backspace, removes the backspace and the character
immediately preceeding it."
  (gpb-util-replace-string (gpb-util-reduce-string string ".\b" "")
                           "\b" ""))

(defun gpb-util-starts-with (string prefix)
  "Uses case-fold-search"
  (and (<= (length prefix) (length string))
       (save-match-data (string-match (concat "\\`" (regexp-quote prefix))
                                      string))
       t))

(defun gpb-util-ends-with (string postfix)
  "Uses case-fold-search"
  (and (<= (length postfix) (length string))
       (save-match-data
         (string-match (concat "\\(.\\|\n\\)*"
                               (regexp-quote postfix)
                               "\\'")
                       string))
       t))

(defun gpb-util-center-string (string width &optional truncate-left)
  "Center STRING in a string of width WIDTH"
  (assert (> width 3))
  (let* ((length (length string)) left-margin right-margin)
    (if (<= length width)
        ;; Center it
        (progn
          (setq left-margin (/ (- width length) 2)
                right-margin (- width (+ left-margin length)))
          (concat (make-string left-margin ? )
                  string
                  (make-string right-margin ? )))
      (if truncate-left
          ;; trim off left side
          (concat "..." (substring string
                                   (+ (- length width) 3)
                                   length))
        ;; trim off right side
        (concat (substring string 0 (- width 3)) "...")))))

(defun gpb-util-last-string-match (regex string)
  (when (string= regex ".*")
    (error "gpb-util-last-string-match regex=%S" regex))
  (when string
    (let ((last-match-start nil) match-start)
      (when (setq match-start
                  (string-match regex string))
        (while match-start
          (setq last-match-start match-start
                match-start (string-match regex string
                                          (1+ match-start))))
        (string-match regex string last-match-start)))))

(defun gpb-util-make-variable-toggler (var name)
  "Returns an interactive function which toggles a variable."
  `(lambda ()
     (interactive)
     (setq ,var (not ,var))
     (if ,var
         (message "%s: ON" ,name)
       (message "%s: OFF" ,name))))

(defun gpb-util-replace-string (string find replace)
  "Replace each occurance of FIND in STRING with REPLACE."
  (let ((result nil))
    (while (> (length string) 0)
      (if (equal find
		 (substring string 0 (min (length find) (length string))))
	  (progn
	    (push replace result)
	    (setq string (substring string (length find) (length string))))
	;; else
	(push (substring string 0 1) result)
	(setq string (substring string 1 (length string)))))
    (apply 'concat (nreverse result))))

(defun gpb-util-reduce-string (string regex replacement)
  "Replacing `regex' with `replacement' repeatedly until string
  no longer changes."
  ;;  We do the replacements one at a time by matching to the end of
  ;;  the string and replacing the subexpression.
  (setq regex (concat "\\(" regex "\\)\\(.\\|\n\\)*\\'"))
  (let ((new-string (replace-regexp-in-string regex replacement
                                              string nil nil 1)))
    (while (not (equal string new-string))
      (setq string new-string
            new-string (replace-regexp-in-string regex replacement
                                                 string nil nil 1)))
    string))

(defun gpb-util-expand-tabs (string)
  "Expand tabs in string to tab-width spaces."
  (gpb-string-replace
   string "\t" (make-string tab-width ? )))

(defun gpb-util-remove-items (list predicate)
  "Remove all items from list that match predicate."
  (let ((new-list nil) item)
    (while list
      (setq item (car list)
            list (cdr list))
      (unless (funcall predicate item)
        (push item new-list)))
    (nreverse new-list)))


(defun gpb-util-get-next-item (item list &optional offset)
  "Get the item offset items past item in list.
  Offset defaults to 1.  Returns nil when the item is not found.
  The search cycles around the list."
  (setq offset (or offset 1))
  (let ((pos (position item list :test 'equal)))
    (when pos (progn
                (setq pos (mod (+ pos offset) (length list)))
                (elt list pos)))))

(defun gpb-util-raise-minor-mode-keymap-priority (minor-mode)
  "Move the minor mode keymap to the front of minor-mode-map-alist"
  (let ((lighter (aget minor-mode-alist minor-mode t))
        (keymap (aget minor-mode-map-alist minor-mode t)))
    ;; (setq minor-mode-alist (cons minor-mode
    ;;                              (remove minor-mode minor-mode-alist)))
    (adelete 'minor-mode-alist minor-mode)
    (adelete 'minor-mode-map-alist minor-mode)
    (if lighter
        (push (cons minor-mode lighter) minor-mode-alist)
      (push minor-mode minor-mode-alist))
    (when keymap
        (push (cons minor-mode keymap) minor-mode-map-alist))))

(defun gpb-util-join-strings (list-of-string &optional separator)
  "Join a list of strings"
  (let ((result "")
        (sep ""))
    (setq separator (or separator ""))
    (while list-of-string
      (setq result (concat result sep (car list-of-string))
            list-of-string (cdr list-of-string)
            sep separator))
    result))

(defun gpb-util-truncate-string (text &optional where length repl)
  "Truncate a string replacing text with REPL

LENGTH defaults to 100
WHERE is 'left 'right or 'center.  Default is 'right
REPL defaults to \"...\""
  (setq length (or length 100)
        where (or where 'right)
        repl (or repl "...")
        length (- length (length repl)))
  (cond
   ((<= (length text) length)
    text)
   ((eq where 'right)
    (concat (substring text 0 length) repl))
   ((eq where 'center)
    (let* ((front-offset (/ length 2))
           (back-offset (- length front-offset)))
      ;; (length (substring text 0 front))
      ;; (length (substring text (- back))
      (concat (substring text 0 front-offset)
              repl
              (substring text (- back-offset)))))
   ((eq where 'left)
    (concat (apply 'propertize (copy-sequence repl) (text-properties-at 0 text))
            (substring text (- length))))
   (t
    (error "Bad argument"))))

(defun gpb-util-split-string (text sep-regex &optional max-splits
                                   omit-blank-entries)
  "Split TEXT at the regular expression SEP-REGEX"
  (let ((result nil) first)
    (while (and (or (null max-splits) (> max-splits 0))
                (string-match (concat
                               "\\(\\(?:.\\|\n\\)*?\\)" ; nongreedy anything
                               sep-regex
                               "\\(\\(?:.\\|\n\\)*\\)") ; greedy anything
                              text))
      (setq first (match-string 1 text)
            text (match-string 2 text))
      (unless (and omit-blank-entries (string-equal first ""))
        (setq result (nconc result (list first))))
      (unless (null max-splits)
        (setq max-splits (1- max-splits))))
    (unless (and omit-blank-entries (string-equal text ""))
      (setq result (append result (list text))))
    result))

(defun gpb-util-strip-string (s)
  (gpb-util-reduce-string
   (gpb-util-reduce-string s "^[ \t\n]" "")  "[ \t\n]$" ""))


(defun gpb-util-list-to-english (list)
  (cond
   ((= (length list) 0) "")
   ((= (length list) 1) (elt list 0))
   ((> (length list) 1)
    (let ((result (car list)))
      (setq list (cdr list))
      (while list
        (if (cdr list)
            (setq result (gpb-util-join-strings
                          (list result ", " (car list))))
          (setq result (gpb-util-join-strings
                        (list result ", and " (car list)))))
        (setq list (cdr list)))
      result))))

(defmacro gpb-util-cycle-list-in-place (list)
  "Cycle the list in place.  This is a macro that changes the
value stored in the symbol that is passed in."
  `(let ((second (cdr ,list)))
    (setcdr ,list nil)
    (setcdr (last second) ,list)
    (setq ,list second)))

(defun gpb-util-is-longer-p (x y)
  (> (length x) (length y)))

(defun gpb-util-matches-end (string end)
  (let ((start (- (length string) (length end))))
    (and (>= start 0)
         (equal (substring string start) end))))

(defun gpb-util-find-last (regex text &optional start)
  (let ((start (or start 0))
        (prev nil))
    (while (string-match regex text start)
      (setq prev start
            start (1+ (match-beginning 0))))
    (when prev
      (string-match regex text prev))))

(defun gpb-util-ensure-newline (string)
  "Ensure string end with a newline."
  (assert (stringp string))
  (if (or (equal (length string) 0)
          (not (equal (elt string (1- (length string))) ?\n)))
      (concat string "\n")
    string))

(defun gpb-util-insert-item (item list pos)
  (assert (and (<= pos (length list))
              (>= pos 0)))
  (setq list (copy-sequence list))
  (if (eq pos 0)
      (if (null list)
          (setq list (list item))
        (progn
          (setcdr list (cons (car list) (cdr list)))
          (setcar list item)))
    (let ((i 1)
          (before list)
          (after (cdr list)))
      (while (< i pos)
        (setq before after
              after (cdr before)
              i (1+ i)))
      (setcdr before (cons item after))))
  list)

(defun gpb-util-push-end (x place)
  "Append X to the list stored in the symbol place"
  (set place (append (symbol-value place) (list x))))

(defun gpb-util-append-item (item list)
  "Append item to the list"
  (append list (list item)))

(defun gpb-util-make-emacs-completer (get-prefix get-completions)
  "Produce an emacs completer using GET-PREFIX and GET-COMPLETIONS.

GET-PREFIX is called with a string and returns the part which
should be completed.  This is generally the part of the string
after the last dot in programming languages.  GET-COMPLETIONS is
then called in original buffer and should return a list of
completions.  This function returns a function which conforms to
the interface defined in `(elisp)Programmed Completion'.  The
returned function may be passed to `completing-read' as the
COLLECTION argument.
"
  ;; This is basically a closure.  Maybe this should be a macro?
  (lexical-let ((get-prefix get-prefix)
                (get-completions get-completions))
    (lambda (string pred action)
      (logval '(list string pred action) 'gpb-pyns--make-emacs-completer t)
      (let* ((prefix (funcall get-prefix string))
             (gpb-log-enabled t)
             (gpb-log-label 'gpb-util-make-emacs-completer)
             candidates full-candidates result)
        (gpb-log-forms 'gpb-util-make-emacs-completer 'prefix)
        (if (eq (car-safe action) 'boundaries)
            (setq result (list 'boundaries (length prefix))))
        ;; Otherwise go to calling buffer
        (with-current-buffer (let ((win (minibuffer-selected-window)))
                               (if (window-live-p win) (window-buffer win)
                                 (current-buffer)))
          (setq candidates (funcall get-completions string)
                full-candidates (mapcar (lambda (s) (concat prefix s))
                                        candidates)))
        (gpb-log-forms 'gpb-util-make-emacs-completer 'candidates 'full-candidates)
        ;; (logval 'candidates 'gpb-util-make-emacs-completer t)
        ;; (logval 'full-candidates 'gpb-util-make-emacs-completer t)
        (cond
         ;; Find common extension
         ((null action)
          (setq result (try-completion string full-candidates predicate)))
         ;; Get completions
         ((eq action t)
          (setq result candidates))
         ((eq action 'lambda)
          ;; see test-completion
          (setq result (member string full-candidates))))
        (gpb-log-forms 'gpb-util-make-emacs-completer 'result)
        ;;(logval 'result 'gpb-pyns--make-emacs-completor t)
        result))))

(defun gpb-util-indent-string (str &optional prefix)
  (setq prefix (or prefix "    "))
  (mapconcat (lambda (s) (concat prefix s))
             (gpb-util-split-string str "\n")
             "\n"))

(defun gpb-util-make-gray (str)
  (add-text-properties 0 (length str)
                       `(face ((foreground-color . "gray60"))) str)
  str)


(provide 'gpb-util)
