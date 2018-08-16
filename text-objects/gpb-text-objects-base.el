;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  VIM-like text objects
;;
;;  This file is self-contained and provides the underlying API for a text
;;  object system.  The other files in the project extend and build upon
;;  the API provided by this file.
;;
;;  TODO: Add macros to imenu list.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'repeat)
(require 'thingatpt)

(defvar gpb-tobj--enable-warnings t "Enable warning messages.")

;; Add syntax highlighting for macros
(font-lock-add-keywords 'emacs-lisp-mode
 '(;; ("(\\(gpb-tobj--declare-keymap\\)[ \t]+\\(\\(\\sw\\|\\s_\\)+\\)"
   ;;  (1 font-lock-keyword-face)
   ;;  (2 font-lock-variable-name-face))
   ("(\\(gpb-tobj--define-command\\)[ \t]+\\(\\(\\sw\\|\\s_\\)+\\)"
    (1 font-lock-keyword-face)
    (2 font-lock-function-name-face))))


(defvar gpb-tobj--text-object-alist nil
  "This alist holds entries of the form (SYMBOL . PLIST) where
  SYMBOL is a symbol that is associated with text object symbol
  and the properties in PLIST hold contain all the relevant
  information about the given text object.  Use
  `gpb-tobj--define-text-object' to add entries to this list.")

(defvar gpb-tobj--current-text-object nil)
(defvar gpb-tobj--current-text-object-modifiers nil)

(defvar gpb-tobj--global-keymap-alist nil)
(defvar gpb-tobj--local-keymap-alist nil)
(make-variable-buffer-local 'gpb-tobj--local-keymap-alist)

(defvar gpb-tobj--default-keymap nil)

(defvar gpb-tobj--base-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [t] 'gpb-tobj--bad-text-object)
    (define-key map [(escape)] 'gpb-tobj--cancel-command)
    (define-key map [(control g)] 'gpb-tobj--cancel-command)
    (define-key map [(control \])] 'gpb-tobj--cancel-command)
    map)
  "The is the parent keymap for all global text object keymaps.")

(defvar gpb-tobj--pre-read-text-object-hook nil
  "Hook called before a text object is read.

This hook is intended to provide a way to give the user visual
feed back.  For example, one could change the appearance of the
cursor to let the user know that they are now inputing a text
object.")

(defvar gpb-tobj--post-read-text-object-hook nil
  "Hook called after a text object is read.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun gpb-tobj--bad-text-object ()
  (interactive)
  (error "The key sequence %s is not bound to a text object"
                          (key-description (this-command-keys))))


(defun gpb-tobj--cancel-command ()
  (interactive)
  (error "Text object command cancelled"))


(defun gpb-tobj--debug-command (command)
  (append `(defun ,command) (cdr (symbol-function command))))


(defun gpb-tobj--debug-text-object (obj)
  (let ((func (gpb-tobj--get-text-object-property obj :find-func))
        (extend-func (gpb-tobj--get-text-object-property obj
                                                         :extend-region-func))
        (func-symbol (intern (format "%s-implementation" obj)))
        (extend-func-symbol (intern (format "%s-extend-implementation" obj))))
    (when (and func (symbolp func))
      (setq func (symbol-function func)))
    (when (and extend-func (symbolp extend-func))
      (setq extend-func (symbol-function extend-func)))
    (gpb-tobj--set-text-object-property obj :find-func func-symbol)
    (eval `(defun ,func-symbol ,@(cdr func)))
    (if (not extend-func)
        `(defun ,func-symbol ,@(cdr func))
      (gpb-tobj--set-text-object-property obj :extend-region-func
                                          extend-func-symbol)
      (eval `(defun ,extend-func-symbol ,@(cdr extend-func)))
      `((defun ,func-symbol ,@(cdr func))
        (defun ,extend-func-symbol ,@(cdr extend-func))))))


(defun gpb-tobj--declare-keymap (map-symbol doc)
  (let ((new-keymap (make-sparse-keymap doc)))
    (set-keymap-parent new-keymap gpb-tobj--base-keymap)
    (aput 'gpb-tobj--global-keymap-alist map-symbol new-keymap)
    ;; (aput 'gpb-tobj--keymap-doc-alist map-symbol doc)
    (define-key new-keymap [(f1)]
      `(lambda () (interactive) (gpb-tobj--describe-keymap ',map-symbol)))
    (define-key new-keymap [(control h)]
      `(lambda () (interactive) (gpb-tobj--describe-keymap ',map-symbol)))))


(defmacro gpb-tobj--declare-keymap-transition (from key to &optional local)
  (let ((map (gpb-tobj--get-keymap from local)))
    (define-key map key `(lambda ()
                           (interactive)
                           (text-object-keymap ,to)))
    nil))


(defun gpb-tobj--define-key (keymap-symbol key tobj &rest modifiers)
  (let* ((local (plist-get modifiers :local))
         (keymap (gpb-tobj--get-keymap keymap-symbol local)))
    ;; The following lambda expression is never evaluated.  See
    ;; `gpb-tobj--read-text-object' for the details.
    (define-key keymap key `(lambda ()
                              (interactive)
                              (text-object ,symbol ,@modifiers)))))


(defmacro gpb-tobj--define-command (symbol args doc &rest body)
  "Define a command that acts on a text object.

ARGS should be (obj beg end) or (obj beg end &rest kwargs) where BEG END
delimit the region that BODY should act upon (so
`gpb-tobj--define-command' looks like a `defun').

Keyword arguments are specified before BODY.  You don't need to
quote symbols that appear in the option list because the
arguments are removed before BODY is evaluated.  Possible
arguments are:

:keymap KEYMAP
The text object keymap that should be used to read the
object for this command.  This is required.

:ignore-region {nil, t}
The command acts on the region when
`use-region-p' returns t unless this argument is set to non-nil.
Typically commands providing cursor movement want to ignore the
region.

:object-modifiers PLIST
Initial text object modifiers.  See the documentation of
`gpb-tobj--current-text-object-modifiers' for more information.

A hook named post-SYMBOL-hook is also defined and this hook is
called after the command is invoked while the ARGS above are
still let bound."
  (declare (doc-string 2)
           (indent 1))
  (let ((command-symbol symbol)
        (pre-command-hook-symbol (intern (format "pre-%s-hook" symbol)))
        (post-command-hook-symbol (intern (format "post-%s-hook" symbol)))
        (keymap gpb-tobj--default-keymap)
        ignore-region modifiers)
    (makunbound post-command-hook-symbol)

    (while (keywordp (car-safe body))
      (let ((key (pop body))
            (value (pop body)))
        (case key
          (:keymap (setq keymap value))
          (:ignore-region (setq ignore-region value))
          (:object-modifiers (setq modifiers value))
          (t (error "Undefined key word option: %s %s" key value)))))
    `(progn
       (defvar ,pre-command-hook-symbol nil
         ,(format "Hook called before `%s'" command-symbol))
       (defvar ,post-command-hook-symbol nil
         ,(format "Hook called after `%s'" command-symbol))

       (defun ,command-symbol (&optional obj pos &rest modifiers)
         ,doc
         (interactive)
         (cond
          ;; If called with a text object, use that object.  This is
          ;; essentially the base case, and the following cases all
          ;; invoke this case.  This also allows you to invoke a
          ;; command directly in Emacs LISP code without prompting the
          ;; user for a text object.
          (obj
           (assert (symbolp obj))
           (setq pos (or pos (point))
                 modifiers (nconc modifiers ',modifiers))
           (let* ((body-function (lambda ,args ,@body)))
             ;; (plist (aget gpb-tobj--text-object-alist obj))
             ;; (find-func (plist-get plist :find-func)))
             (apply 'run-hooks ,pre-command-hook-symbol)
             (multiple-value-bind (beg end)
                 (apply 'gpb-tobj--find-text-object obj pos modifiers)
               (condition-case exc
                   (apply body-function obj beg end modifiers)
                 ('wrong-number-of-arguments
                  (funcall body-function obj beg end)))))
           (apply 'run-hooks ,post-command-hook-symbol))

          ;; If the region is active, we use the region as the text
          ;; object without prompting the user.
          ,@(unless ignore-region
              `(((use-region-p) (,command-symbol 'region (point)))))

          ;; If we are repeating the command (see `repeat') then we
          ;; use the same text object again without prompting the
          ;; user.
          ((repeat-is-really-this-command)
           (apply ',command-symbol gpb-tobj--current-text-object (point)
                  gpb-tobj--current-text-object-modifiers))

          ;; Otherwise, prompt the user for a text object and any modifiers
          (t
           (gpb-tobj--read-text-object ',keymap)
           (assert gpb-tobj--current-text-object)
           (apply ',command-symbol gpb-tobj--current-text-object
                  (point) gpb-tobj--current-text-object-modifiers)
           ;; See http://stackoverflow.com/questions/7560094/two-key-shortcut-in-emacs-without-repressing-the-first-key
           ;; (let ((repeat-key last-input-event))
           ;;   (while (eq last-input-event repeat-key)
           ;;     (and (fboundp 'blink-cursor-end) (blink-cursor-end))
           ;;     (apply ',command-symbol gpb-tobj--current-text-object
           ;;            (point) gpb-tobj--current-text-object-modifiers)
           ;;     (let (this-command-keys this-command-keys-vector)
           ;;       (setq last-input-event (read-event nil nil 2))))
           ;;   (when last-input-event
           ;;     (push last-input-event unread-command-events)))
           )))

       (eval-after-load 'eldoc '(eldoc-add-command ',command-symbol)))))

;; (defmacro gpb-tobj--define-command-2 (symbol args doc &rest body)
;;   "Define a command that acts on a text object.

;; ARGS should be a list of four symbols that are bound when BODY is
;; called (just like in regular `defun').  The usual choice of
;; variable names is (OBJ COUNT BEG END).  These four values define
;; the text object that the code in BODY should act upon.

;; Keyword arguments are specified before BODY.  You never need to
;; be quote symbols in the option list because the options form is
;; never evaluated.  Possible arguments are:

;; :keymap         The text object map that should be used to read
;;                 the object for this command.  The possible values
;;                 are root, next, and previous.  This is required.

;; :ignore-region  The command acts on the region when `use-region-p'
;;                 returns t unless this argument is set to non-nil.

;; :work-backwards   If this argument is non-nil, the count is negated
;;                 before it is passed to the text object.  This is
;;                 useful for commands which work backwards.

;; A hook named post-SYMBOL-hook is also defined and this hook is
;; called after the command is invoked and the variables above are
;; still let bound."
;;   (declare (doc-string 2)
;;            (indent 1))
;;   (let ((command-symbol symbol)
;;         (post-command-hook-symbol (intern (format "post-%s-hook" symbol)))
;;         (keymap (gpb-tobj--remove-keyword-arg body :keymap t))
;;         (ignore-region (gpb-tobj--remove-keyword-arg body :ignore-region t)))
;;         (work-backwards (gpb-tobj--remove-keyword-arg body :work-backwards t)))
;;     (when (keywordp (car-safe body)) (error "Unknown option: %S" key))
;;     `(progn
;;        (makunbound post-command-hook-symbol)

;;        (defvar ,post-command-hook-symbol nil
;;          ,(format "Hook called after `%s'" command-symbol))

;;        (defun ,command-symbol (&optional obj pos count)
;;          ,doc
;;          (interactive)
;;          (let ((body-function (lambda ,args ,@body)) beg end)
;;            ;; First we determine the kind of object to act upon
;;            (cond
;;             ;; If the region is active, we use the region as the text
;;             ;; object without prompting the user.
;;             ,@(unless ignore-region
;;                 `(((use-region-p)
;;                    (setq gpb-tobj--current-text-object 'region
;;                          gpb-tobj--current-text-object-count 1))))

;;             ;; If we are repeating the command (see `repeat') then we
;;             ;; use the same text object again without prompting the
;;             ;; user.  Otherwise, prompt the user for a text object
;;             ((not (repeat-is-really-this-command))
;;              (gpb-tobj--read-text-object ',keymap)))
;;            (assert (and gpb-tobj--current-text-object
;;                         gpb-tobj--current-text-object-count))

;;            ;; Determine the bounds of the object
;;            (cond
;;             ((eq gpb-tobj--current-text-object 'region)
;;              (setq beg (region-beginning)
;;                    end (region-end)))
;;             (t
;;              (multiple-value-setq (beg end)
;;                (gpb-tobj--find-text-object-2
;;                 gpb-tobj--current-text-object
;;                 (point)
;;                 ,@(if work-backwards
;;                       '((- gpb-tobj--current-text-object-count))
;;                     '(gpb-tobj--current-text-object-count))))))

;;            ;; Act on the region
;;            (funcall body-function obj beg end)
;;            (run-hooks ',post-command-hook-symbol)))))


;; (defmacro gpb-tobj--define-text-object (symbol args doc &rest body)
;;   "Define a text object and bind it to a key sequence.

;; ARGS should just be (POS COUNT).  BODY can be preceeded by
;; keyward argument pairs.  Possible keyward arguments are as
;; follows:

;; :keymap            keymap where object is defined. Required.
;; :local             use buffer local keymap? Defaults to nil.
;; :forward-key       when the object is envoked using this command,
;;                    the count argument is not negated.
;; :backward-key      when the object is envoked using this command,
;;                    the count argument is negated.
;; "
;;   (declare (indent 1) (doc-string 2))
;;   (let (keymap plist local global-map keymap forward-key backward-key
;;         function function-args)

;;     ;; Handle keyword options
;;     (while (keywordp (car-safe body))
;;       (let ((key (pop body))
;;             (value (pop body)))
;;         (case key
;;           (:local (setq local value))
;;           (:keymap (setq keymap value))
;;           (:forward-key (setq forward-key value))
;;           (:backward-key (setq backward-key value))
;;           (t (error "Unknown option: %S" key)))))

;;     (assert (and keymap (or forward-key backward-key)))

;;     (setq keymap (gpb-tobj--get-keymap keymap local))
;;     (when (and gpb-tobj--enable-warnings
;;                (aget gpb-tobj--text-object-alist symbol))
;;       (message "Warning: text object %S is already defined." symbol))
;;     (aput 'gpb-tobj--text-object-alist symbol
;;           `(:function (lambda ,args ,@body) :doc ,doc))

;;     (when forward-key
;;       (define-key keymap forward-key
;;         `(lambda ()
;;           (interactive)
;;           (text-object ,symbol)
;;           ,@(when (and forward-key backward-key)
;;               `(keybinding ,(format "%s or %s" forward-key backward-key))))))

;;     (when backward-key
;;       (define-key keymap backward-key
;;         `(lambda ()
;;           (interactive)
;;           (text-object ,symbol)
;;           (negate-count t)
;;           ,@(when (and forward-key backward-key)
;;               '(:keybinding nil)))))

;;     `(quote ,symbol)))


(defmacro gpb-tobj--define-text-object (symbol args doc &rest body)
  "Define a text object and bind it to a key sequence.

ARGS should just be (pos &rest modifiers) and BODY should contain
code that returns a list of the form (list beg end), where beg
and end are buffer positions that deimit the current text object.

BODY may be preceded by keyword argument pairs.  The only keyword
argument that is processed by this macro is :key-binding which
should be followed by a list which is passed as arguments to
`gpb-tobj--define-key'.  The remaining keyword arguments are
associated with the text object.  See
`gpb-tobj--text-object-alist' for the details."
  (declare (indent 1) (doc-string 2))
  (assert (stringp doc))
  (when (and gpb-tobj--enable-warnings
             (aget gpb-tobj--text-object-alist symbol))
    (message "Warning: text object %S is already defined." symbol))
  (let (bindings kwargs)
    ;; Handle keyword options
    (while (keywordp (car-safe body))
      (let ((key (pop body))
            (value (pop body)))
        (case key
          (:key-binding (add-to-list 'bindings value))
          (t (setq kwargs (append (list key value) kwargs))))))
    ;; Add text object info to main alist
    (aput 'gpb-tobj--text-object-alist symbol
          `(:doc ,doc
            ,@kwargs
            :find-func (lambda ,args ,@body)))
    ;; Define any key bindings
    (dolist (b bindings)
      (let ((map-symbol (car b)) (key (cadr b)) (modifiers (cddr b)))
        (apply 'gpb-tobj--define-key map-symbol key symbol modifiers)))
    ;; Done.  We may as well return the symbol.
    `(quote ,symbol)))


(defun gpb-tobj--describe-key-binding (key binding &optional prefix)
  "Describe the key binding associated with KEY and BINDING.

This is a utility function for `gpb-tobj--describe-keymap'."
  (let ((keyseq-desc (key-description (append prefix (list key)))))
    ;; (message "keyseq-desc %S" keyseq-desc)
    (cond
     ;; Recurse through sub-keymaps
     ((keymapp binding)
      (map-keymap (lambda (next-key binding)
                    (gpb-tobj--describe-key-binding
                     next-key binding (append prefix (list key))))
                  binding))

     ;; Describe text objects
     ((and (listp binding) (assq 'text-object binding))
      (insert (format "%-14s  %s\n"
                      keyseq-desc
                      (gpb-tobj--get-text-object-property
                       (cadr (assq 'text-object binding)) :doc))))

     ;; Describe keys that switch to other tobject keymaps
     ((and (listp binding) (assq 'text-object-keymap binding))
      (insert (format "%-14s  Switch to \"%s\" text object keymap\n"
                      keyseq-desc
                      (cadr (assq 'text-object-keymap binding))))))))


(defun gpb-tobj--describe-keymap (name)
  "Describe the text objects in local and global keymaps with name NAME"
  (let* ((buf-name "*Text Object Help*")
         (win (get-buffer-window buf-name)))
    ;; (message "desc: %S" win)
    (if win
        (with-selected-window win
          (condition-case err
              (scroll-up)
            (error (set-window-start win (point-min)))))
      (let* ((help-window-select nil)
             (global-map (gpb-tobj--get-keymap name))
             (local-map (gpb-tobj--get-keymap name t)))
        (display-buffer (get-buffer-create buf-name) t)
        (with-current-buffer buf-name
          (erase-buffer)
          (insert "Text Objects (repeat help key to scroll)\n\n")
          (insert "Text objects are associated with the following key bindings:")
          (insert "\n")
          (when (cdr local-map)
            (insert "Buffer Local Objects:\n\n")
            (insert "key             object\n")
            (insert "---             -------\n\n")
            (map-keymap 'gpb-tobj--describe-key-binding local-map))
          (insert "\nGlobal Objects:\n\n")
          (insert "key             object\n")
          (insert "---             -------\n\n")
          (map-keymap 'gpb-tobj--describe-key-binding global-map))))))


(defun gpb-tobj--find-text-object (obj pos &rest modifiers)
  "Find the OBJ at POS.

OBJ is a symbol that is associated with text object (see
`gpb-tobj--define-text-object' and `gpb-tobj--text-object-alist')
and POS is a buffer position.  MODIFIERS is a plist of properties
that modify the text object.  These modifiers are passed on
the :find-func which is associated with the text obj OBJ."
  (if (eq obj 'region)
      (list (region-beginning) (region-end))
    (let* ((find-func (gpb-tobj--get-text-object-property obj :find-func))
           (msg-func (gpb-tobj--get-text-object-property
                      obj :search-msg-func
                      (lambda (obj &rest modifiers) (format "%S" obj))))
           ;; Don't log the temp message
           (message-log-max nil))
      (with-temp-message
          (format "Searching for %s..." (apply msg-func obj modifiers))
        (apply find-func pos modifiers)))))
;; (message "Found %s" (apply msg-func obj modifiers)


(defun gpb-tobj--get-keymap (name &optional local)
  "Get the keymap which corresponds to NAME."
  (let ((global-map (aget gpb-tobj--global-keymap-alist name))
        (local-map (aget gpb-tobj--local-keymap-alist name)))
    (unless global-map (error "Bad keymap name: %s" name))
    (cond
     ((not local) global-map)
     (local-map local-map)
     (t
      (let ((new-keymap (make-sparse-keymap)))
        (aput 'gpb-tobj--local-keymap-alist name new-keymap)
        new-keymap)))))

(defun gpb-tobj--get-text-object-property (obj prop &optional default)
  "Get a property value associated with a text object.

Text object \"properties\" are keyword/property pairs that are
defined when the text object is defined.  This is different than
a text object \"modifier\".  A text object \"modifier\" is an
option that is specified when the text object is being read from
the user."
  (or (plist-get (aget gpb-tobj--text-object-alist obj) prop) default))


(defun gpb-tobj--get-text-object-modifier (keyword &optional default)
  "Get the current text object modifier.

Text object modifiers are similar to the arguments that are read
using `universal-argument' or `digit-argument'.  They are read
during the command loop in `gpb-tobj--read-text-object' and serve
to modify the text object or text object command that is being
invoked.  These modifiers are temporary and only apply to the
current command being read form the user."
  (or (plist-get gpb-tobj--current-text-object-modifiers keyword) default))


(defun gpb-tobj--global-set-key (key command)
  "Define a keybinding in all text object keymaps."
  (define-key gpb-tobj--base-keymap key command))


(defun gpb-tobj--read-text-object (map-symbol)
  "Prompt user for a text object and record the result.

This function explicitly sets `gpb-tobj--current-text-object' and
clears `gpb-tobj--current-text-object-modifiers'.  The execution of
the non text-object key bindings will often modify the plist
`gpb-tobj--current-text-object-modifiers' to store information.
For example, the number keys could be bound to functions which
store information in this plist under the key :count.  This is
very similiar to (and slightly more general than) the
`universal-argument' system in the usual Emacs command loop."
  (let* ((keymap-alist (mapcar
                        (lambda (name-keymap)
                          (let* ((name (car name-keymap))
                                 (global-map (cdr name-keymap))
                                 (local-map (gpb-tobj--get-keymap name t))
                                 (keymap (copy-keymap local-map)))
                            (set-keymap-parent keymap global-map)
                            (cons name keymap)))
                        gpb-tobj--global-keymap-alist))
         (overriding-terminal-local-map (aget keymap-alist map-symbol))
         (current-message (current-message))
         key-sequence last-command-event command)
    (gpb-log-form 'gpb-tobj--read-text-object 'keymap-alist)
    (unless overriding-terminal-local-map
      (error "Bad map-symbol: %S" map-symbol))
    ;; Reset the modifiers
    (setq gpb-tobj--current-text-object-modifiers nil)
    (run-hooks 'gpb-tobj--pre-read-text-object-hook)
    (unwind-protect
        (catch 'text-object-selected
          (save-window-excursion
            (save-excursion
              (while t
                (setq key-sequence (read-key-sequence
                                    (and (not (minibuffer-window-active-p
                                               (selected-window)))
                                         "Enter text object (f1 for help):")
                                    nil nil nil t)
                      last-command-event (elt key-sequence
                                              (1- (length key-sequence)))
                      command (key-binding key-sequence t))
                (gpb-log-forms 'gpb-tobj--read-text-object 'command)
                (cond
                 ;; If the commmand corresponds to a new keymap, switch
                 ((and (listp command) (assq 'text-object-keymap command))
                  (let* ((new-keymap-symbol (cadr (assq 'text-object-keymap
                                                        command)))
                         (new-keymap (aget keymap-alist new-keymap-symbol)))
                    (setq overriding-terminal-local-map new-keymap
                          txt (format "%s " new-keymap-symbol))))
                 ;; If the commmand corresponds to a text object we are done
                 ((and (listp command) (assq 'text-object command))
                  (let ((obj (cadr (assq 'text-object command)))
                        (modifiers (cddr (assq 'text-object command))))
                    (setq gpb-tobj--current-text-object obj
                          gpb-tobj--current-text-object-modifiers
                          (nconc gpb-tobj--current-text-object-modifiers
                                 modifiers))
                    ;; (message "Searching for %s..." obj)
                    )
                  (throw 'text-object-selected nil))
                 ;; Otherwise, call the command
                 (t
                  (funcall command)))))))
      (message current-message)
      (run-hooks 'gpb-tobj--post-read-text-object-hook))))


(defun gpb-tobj--reset-all-keymaps ()
  "This is just for debugging purposes"
  (makunbound 'gpb-tobj--global-keymap-alist)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (boundp 'gpb-tobj--local-keymap-alist)
        (setq gpb-tobj--local-keymap-alist nil)))))


(defun gpb-tobj--set-default-keymap (keymap-symbol)
  "Set a default keymap for key bindings and commands"
  (setq gpb-tobj--default-keymap keymap-symbol))


(defun gpb-tobj--set-text-object-modifier (keyword value)
  "Update the current text object modifiers.

This command should be called by functions that are invoked
during `gpb-tobj--read-text-object'.  See
`gpb-tobj--get-text-object-modifier' for more information."
  (setq gpb-tobj--current-text-object-modifiers
        (plist-put gpb-tobj--current-text-object-modifiers keyword value))
  (gpb-log-form 'gpb-tobj--set-text-object-modifier 'gpb-tobj--current-text-object-modifiers))


(defun gpb-tobj--set-text-object-property (obj prop value)
  "Get a property value associated with a text object.

See `gpb-tobj--get-text-object-property' for more information."
  (plist-put (aget gpb-tobj--text-object-alist obj) prop value))


;; (defun gpb-tobj--switch-to-map (name)
;;   "Change the current active text object map.

;; This command should only be called from the event loop inside
;; `gpb-tobj--read/mark-tobj'."
;;   (assert (and (boundp 'inside-read/mark-tobj) inside-read/mark-tobj))
;;   (let  ((global-map (gpb-tobj--get-keymap name))
;;          (local-map (gpb-tobj--get-keymap name t)))
;;     (assert (keymapp global-map))
;;     (assert (keymapp local-map))
;;     (let ((keymap (copy-keymap local-map)))
;;       (set-keymap-parent keymap global-map)
;;       (setq overriding-terminal-local-map keymap))))


;; (defun gpb-tobj--text-object-p (symbol)
;;   "Is SYMBOL a text object?

;; A text object is a symbol which has been defined by
;; `gpb-tobj--define-text-object'"
;;   (and (symbolp symbol)
;;        (boundp symbol)
;;        (fboundp symbol)
;;        (plist-get (symbol-value symbol) :is-text-object)))



(provide 'gpb-text-objects-base)
