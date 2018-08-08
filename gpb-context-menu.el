(require 'imenu)
(require 'ffap)

(eval-when-compile 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Easy setup
;;    (require 'gpb-contect-menu)
;;    (gpb-context-menu-mode 1)
;;    (gpb-cm:add-context-menu-items ...)
;;
;;  Use the function `gpb-cm:add-context-menu-items' to register functions
;;  that generate context menu items.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar gpb-cm:context-menu-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [up-mouse-3] nil)
    (define-key map [down-mouse-3] nil)
    (define-key map [drag-mouse-3] nil)
    (define-key map [mouse-3] 'gpb-cm:show-context-menu)
    (define-key map [(shift f10)] 'gpb-cm:show-context-menu)
    (define-key map [(menu)] 'gpb-cm:show-context-menu)
    (define-key map [(apps)] 'gpb-cm:show-context-menu)
    map)
  "Keymap for `gpb-context-menu-mode'")


(defvar gpb-cm:item-generators nil
  "A list of functions used to generate the context menu.

This variable is an implementation detail.  The user should use
`gpb-cm:register-item-generator' to add and remove context menu
item generators.  This varaible and is managed like a hook
variable to allow for buffer local bindings.")


(defvar gpb-cm:show-menu-fake-map (make-sparse-keymap)
  "Keymap used to show keybindings in the menu")


(define-minor-mode gpb-context-menu-mode
  "Minor mode that adds a context sensitive menu

\\{gpb-cm:context-menu-mode-map}"
  :global t
  :keymap gpb-cm:context-menu-mode-map)


(define-minor-mode gpb-cm:show-menu-helper-mode
  "Implementation detail.

This minor mode exists so that we may install a fake keymap to
fool Emacs into shows the correct keybinding hints in the context
menu."
  :lighter " Menu"
  :keymap gpb-cm:show-menu-fake-map)


(defun gpb-cm:add-context-menu-items (func &optional local remove)
  "Register FUNC to add items to the context menu"
  (cond
   ((and local remove)
    (setq gpb-cm:item-generators (delq func gpb-cm:item-generators)))
   (remove
    (setq-default gpb-cm:item-generators
                  (delq func (default-value 'gpb-cm:item-generators))))
   (local
    (unless (local-variable-p 'gpb-cm:item-generators)
      (set (make-local-variable 'gpb-cm:item-generators) '(t)))
    (add-to-list 'gpb-cm:item-generators func))
   ((not (member func (default-value 'gpb-cm:item-generators)))
    (setq-default gpb-cm:item-generators
                  (cons func (default-value 'gpb-cm:item-generators))))))


(defun gpb-cm:format-string (string width &optional center)
  (concat
   (cond
    ;; Center if necessary
    ((and center (< (length string) width))
     (concat (make-string (/ (- width (length string)) 2) ?\ ) string))
    ;;
    ((> (length string) width)
     (concat "..." (substring string (- (length string) (- width 3)))))
    (t
     string))))


(defun gpb-cm:generate-ffap-menu-items ()
  "Generate file related context menu items."
  (let ((file/url (ffap-file-at-point)))
    (when (gpb-util-starts-with file/url "file://")
      (setq file/url (substring file/url 7) file/url nil))
    (when file/url
      ;; The list of items
      `(separator
        ,(file-name-nondirectory file/url)
        (,(format "Edit %s" (file-name-nondirectory file/url))
         (find-file-other-window ,file/url) :key-binding gpb-ffap)
        ,(unless (member (file-name-extension file/url)
                         '("el" "txt" "py"))
           `(,(format "Open %s" (file-name-nondirectory file/url))
             (shell-command ,(format "xdg-open \"%s\"" file/url) "*xdg-open*")))
        separator))))


(defun gpb-cm:generate-file-items ()
  "Generate file related context menu items."
  (let ((filename (or (ffap-file-at-point)
                      (ffap-url-at-point)
                      (ffap-string-at-point)))
        url)
    (let ((thing-at-point-short-url-regexp nil))
      (ignore-errors
        (setq url (thing-at-point-url-at-point))))
    (when (gpb-util-starts-with url "file://")
      (setq filename (substring url 7) url nil))
    ;; The list of items
    `(separator
      ,@(when (and filename
                   (not (string-equal filename ""))
                   (not (member major-mode '(dired-mode Buffer-menu-mode)))
                   ;; The following can throw tramp-related errors
                   (condition-case exception
                       (file-exists-p filename)
                     (error nil)))
          `(,(file-name-nondirectory filename)
            ("Edit" `(find-file-other-window ,filename))
            ,(unless (member (file-name-extension filename) '("el" "txt" "py"))
               `("Open" (gpb-open-file ,filename)))))
      ,(when url
         (,(format "Open %s" (file-name-nondirectory url)) (browse-url ,url)))
      separator)))


(defun gpb-cm:popup-menu (item-generator-symbol)
  "Make a menu from a list of item generators.

The list of functions called to generate a context menu.

ITEM-GENERATORS should be a list of functions.  Each function is
called with the point closest to the position where the menu
click happened and should return a list of items.  An item is
either 'separator, nil (ignored for convenience), a string, or a
list containing entries of the form: (STRING FORM PLIST)

A string is treated as a label/header in the menu.  A list is
treated as a menu entry.  In this case FORM can be a symbol
associated with an interactive command or a lisp form that will
be wrapped in a lambda function with an interactive
specification.  The PLIST can contian the following key/value
pairs:

  :centered    VALUE
  :disabled    VALUE
  :key-binding INTERACTIVE-FUNCTION

where VALUE should nil or t and INTERACTIVE-FUNCTION is a symbol
with an interactive function definition.
"
  (let ((menu (make-sparse-keymap))
        (separator-count 0)
        (function-item-count 0)
        click-x-y click-window click-point menu-pos items menu-width)
    (unwind-protect
        (progn
          (cond
           ;; Get the click position
           ((mouse-event-p last-command-event)
            (setq click-point (posn-point (elt last-command-event 1))
                  click-x-y (posn-x-y (elt last-command-event 1))
                  click-window (posn-window (elt last-command-event 1))
                  click-x (or (car click-x-y) 0)
                  click-y (or (cdr click-x-y) 0))
            (select-window click-window))
           (t
            (setq click-point (point)
                  click-x-y (posn-x-y (posn-at-point))
                  click-x (or (car click-x-y) 0)
                  click-y (or (cdr click-x-y) 0)
                  click-window (selected-window))))

          ;; Move the menu below the current line so that the current line
          ;; is visible above the context menu.
          (setq click-y (+ click-y (frame-char-height)))
          (gpb-log-forms 'gpb-cm:popup-menu 'click-point)
          (setq menu-pos (list (list click-x click-y) click-window))

          ;; Move the point and disable the region unless the click occured
          ;; inside the current region.
          (when (region-active-p)
            (unless (and (<= (region-beginning) click-point)
                         (<= click-point (region-end)))
              ;; The following let binding is a work around.
              ;; `deactivate-mark' mark calls `x-set-selection' and this
              ;; somehow breaks `popup-menu' below.  I have no idea why.
              (let (select-active-regions) (deactivate-mark))
              (goto-char click-point)))

          ;; Call item generators with point and region properly set.
          (setq items (apply 'append
                             (mapcar
                              (lambda (f)
                                (if (eq f t)
                                    ;; Insert all global menu items
                                    (apply 'append
                                           (mapcar (lambda (f)
                                                     (funcall f))
                                                   (default-value
                                                     item-generator-symbol)))
                                  (funcall f)))
                              (symbol-value item-generator-symbol))))

          ;; Remove nils as a convenience
          (setq items (delq nil items))

          ;; Remove any initial separators.
          (while (eq (car items) 'separator)
            (setq items (cdr items)))

          ;; Remove any trailing separators
          (setq items (nreverse items))
          (while (eq (car items) 'separator)
            (setq items (cdr items)))
          (setq items (nreverse items))

          ;; Remove double separators
          (let ((l items))
            (while l
              (if (and (eq (car l) 'separator)
                       (eq (cadr l) 'separator))
                  (setcdr l (cddr l))
                (setq l (cdr l)))))

          ;; Clear fake keybindings
          ;; Dont do this: (setq gpb-cm:show-menu-fake-map make-sparse-keymap))
          (setcdr gpb-cm:show-menu-fake-map nil)

          ;; Compute menu width
          (gpb-log-forms 'gpb-cm:popup-menu 'items)
          (setq menu-width
                (apply 'max (cons 20 (mapcar
                                      (lambda (s)
                                        (or (and (eq s 'separator) 0)
                                            (and (stringp s) (+ 6 (length s)))
                                            (and (stringp (car s))
                                                 (+ 6 (length (car s))))))
                                      items))))

          ;; Construct the menu keymap
          (dolist (item items)
            (cond
             ;; Separator item
             ((eq item 'separator)
              (easy-menu-add-item menu nil
                                  (vector (format "gpb-cm:menu-separator-%d"
                                                  (incf separator-count))
                                          nil :label "--")))

             ;; A label with no key binding
             ((stringp item)
              (easy-menu-add-item menu nil `[,(gpb-cm:format-string
                                               item menu-width t)
                                             nil nil]))
             ;; Normal item
             (t
              (let* ((string (car item))
                     (form (cadr item))
                     (interactive-function (if (commandp form) form
                                             `(lambda () (interactive) ,form)))
                     (func-symbol interactive-function)
                     (plist (cddr item))
                     disabled)
                (unless (symbolp func-symbol)
                  (let ((symbol (intern (format "gpb-cm:menu-function-symbol-%s"
                                                (incf function-item-count)))))
                    (fset symbol interactive-function)
                    (setq func-symbol symbol)))
                ;; Handle keyword arguments
                (while plist
                  (let ((arg (pop plist))
                        (value (pop plist)))
                    (cond
                     ((eq arg :disabled) (setq disabled t))
                     ;; Show the binding for another function
                     ((eq arg :key-binding)
                      (let ((key (or
                                  (where-is-internal value nil 'first-only)
                                  (where-is-internal value (current-global-map)
                                                     'first-only))))
                        (when key (define-key gpb-cm:show-menu-fake-map key
                                    func-symbol))))
                     (t
                      (error "Bad keyword argument: %S %S" arg value)))))

                ;; Add menu item
                (easy-menu-add-item
                 menu nil (vector (gpb-cm:format-string string menu-width)
                                  func-symbol
                                  (not disabled)))))))

          ;; Install the fake key map so that emacs shows the
          ;; keybindings that we want.
          (gpb-util-raise-minor-mode-keymap-priority
           'gpb-cm:show-menu-helper-mode)
          (gpb-cm:show-menu-helper-mode 1)
          (force-mode-line-update)
          (redisplay t)
          ;; Show menu
          (gpb-log-forms 'gpb-cm:popup-menu 'menu
                         'menu-pos 'mark-active)
          (popup-menu menu menu-pos))

      ;; The following forms are always evaluated by unwind-protect
      (gpb-cm:show-menu-helper-mode -1))))


(defun gpb-cm:show-context-menu ()
  "Show the context menu.

  Just a thin wrapper around gpb-cm:popup-menu.
  "
  (interactive)
  (gpb-cm:popup-menu 'gpb-cm:item-generators))


(provide 'gpb-context-menu)
