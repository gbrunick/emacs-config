;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Support for simple settings buffers.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Local Variables:
;;
;;  In parent buffer:
;;    gpb-settings:property-sheet-buffer
;;
;;  In property sheet buffer:
;;    gpb-settings:layout-function
;;    gpb-settings:parent-buffer
;;

;; (defvar gpb-settings:indent 0
;;   "Current indentation for widget insertion.")

;; (defun gpb-settings:indent () (make-string gpb-settings:indent ?\ ))

(require 'widget)
(require 'wid-edit)

(defvar gpb-settings:keymap
  (let ((keymap (make-sparse-keymap)))
    (set-keymap-parent keymap widget-keymap)
    (define-key keymap " " 'widget-button-press)
    (define-key keymap [(up)] 'widget-backward)
    (define-key keymap [(down)] 'widget-forward)
    (define-key keymap "k" 'widget-backward)
    (define-key keymap "j" 'widget-forward)
    (define-key keymap [(left)] 'widget-backward)
    (define-key keymap [(right)] 'widget-forward)
    ;; We want the non-interactive version of kill-buffer
    (define-key keymap "q" 'gpb-settings:close-property-sheet)
    (define-key keymap [(escape)] 'gpb-settings:close-property-sheet)
    (define-key keymap [(control g)] 'gpb-settings:close-property-sheet)
    keymap)
  "Keymap for the property sheet buffer")


(define-derived-mode property-list-mode nil "Settings"
  "A major mode for editing buffer local variables")


(defun gpb-settings:make-settings-buffer (layout-function &optional buffer)
  "Create a settings buffer for BUFFER or current buffer.

LAYOUT-FUNCTION should call the functions gpb-settings:insert-* to
construct the settings buffer."
  (let ((parent-buffer (or buffer (current-buffer)))
        property-buffer)
    (with-current-buffer parent-buffer
      (setq property-buffer
            (or (and (boundp 'gpb-settings:property-sheet-buffer)
                     (buffer-live-p gpb-settings:property-sheet-buffer)
                     gpb-settings:property-sheet-buffer)
                (generate-new-buffer "*Settings*")))
      ;;(logval 'property-buffer)
      (make-local-variable 'gpb-settings:property-sheet-buffer)
      (setq gpb-settings:property-sheet-buffer property-buffer)
      (add-hook 'kill-buffer-hook 'gpb-settings:close-property-sheet nil t))
    (with-current-buffer property-buffer
      (property-list-mode)
      (make-local-variable 'gpb-settings:parent-buffer)
      (setq gpb-settings:parent-buffer parent-buffer)
      (make-local-variable 'gpb-settings:layout-function)
      (setq gpb-settings:layout-function layout-function)
      ;; We create a copy of the keymap so that we can install buffer
      ;; specific bindings without changing gpb-settings:keymap
      (let ((keymap (make-sparse-keymap)))
        (set-keymap-parent keymap gpb-settings:keymap)
        (use-local-map keymap))
      ;; Call this after setting the keymap as the layout may modify
      ;; the keymap
      (gpb-settings:update-layout)
      (widget-setup)
      (beginning-of-buffer)
      (widget-forward 1)
      )
    (switch-to-buffer property-buffer)))


(defun gpb-settings:close-property-sheet (&optional buffer)
  "Close the property sheet.
May be called with the property sheet buffer or the parent buffer.
Safe to call if there is no property buffer."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (cond
     ;; If we are in a parent directory
     ((and (boundp 'gpb-settings:property-sheet-buffer)
           (buffer-live-p gpb-settings:property-sheet-buffer))
      (message "Closing %s..." gpb-settings:property-sheet-buffer)
      (bury-buffer gpb-settings:property-sheet-buffer)
      (kill-buffer gpb-settings:property-sheet-buffer))
     ;; If we are in the property buffer
     ((and (boundp 'gpb-settings:parent-buffer)
           (buffer-live-p gpb-settings:parent-buffer))
      (let ((buf gpb-settings:parent-buffer))
        (kill-buffer)
        (switch-to-buffer buf))))))


(defun gpb-settings:update-layout ()
  "Update the layout in the property sheet buffer.

  This function is safe to call if the properties buffer has been
  deleted."
  ;;(message "Update layout %s" (current-buffer))
  (let ((buffer (current-buffer)))
    ;; If we are in the parent buffer, move to property buffer
    (when (and (boundp 'gpb-settings:property-sheet-buffer)
               (buffer-live-p gpb-settings:property-sheet-buffer))
      (setq buffer gpb-settings:property-sheet-buffer))
    (with-current-buffer buffer
      ;; If the property buffer has been closed, the following when
      ;; clause will fail.
      (when (boundp 'gpb-settings:parent-buffer)
        (let ((inhibit-read-only t)
              (gpb-settings:in-layout-function t)
              (widget-buffer (current-buffer))
              (layout-function gpb-settings:layout-function)
              (col (current-column))
              (row (line-number-at-pos)))
          (erase-buffer)
          (with-current-buffer gpb-settings:parent-buffer
            (gpb-settings:insert-text (format "Settings for buffer: %s\n"
                                        (current-buffer)))
            (funcall layout-function)
            (gpb-settings:insert-text "\n\n\nEsc or q to exit.\n"))
          (goto-line row)
          (move-to-column (max 1 (1- col)))
          ;;(widget-forward -1)
          (widget-forward 1)
          )))))

(defun gpb-settings:write-local-variables (comment &rest symbol-list)
  "Should be called from the parent buffer.  Safe to call if
  there is no property buffer."
  (save-excursion
    (goto-char (max-char))
    (ignore-errors
      (re-search-backward (format (concat "%s\s+Local Variables:\s*\n"
                                          "\\(.*\n\\)\\{1,10\\}+"
                                          "%s\s+End:[ \n]*\\'" )
                                  (regexp-quote comment)
                                  (regexp-quote comment))
                          (max 0 (- (point-max) 5000)))
      (replace-match ""))
    (insert "\n")
    (delete-blank-lines)
    (insert (format "\n%s Local Variables:\n" comment))
    (dolist (symbol symbol-list)
      (insert (format "%s  %s: %S\n" comment
                      (symbol-name symbol) (symbol-value symbol))))
    (insert (format "%s End:\n\n" comment))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Layout functions.
;;
;;  These functions should only be called inside a function that is
;;  passed to gpb-settings:make-settings-buffer.  In particular, they assume
;;  that gpb-settings:parent-buffer has been set correctly.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defmacro gpb-settings:with-indent (&rest forms)
;;   "Layout form for indentation."
;;   `(let ((gpb-settings:indent (+ gpb-settings:indent 2)))
;;     (progn ,@forms)))

(defvar gpb-settings:in-layout-function nil)

(defun gpb-settings:insert-text (text) ;;&rest args)
  "Insert text into property sheet.

  Should only be used in a function passed to
  gpb-settings:make-settings-buffer because it must run within a let
  statement that binds widget-buffer"
  (unless gpb-settings:in-layout-function
    (error "gpb-settings:insert-text must be called inside a layout function"))
  (with-current-buffer widget-buffer
    ;;(widget-insert (apply 'format args))
    (widget-insert text)))



(defun gpb-settings:define-key (key callback)
  "Add a keybinding to property buffer. Callback is run in parent buffer."
  (unless gpb-settings:in-layout-function
    (error "gpb-settings:define-key must be called inside a layout function"))
  (with-current-buffer widget-buffer
    ;; (logval '(current-buffer) "gpb-settings:define-key")
    ;; (logval '(current-local-map) "gpb-settings:define-key")
    (local-set-key
     key `(lambda ()
            (interactive)
            (with-current-buffer gpb-settings:parent-buffer
              (funcall ',callback))
            (gpb-settings:update-layout)))))

(defun gpb-settings:insert-string-chooser (value button-text callback
                                     &optional precallback
                                     must-exist allow-clear
                                     width)
  "Create a widget that holds a string.

  A clear button resets the value to nil.

  PRECALLBACK is called with the current value of the widget.
  The result is then passed to callback as the new value of the
  widget.  See gpb-settings:insert-file-chooser for an example of this
  use.

  Should only be used in a function passed to
  gpb-settings:make-settings-buffer because it must run withing a let
  statement that binds widget-buffer"
  ;;(logval 'value "gpb-settings:insert-string-chooser")
  (setq width (or width 50)
        precallback (or precallback 'gpb-settings:string-chooser-precallback))

  (let* ((text-width (- width (+ 3 (length button-text)))))
    ;; Add room for a clear button when allow-clear
    (when allow-clear (setq text-width (- text-width 8)))
    (with-current-buffer widget-buffer
      ;; (widget-insert (concat (gpb-settings:indent)
      ;;                        (replace-regexp-in-string
      ;;                         "\n" (concat "\n" (gpb-settings:indent)) text)))
      (widget-insert
       (concat (propertize
                (if (stringp value)
                    ;; Trim length or extend length of filename
                    (if (> (length value) text-width)
                        (concat "..." (substring value (- 3 text-width)))
                      (concat value (make-string (- text-width
                                                    (length value)) ?\ )))
                  (make-string text-width ?\ ))
                'face '((background-color . "grey90"))) " "))
      (setq gpb-save-func
            `(lambda (&rest ignore)
                  ;; Call the precallback with current value and then
                  ;; send the result to the callback.
                  (with-current-buffer gpb-settings:parent-buffer
                    (funcall ',callback (funcall ',precallback ,value)))
                  (gpb-settings:update-layout)))
      (widget-create
       'push-button
       :notify gpb-save-func
       :tag button-text)
      (when allow-clear
        (widget-insert " ")
        (widget-create 'push-button
                       :notify `(lambda (&rest ignore)
                                  (with-current-buffer gpb-settings:parent-buffer
                                    (funcall ',callback nil))
                                  (gpb-settings:update-layout))
                       :tag "clear"))
      ;;(widget-insert "\n")
      )))


(defun gpb-settings:string-chooser-precallback (current-value)
  ;;(logval 'current-value "file precallback")
  (read-string "Enter new value: " nil nil current-value))


(defun gpb-settings:file-chooser-precallback (current-value)
  ;;(logval 'current-value "file precallback")
  (if (stringp current-value)
      (read-file-name "Enter filename: "
                      (file-name-directory current-value)
                      nil 'confirm nil
                      ;;(file-name-nondirectory current-value)
                      'file-regular-p)
    (read-file-name "Enter filename: " nil nil 'confirm nil 'file-regular-p)))

(defun gpb-settings:insert-file-chooser (value callback
                                   &optional must-exist allow-clear width)
  (gpb-settings:insert-string-chooser value "browse" callback
                                'gpb-settings:file-chooser-precallback
                                must-exist allow-clear width))

(defun gpb-settings:dir-chooser-precallback (current-value)
  ;;(logval 'current-value "file precallback")
  (if (stringp current-value)
      (read-file-name "Enter directory: "
                      (file-name-directory current-value)
                      nil 'confirm nil
                      'file-directory-p)
    (read-file-name "Enter directory: " nil nil 'confirm nil 'file-directory-p)))

(defun gpb-settings:insert-dir-chooser (value callback
                                   &optional must-exist allow-clear width)
  (gpb-settings:insert-string-chooser value "browse" callback
                                'gpb-settings:dir-chooser-precallback
                                must-exist allow-clear width))

(defun gpb-settings:insert-button (label callback)
  "Add a button to the property sheet.

  Should only be used in a function passed to
  gpb-settings:make-settings-buffer because it must run withing a let
  statement that binds widget-buffer"
  (with-current-buffer widget-buffer
    (widget-create 'push-button
                   :notify (gpb-settings:wrap-callback callback)
                   label)))

(defun gpb-settings:insert-checkbox (value callback)
  "Create a checkbox.  Callback gets 't or nil on changes.

Should only be used in a function passed to
`gpb-settings:make-settings-buffer' because it must run withing a
let statement that binds widget-buffer"
  (with-current-buffer widget-buffer
    ;;(widget-insert (make-string gpb-settings:indent ?\ ))
    (widget-create 'checkbox
                   :notify (gpb-settings:wrap-callback callback)
                   :value value
                   t);;'gpb-on-check
    ;;(widget-insert (format " %s\n" text))
    ))

(defun gpb-settings:insert-radiobox (value callback &optional label)
  "Create a radiobox.  Callback always gets 't

Should only be used in a function passed to
`gpb-settings:make-settings-buffer' because it must run withing a
let statement that binds widget-buffer"
  (with-current-buffer widget-buffer
    (let ((widget-push-button-prefix "(")
          (widget-push-button-suffix ")"))
      (widget-create 'push-button
                     :notify (gpb-settings:wrap-callback callback)
                     (if value "*" " "))
      (when label
        (insert " ")
        (insert label)))))


(defun gpb-settings:insert-choice (value string-value-alist callback
                                   &optional indent buffer)
  "Create a choice that changes the value of symbol.

  Should only be called from within a let statement that binds
  gpb-settings:widget-buffer

  One of the values in string-value-alist can be 'no-match
  indent is an integer (number of space)

  Warning: this seems to be very slow?
  "
  (with-current-buffer widget-buffer
    (let ((values (mapcar (lambda (tag-value) (cdr tag-value))
                          string-value-alist))
          (choice-list (mapcar (lambda (tag-value)
                                 (list 'item
                                       :tag   (car tag-value)
                                       :value (cdr tag-value)))
                               string-value-alist))
          widget)
      (setq indent (current-column)) ;; or indent "  "))
      (unless (member value values)
        (if (member 'no-match values)
            (setq value 'no-match)
          (error "gpb-settings:insert-choice: %S is not in the list %S" value values)))
      ;;(logval 'choice-list)
      (setq widget (apply 'widget-create 'radio-button-choice
                          :value value
                          :notify (gpb-settings:wrap-callback callback)
                          ;; Indent does not seem to affect the first
                          ;; radio-box, but format does.
                          :indent indent
                          ;; :format (concat indent "%v")
                          choice-list))
      ;; The following is pretty slow
      ;;(widget-value-set widget value)
      widget)))

(defun gpb-settings:wrap-callback (callback)
  "Wrap up a callback function for a widget.

  This does three things:
  1. It gets the value from the widget and passes to the callback
      function.
  2. It executes the callback function in the parent buffer, not
     the buffer where the widgets live.  We do this so that
     callbacks can easily change buffer local variables.
  3. Updates the widget buffer to reflect changes in the value of
     the local variables."
  `(lambda (widget &rest args)
     ;; I think that widget-value has to occur in the widget buffer
     (let ((value (widget-value widget)))
       ;; (logval 'value)
       ;; Now we switch to the parent buffer so that the callback can
       ;; use the buffer local variable that live there.
       (with-current-buffer gpb-settings:parent-buffer
         (condition-case exception
             (funcall ',callback value)
           ('wrong-number-of-arguments (funcall ',callback)))))
     (gpb-settings:update-layout)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;  Test case
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gpb-settings:test-case ()
  (interactive)
  (pop-to-buffer "*Test Buffer*")
  (erase-buffer)
  (insert "This is the test buffer.\n\n")
  (insert "The local variables live in this buffer.\n\n")
  (set (make-local-variable 'test-file)
       "/home/gbrunick/usr-common/src/emacs/prop-sheets.el")
  (set (make-local-variable 'test-dir)
       "/home/gbrunick/usr-common/src/emacs/")
  (set (make-local-variable 'test-check-1) t)
  (set (make-local-variable 'test-check-2) nil)
  (set (make-local-variable 'test-animal) 'cat)
  (set (make-local-variable 'test-cat) 'cat1)
  (set (make-local-variable 'test-dog) 'dog2)
  (local-set-key [(control p)] 'gpb-settings:show-settings))

(defun gpb-settings:show-settings ()
  (interactive)
  (gpb-settings:make-settings-buffer 'test-layout))

(defun test-layout ()
  (message "Entering test-layout")
  (logval '(current-buffer))
  (logval 'gpb-settings:property-sheet-buffer)
  (logval 'test-file)
  (logval 'test-dir)
  (logval 'test-check-1)
  (logval 'test-check-2)
  (logval 'test-animal)
  (logval 'test-cat)
  (logval 'test-dog)

  (gpb-settings:insert-text "\n  Some choices:\n")
  (gpb-settings:insert-text "    File: ")
  (gpb-settings:insert-file-chooser
   test-file (lambda (val)
                 (logval 'val "file callback")
                 (setq test-file val)))
  (gpb-settings:insert-text "\n     Dir: ")
  (gpb-settings:insert-dir-chooser
   test-dir (lambda (val)
              (logval 'val "dir callback")
              (setq test-dir val)) t t)
  (gpb-settings:insert-text "\n\n  Select an animal:\n    ")
  (gpb-settings:insert-choice test-animal
                        '(("Cat" . cat) ("Dog" . dog))
                        (lambda (val)
                          (message "animal: %S" val)
                          (setq test-animal val)))
  (cond
   ((eq test-animal 'cat)
    (gpb-settings:insert-text "\n  Further cat choices:\n    ")
    (gpb-settings:insert-choice test-cat
                           '(("Cat1" . cat1) ("Cat2" . cat2))
                           (lambda (val)
                             (message "cats: %S" val)
                             (setq test-cat val))))
   ((eq test-animal 'dog)
    (gpb-settings:insert-text "\n  Further dog choices:\n    ")
    (gpb-settings:insert-choice test-dog
                          '(("Dog1" . dog1) ("Dog2" . dog2) ("Dog3" . dog3))
                          (lambda (val)
                            (message "cats: %S" val)
                            (setq test-dog val)))))
  (gpb-settings:insert-text "\n  Some check boxes:\n    ")
  (gpb-settings:insert-checkbox test-check-1
                                (lambda (val) (setq test-check-1 val)))
  (gpb-settings:insert-text " Test check 1\n    ")
  (gpb-settings:insert-checkbox test-check-2
                                (lambda (val) (setq test-check-2 val)))
  (gpb-settings:insert-text " Test check 2\n\n  ")
  (gpb-settings:insert-button "close" 'gpb-settings:close-property-sheet)
  )

(defun gpb-settings:widget-example ()
  "From the widgets manual."
  (interactive)
  (switch-to-buffer "*Widget Example*")
  (kill-all-local-variables)
  (make-local-variable 'widget-example-repeat)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (let ((all (overlay-lists)))
    ;; Delete all the overlays.
    (mapcar 'delete-overlay (car all))
    (mapcar 'delete-overlay (cdr all)))
  (widget-insert "Here is some documentation.\n\nName: ")
  (widget-create 'editable-field
		 :size 13
		 "My Name")
  (widget-create 'menu-choice
		 :tag "Choose"
		 :value "This"
		 :help-echo "Choose me, please!"
		 :notify (lambda (widget &rest ignore)
			   (message "%s is a good choice!"
				    (widget-value widget)))
		 '(item :tag "This option" :value "This")
		 '(choice-item "That option")
		 '(editable-field :menu-tag "No option" "Thus option"))
  (widget-insert "Address: ")
  (widget-create 'editable-field
		 "Some Place\nIn some City\nSome country.")
  (widget-insert "\nSee also ")
  (widget-create 'link
		 :notify (lambda (&rest ignore)
			   (widget-value-set widget-example-repeat
					     '("En" "To" "Tre"))
			   (widget-setup))
		 "other work")
  (widget-insert " for more information.\n\nNumbers: count to three below\n")
  (setq widget-example-repeat
	(widget-create 'editable-list
		       :entry-format "%i %d %v"
		       :notify (lambda (widget &rest ignore)
				 (let ((old (widget-get widget
							':example-length))
				       (new (length (widget-value widget))))
				   (unless (eq old new)
				     (widget-put widget ':example-length new)
				     (message "You can count to %d." new))))
		       :value '("One" "Eh, two?" "Five!")
		       '(editable-field :value "three")))
  (widget-insert "\n\nSelect multiple:\n\n")
  (widget-create 'checkbox t)
  (widget-insert " This\n")
  (widget-create 'checkbox nil)
  (widget-insert " That\n")
  (widget-create 'checkbox
		 :notify (lambda (&rest ignore) (message "Tickle"))
		 t)
  (widget-insert " Thus\n\nSelect one:\n\n")
  (widget-create 'radio-button-choice
		 :value "One"
		 :notify (lambda (widget &rest ignore)
			   (message "You selected %s"
				    (widget-value widget)))
		 '(item "One") '(item "Another One.") '(item "A Final One."))
  (widget-insert "\n")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (if (= (length (widget-value widget-example-repeat))
				  3)
			       (message "Congratulation!")
			     (error "Three was the count!")))
		 "Apply Form")
  (widget-insert " ")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (widget-example))
		 "Reset Form")
  (widget-insert "\n")
  (use-local-map widget-keymap)
  (widget-setup))


(provide 'gpb-settings)
