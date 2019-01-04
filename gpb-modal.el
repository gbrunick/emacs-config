;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  VIM-ish modal editing.  But not that much like VIM.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl-lib)
(require 'gpb-logging)
(require 'gpb-text-objects)

(defvar gpb-modal--enable-logging nil
  "If non-nil, log information to *Modal Log* buffer.")

(defcustom gpb-modal-command-cursor-type 'box
  ;; If `cursor-type' is nil, then emacs does not show a cursor in
  ;; either selected or nonselected windows (see get_window_cursor_type
  ;; in xdisp.c), so is better to use '(bar . 0) than nil.
  "Value for `cursor-type' when in command mode.")

(defcustom gpb-modal-insert-cursor-type 'bar
  "Value for `cursor-type' when in insert mode.")

(define-minor-mode gpb-modal-mode
  "A global modal editting mode for cursor movement loosely based on vim.

Use the command \\[gpb-model-describe-bindings] to see the
current active keybindings."
  :global t
  (if gpb-modal-mode
      (progn
        (add-hook 'post-command-hook 'gpb-modal--post-command-hook)
        (set-default 'cursor-in-non-selected-windows '(bar . 1))
        (gpb-modal--enter-command-mode))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when gpb-modal--keymap-overlay
          (delete-overlay gpb-modal--keymap-overlay)
          (setq gpb-modal--keymap-overlay nil))))
    (remove-hook 'post-command-hook 'gpb-modal--post-command-hook)
    (set-default 'cursor-in-non-selected-windows t)))

(defvar gpb-modal--current-mode nil
  "The current global editing mode.

Warning: this use of the word \"mode\" is similiar to VIM and
local to the gpb-modal package.  In particular, this use of the
word \"mode\" does not agree with the usual meaning of the word
\"mode\" in Emacs such as `major-mode' and `minor-mode-alist'.")

(defvar gpb-modal--enter-insert-mode-hook nil
  "Functions run after entering insert mode")

(defvar gpb-modal--enter-command-mode-after
  `(gpb-eval-prev-defun eval-defun set-mark-command gpb-set-mark-command
    gpb-eval-buffer save-buffer gpb-latex-compile-document
    gpb-lisp-eval-something gpb-execute-shell-script)
  "The function `gpb-modal--post-command-hook' automatically
switches to command mode after one of these commands.  This may
be convenient for commands that usually follow the completion of
editing or precede cursor movement like saving the file or
setting the mark in transient mark mode.")

(defvar gpb-modal--enter-insert-mode-after
  `(gpb-latex-insert-item kmacro-start-macro-or-insert-counter)
  "The function `gpb-modal--post-command-hook' automatically
switches to insert mode after one of these commands.  This may be
convenient for commands that usually precede the entering of
text.")

(defvar gpb-modal--enter-insert-mode-major-modes
  `(;;imenu-tree-mode browse-kill-ring-mode
    ;; property-list-mode
    ediff-mode calc-mode
    ;;debugger-mode ;;Buffer-menu-mode
    ;;log-edit-mode bookmark-bmenu-mode
    ;;Info-mode doc-view-mode ;; vc-dir-mode vc-hg-log-view-mode
    ;;Man-mode ;; ert-results-mode
    )
  "If a major mode is contained in this list, then we enter
insert mode when we switch to that buffer.  Normally we switch to
command mode when we enter a buffer.  See
`gpb-modal--insert-mode-buffer-p' for more.")

(defvar gpb-modal--insert-mode-buffer-predicates nil
  "Each function in this list is called with a single argument
which is equal to a buffer.  If the function returns t, then we
enter insert mode when we switch to the buffer.  Normally we
switch to command mode when we enter a buffer.  See
`gpb-modal--insert-mode-buffer-p' for more.")

;; (add-hook 'Man-mode-hook 'gpb-modal--enter-insert-mode)

(defvar gpb-modal--previous-buffer nil
  "The function `gpb-modal--post-command-hook' uses this variable
  to determine if the current buffer has changed.")

(defvar gpb-modal--previous-window nil
  "The function `gpb-modal--post-command-hook' uses this variable
  to determine if the current window has changed.")

(defvar gpb-modal--mode-changed nil
  "Has the mode just changed?\n
The functions `gpb-modal--enter-command-mode' and
`gpb-modal--enter-insert-mode' set this flag so that
`gpb-modal--post-command-hook' does not undo these changes.")

(defvar-local gpb-modal--keymap-overlay nil
  "The the buffer local overlay that enables the modal keymap.")
(put 'gpb-modal--keymap-overlay 'permanent-local t)

(defconst gpb-modal--keymap-priority 200)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Keymaps and keymap management functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun gpb-modal--make-soft-command (command)
  "Wrap COMMAND to check for overriding buffer local bindings."
  (assert (commandp command))
  `(lambda ()
    (interactive)
    (let ((binding (gpb-modal--with-disabled-overlay-keymap
                     (or (cdar (minor-mode-key-binding (this-command-keys)))
                         (local-key-binding (this-command-keys))))))
      (message "local binding: %S" binding)
      (if binding
          (call-interactively binding)
        (call-interactively ',command)))))


(defvar gpb-modal--insert-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?j)] 'gpb-modal--enter-command-mode)
    (define-key map [?\C- ] 'gpb-modal--enter-command-mode)
    (fset 'gpb-modal--insert-mode-map map)
    map)
  "The global keymap for insert mode.")

(defvar gpb-modal--global-command-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap self-insert-command] 'gpb-modal--not-defined)
    (dolist (key '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
      (define-key map key (gpb-modal--make-soft-command 'digit-argument)))

    (define-key map "i" 'gpb-modal--enter-insert-mode)
    (define-key map "\M-i" 'gpb-modal--execute-one-command)
    ;; (define-key map "I" 'gpb-modal--enter-overwrite-mode)
    ;; (define-key map [(control ?\;)] 'gpb-modal--enter-insert-mode)

    (define-key map "j" 'next-line)
    (define-key map "k" 'previous-line)
    (define-key map "h" 'backward-char)
    (define-key map "l" 'forward-char)

    (define-key map "t" 'isearch-forward)
    (define-key map "T" 'isearch-backward)

    (define-key map "a" 'repeat)
    (define-key map "^" 'back-to-indentation)
    (define-key map "%" 'gpb-modal--to-matching-delimiter)
    (define-key map "J" 'scroll-up-command)
    (define-key map "K" 'scroll-down-command)
    (define-key map "$" 'end-of-line)

    (define-key map "m" 'mark-text-object)
    ;; (define-key map "M" 'gpb-reg-reselect-region)
    (define-key map "c" 'copy-text-object)
    (define-key map "C" 'copy-append-text-object)

    (define-key map "b" 'goto-beginning-of-text-object)
    (define-key map "e" 'goto-end-of-text-object)
    (define-key map "n" 'goto-next-text-object)
    (define-key map "f" 'isearch-for-text-object)
    (define-key map "F" 'isearch-backward)
    ;;(define-key map "f" 'goto-next-text-object)
    (define-key map "=" 'indent-text-object)
    (define-key map ";" 'comment/uncomment-text-object)
    (define-key map ":" 'gpb-exec-command/eval-expression)

    (define-key map "w" 'gpb-modal--next-word)
    (define-key map "W" 'gpb-modal--beginning-of-word)
    (define-key map "s" 'gpb-modal--next-symbol)
    (define-key map "S" 'gpb-modal--beginning-of-symbol)
    (define-key map " " 'gpb-modal--next-VIM-WORD)
    (define-key map [(shift ?\ )] 'gpb-modal--beginning-of-VIM-WORD)
    (define-key map "p" 'gpb-modal--next-paragraph)
    (define-key map "P" 'gpb-modal--beginning-of-paragraph)
    (define-key map "." 'gpb-modal--next-sentence)
    (define-key map "," 'gpb-modal--beginning-of-sentence)

    (define-key map "(" 'gpb-modal--beginning-of-parenthesis)
    (define-key map ")" 'gpb-model--end-of-parenthesis)
    (define-key map "[" 'gpb-modal--beginning-of-square-braces)
    (define-key map "]" 'gpb-model--end-of-square-braces)
    (define-key map "{" 'gpb-modal--beginning-of-curly-braces)
    (define-key map "}" 'gpb-model--end-of-curly-braces)
    (define-key map "<" 'gpb-modal--beginning-of-indented-block)
    (define-key map ">" 'gpb-model--end-of-indented-block)

    (define-key map "d" 'delete-text-object)
    (define-key map "x" 'kill-text-object)
    (define-key map "X" 'kill-and-append-text-object)
    (define-key map "r" 'replace-text-object-with-clipboard)
    (define-key map "E" 'execute-text-object)

    (define-key map "z" 'undo)
    (define-key map "u" 'undo)
    (define-key map "v" 'yank)
    (define-key map "V" 'gpb-modal--yank-after)
    ;; (define-key map "\t" 'indent-for-tab-command)

    (define-key map [remap describe-mode] 'gpb-modal--describe-mode)

    (define-key map [(control tab)] 'gpb-next-window)
    (define-key map [(control shift iso-lefttab)] 'gpb-previous-window)
    (define-key map [(control shift tab)] 'gpb-previous-window)
    (define-key map "\C-w" 'gpb-kill-buffer)

    (define-key map "q" (gpb-modal--make-soft-command 'fill-paragraph))

    ;; This allows us to use the symbol
    ;; `gpb-modal--global-command-mode-map' in keymaps.
    (fset 'gpb-modal--global-command-mode-map map)

    map)
  "The global keymap for command mode.")


(defvar gpb-modal--active-region-map
  (let ((map (make-sparse-keymap)))
    ;; CUA-style copy/delete bindings
    (define-key map "\C-c" 'copy-region-as-kill)
    (define-key map "\C-x" 'kill-region)

    (define-key map [(tab)] 'indent-for-tab-command)
    (define-key map "o" 'exchange-point-and-mark)
    (define-key map ">" 'gpb-modal--shift-region-right)
    (define-key map "<" 'gpb-modal--shift-region-left)
    (define-key map "$" 'ispell-region)
    ;; (define-key map "\C-n" 'gpb-modal--narrow-to-region)

    (define-key map [(meta u)] 'upcase-region)
    (define-key map [(meta l)] 'downcase-region)
    (define-key map [(meta c)] 'capitalize-region)

    (define-key map "\"" 'gpb-modal--quote-symbols-in-region)
    (define-key map "'" 'gpb-modal--quote-symbols-in-region)

    ;; TODO: This should really only be bound in the ESS major modes.
    (define-key map "C" 'gpb-modal--wrap-in-code)

    (fset 'gpb-modal--active-region-map map)
    map)
  "This keymap is added when the region is active.")


(defvar gpb-modal--command-mode-keymap-alist nil
  "An alist of conditional command mode keymaps.

Each car is a symbol that is evaluated to determine if the keymap
in the cdr should be included in the current command mode
map (see `gpb-modal--get-active-map').  The primary use case for
this variable is enabling additional keybindings when a given
minor mode is enabled.")


(defvar-local gpb-modal--local-command-mode-map nil
  "The buffer local keymap for insert mode.

Don't modify this variable directly; use `gpb-modal:define-key'")


(defun gpb-modal--get-active-map (&optional mode)
  "Get the current active map.

MODE should be the symbol insert or command.  The map returned is
the top-lvel keymap that is added to the overlay keymap in each
buffer."
  (gpb-modal--with-disabled-overlay-keymap
    (case (or mode gpb-modal--current-mode)
      (insert
       (make-composed-keymap `(gpb-modal--insert-mode-map)
                             (get-char-property (point) 'keymap)))

      (command
       (let* ((local-map gpb-modal--local-command-mode-map)
              (region-map (when (region-active-p)
                            'gpb-modal--active-region-map))
              (cond-maps (mapcar (lambda (x) (when (and (boundp (car x))
                                                        (symbol-value (car x)))
                                               (cdr x)))
                                 gpb-modal--command-mode-keymap-alist))
              ;; The overlay keymap will override any other overlay or text
              ;; properties, so we find the keymap we are covering and put it
              ;; last in the list below.
              (keymap (get-char-property (point) 'keymap))
              (global-map 'gpb-modal--global-command-mode-map)
              (map-list (delq nil `(,local-map ,region-map ,@cond-maps
                                               ,global-map ,keymap)))
              (map (make-composed-keymap map-list)))
         (gpb-modal--log-message "Entering let form.")
         ;; The following key binding provides access to the global command
         ;; map, skipping over any local or alist bindings.
         (define-key map "\C-j" 'gpb-modal--global-command-mode-map)
         ;; `gpb-modal--command-mode-map' is only for help/documentation
         ;; purposes.
         (setq-local gpb-modal--command-mode-map map)
         (gpb-modal--log-forms 'local-map 'region-map 'cond-maps
                               'global-map 'keymap 'map)
         map))

    (t (error "Invalid mode %s" mode)))))


(defun gpb-modal--define-command-key (key function &optional local)
  "Define a key binding in the command keymap"
  (declare (obsolete gpb-modal:define-key "2018-11-03"))
  (gpb-modal:define-key (if local :local :command) key function))


(defun gpb-modal:define-key (where key def)
  "Bind KEY to DEF.

The argument WHERE is a symbol: :insert writes to the global
insert mode map: :command writes to the global command mode map,
:local writes to the buffer local command mode map.  Any other
symbol writes to the conditional command mode map and creates a
binding that is active when the symbol is true.  This allows for
command mode bindings that become active when a minor mode is
activated."
  (let ((map (cond ((eq where :insert) gpb-modal--insert-mode-map)
                   ((eq where :command) gpb-modal--global-command-mode-map)
                   ((eq where :local)
                    ;; The first time we define a local key binding in a
                    ;; buffer, `gpb-modal--local-command-mode-map' is nil
                    ;; so we need to create a sparse keymap.
                    (or gpb-modal--local-command-mode-map
                        (setq gpb-modal--local-command-mode-map
                              (make-sparse-keymap))))
                   ((symbolp where)
                    (or (alist-get where gpb-modal--command-mode-keymap-alist)
                        (let ((new-map (make-sparse-keymap)))
                          (push (cons where new-map)
                                gpb-modal--command-mode-keymap-alist)
                          new-map)))
                   (t
                    (error (concat "WHERE must be a :insert, :command, "
                                   ":local or a minor mode symbol."))))))
    (define-key map key def)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  The main functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun gpb-modal--update-overlay ()
  "Update the buffer keymap according to `gpb-modal--current-mode'.

Ensures that `gpb-modal--keymap-overlay' is defined and updates
the keymap on this overlay."
  (gpb-modal--log-forms '(current-buffer)
                        'gpb-modal--keymap-overlay
                        '(overlay-buffer gpb-modal--keymap-overlay))
  (when (and (overlayp gpb-modal--keymap-overlay)
             (null (overlay-buffer gpb-modal--keymap-overlay)))
    (delete-overlay gpb-modal--keymap-overlay)
    (setq gpb-modal--keymap-overlay nil))
  (when (null gpb-modal--keymap-overlay)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'priority gpb-modal--keymap-priority)
      (setq gpb-modal--keymap-overlay ov))
    (gpb-modal--log-message "Created new overlay in %s" (current-buffer)))

  (let ((modal-map (gpb-modal--get-active-map)))
    ;; Ensure that the overlay covers the entire buffer.
    (save-restriction
      (widen)
      (move-overlay gpb-modal--keymap-overlay (point-min) (point-max)))
    (overlay-put gpb-modal--keymap-overlay 'keymap modal-map)
    (gpb-modal--log-forms 'gpb-modal--current-mode 'modal-map)))


;; (defun gpb-modal--disable-overlay-keymap ()
;;   "Disable the overlay keymap"
;;   (when gpb-modal--keymap-overlay
;;     (overlay-put gpb-modal--keymap-overlay 'keymap nil)
;;     (gpb-modal--log-message "Disabled keymap in %s" (current-buffer))))


(defmacro gpb-modal--with-disabled-overlay-keymap (&rest body)
  "Temporary disable the modal key map while evaluating BODY."
  (declare (indent 0) (debug t))
  `(let ((keymap (and gpb-modal--keymap-overlay
                      (prog1
                          (overlay-get gpb-modal--keymap-overlay 'keymap)
                        (overlay-put gpb-modal--keymap-overlay 'keymap nil)))))
     (unwind-protect
         (progn ,@body)
       (when (and gpb-modal--keymap-overlay keymap)
         (overlay-put gpb-modal--keymap-overlay 'keymap keymap)))))


(defun gpb-modal--describe-mode ()
  "Disable modal keybindings and then call `describe-mode'."
  (interactive)
  (gpb-modal--with-disabled-overlay-keymap (describe-mode)))


(defun gpb-model-describe-bindings (&optional buf)
  (interactive)
  (let ((buf (or buf (current-buffer))))
    (help-setup-xref (list #'gpb-model-describe-bindings buf)
                     (called-interactively-p 'interactive))
    (with-help-window (help-buffer)
      (with-current-buffer buf
        (let ((insert-map (gpb-modal--get-active-map 'insert))
              (command-map (gpb-modal--get-active-map 'command)))
          (princ "Modal Key Bindings\n")
          (princ "==================\n\n")
          (princ "Insert mode bindings:\n\n")
          (princ (substitute-command-keys "\\{insert-map}"))
          (princ "\nCommand mode bindings:\n\n")
          (princ (substitute-command-keys "\\{command-map}")))))))


(defun gpb-modal--execute-one-command ()
  "Disable modal keymaps for one command.
You can use this function to get access to keybindings that have
been hidden by the modal mode keybindings.  The overlay keymap
will be reset in the post-command-hook."
  (interactive)
  (gpb-modal--with-disabled-overlay-keymap
    (setq cursor-type gpb-modal-insert-cursor-type)
    (let* ((key-seq (read-key-sequence nil))
           (command (key-binding key-seq)))
      ;; `read-key-sequence' does not update `last-command-event'.  This
      ;; causes problems with, for example, `self-insert-command' which uses
      ;; `last-command-event'.
      (setq last-command-event last-input-event)
      (call-interactively command))))


(defun gpb-modal--enter-command-mode ()
  "Enter command mode."
  (interactive)
  (setq gpb-modal--current-mode 'command
        gpb-modal--mode-changed t)
  (run-hooks 'gpb-modal--enter-command-mode-hook)
  (when (called-interactively-p)
    (unless (minibuffer-window-active-p (selected-window))
      (message "Entering command mode..."))))


(defun gpb-modal--enter-insert-mode ()
  "Enter insert mode."
  (interactive)
  (setq gpb-modal--current-mode 'insert
        gpb-modal--mode-changed t)
  (run-hooks 'gpb-modal--enter-insert-mode-hook)
  (when (called-interactively-p)
    (unless (minibuffer-window-active-p (selected-window))
      (message "Entering insert mode..."))))


(defun gpb-modal--not-defined ()
  (interactive)
  (error "This key is not bound in command mode"))


(defun gpb-modal--post-command-hook ()
  "See `edebug-enter' for more on interactions with edebug"
  (let* ((next-window (selected-window))
         (next-buffer (window-buffer next-window))
         (window-changed (not (eq gpb-modal--previous-window next-window)))
         (buffer-changed (not (eq gpb-modal--previous-buffer next-buffer)))
         (text-props (text-properties-at (point)))
         (overlays (overlays-at (point)))
         (orig-deactivate-mark deactivate-mark))
    (gpb-modal--log-message "<begin gpb-modal--post-command-hook ...")
    (gpb-modal--log-forms 'this-command
                          'this-original-command
                          'mark-active
                          'deactivate-mark
                          'orig-deactivate-mark
                          'edebug-active
                          'edebug-execution-mode
                          'edebug-stop
                          'arg-mode
                          'gpb-modal--previous-buffer
                          'gpb-modal--previous-window
                          '(current-buffer)
                          'next-buffer
                          'next-window
                          'text-props
                          'overlays
                          'window-changed
                          'buffer-changed
                          'gpb-modal--mode-changed)
    (condition-case exc
        (progn
          (cond
           ;; If the mode has been changed by `gpb-modal--enter-command-mode'
           ;; or `gpb-modal--enter-insert-mode' during the last command, then
           ;; we don't change it again.
           (gpb-modal--mode-changed)

           ;; It seems more reliable to test `this-original-command'
           ;; than `this-command'.
           ((member this-original-command gpb-modal--enter-command-mode-after)
            (gpb-modal--log-message "case 1")
            (gpb-modal--enter-command-mode))

           ((member this-original-command gpb-modal--enter-insert-mode-after)
            (gpb-modal--log-message "case 2")
            (gpb-modal--enter-insert-mode))

           ((or buffer-changed window-changed)
            (with-current-buffer next-buffer
              (cond
               ((gpb-modal--insert-mode-buffer-p)
                (gpb-modal--log-message "case 3")
                (gpb-modal--enter-insert-mode))
               (t
                (gpb-modal--log-message "case 4")
                (gpb-modal--enter-command-mode))))))

          (setq gpb-modal--previous-window (selected-window)
                gpb-modal--previous-buffer (current-buffer)
                gpb-modal--mode-changed nil)

          (with-current-buffer next-buffer
            (setq cursor-type (case gpb-modal--current-mode
                                (command gpb-modal-command-cursor-type)
                                (insert gpb-modal-insert-cursor-type)
                                (t (error "Runtime error"))))
            (gpb-modal--update-overlay)
            (gpb-modal--log-forms 'gpb-modal--current-mode
                                  'cursor-type
                                  'cursor-in-non-selected-windows)))

      (error
       (with-current-buffer (get-buffer-create "*Modal Log*")
         (save-excursion
           (goto-char (point-max))
           (insert (format "Error in gpb-modal--post-command-hook:\n  %S\n" exc))
           (setq gpb-modal--enable-logging t)))))

    (gpb-modal--log-forms 'mark-active 'deactivate-mark)
    (gpb-modal--log-message "... end gpb-modal--post-command-hook>")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Various movement commands
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun gpb-modal--beginning-of-curly-braces (arg)
  (interactive "p")
  (goto-beginning-of-text-object 'curly-braces (point) :count arg :outer t))

(defun gpb-modal--beginning-of-indented-block  (arg)
  (interactive "p")
  (beginning-of-line)
  (goto-beginning-of-text-object 'indented-block (point) :count arg)
  (back-to-indentation))

(defun gpb-modal--beginning-of-paragraph  (arg)
  (interactive "p")
  (goto-beginning-of-text-object 'paragraph (point) :count arg))

(defun gpb-modal--beginning-of-parenthesis (arg)
  (interactive "p")
  (goto-beginning-of-text-object 'parenthesis (point) :count arg :outer t))

(defun gpb-modal--beginning-of-sentence  (arg)
  (interactive "p")
  (goto-beginning-of-text-object 'sentence (point) :count arg))

(defun gpb-modal--beginning-of-square-braces (arg)
  (interactive "p")
  (goto-beginning-of-text-object 'square-braces (point) :count arg :outer t))

(defun gpb-modal--beginning-of-symbol (arg)
  (interactive "p")
  (goto-beginning-of-text-object 'symbol (point) :count arg))

(defun gpb-modal--beginning-of-VIM-WORD  (arg)
  (interactive "p")
  (goto-beginning-of-text-object 'VIM-WORD (point) :count arg))

(defun gpb-modal--beginning-of-word (arg)
  (interactive "p")
  (goto-beginning-of-text-object 'word (point) :count arg))

(defun gpb-modal--backward-to-char ()
  (interactive)
  (unless (member last-command '(gpb-modal--forward-to-char
                                 gpb-modal--backward-to-char))
    (setq gpb-modal--forward-to-char--char (read-char "Backward to char: ")))
  (unless mark-active (push-mark nil t))
  (search-backward (char-to-string gpb-modal--forward-to-char--char)))

(defun gpb-model--end-of-indented-block (arg)
  (interactive "p")
  (goto-end-of-text-object 'indented-block (point) :count arg))

(defun gpb-model--end-of-parenthesis (arg)
  (interactive "p")
  (goto-end-of-text-object 'parenthesis (point) :count arg :outer t))

(defun gpb-model--end-of-square-braces (arg)
  (interactive "p")
  (goto-end-of-text-object 'square-braces (point) :count arg :outer t))

(defun gpb-model--end-of-curly-braces (arg)
  (interactive "p")
  (goto-end-of-text-object 'curly-braces (point) :count arg :outer t))

(defun gpb-modal--forward-to-char ()
  (interactive)
  (unless (member last-command '(gpb-modal--forward-to-char
                                 gpb-modal--backward-to-char))
    (setq gpb-modal--forward-to-char--char (read-char "Forward to char: ")))
  (unless mark-active (push-mark nil t))
  (forward-char)
  (let (mark-active)
    (search-forward (char-to-string gpb-modal--forward-to-char--char)))
  (goto-char (match-beginning 0)))

(defun gpb-modal--indent-according-to-mode ()
  (interactive)
  (indent-according-to-mode)
  (next-line))

(defun gpb-modal--insert-mode-buffer-p (&optional buf)
  "Should we enter insert mode when we enter this buffer?

This function consults `gpb-modal--enter-insert-mode-major-modes'
and `gpb-modal--insert-mode-buffer-predicates' to determine if we
should enter insert mode when we switch to BUF.  Normally we
enter command mode when we switch to a buffer."
  (setq buf (or buf (current-buffer)))
  (with-current-buffer buf
    (catch 'done
      (progn
        (when (member major-mode gpb-modal--enter-insert-mode-major-modes)
          (throw 'done t))
        (dolist (pred gpb-modal--insert-mode-buffer-predicates)
          (when (condition-case exc
                    (funcall pred)
                  (error (message
                          "Error in gpb-modal--insert-mode-buffer-p: %s %s"
                          pred exc)
                         nil))
            (throw 'done t)))))))

;; (defun gpb-modal--make-or-move-overlay (ov beg end &optional buf)
;;   (if ov (move-overlay ov beg end buf)
;;     (make-overlay beg end buf)))

(defun gpb-modal--next-paragraph  (arg)
  (interactive "p")
  (goto-next-text-object 'paragraph (point) :count arg))

(defun gpb-modal--next-sentence  (arg)
  (interactive "p")
  (goto-next-text-object 'sentence (point) :count arg))

(defun gpb-modal--next-symbol (arg)
  (interactive "p")
  (goto-next-text-object 'symbol (point) :count arg))

(defvar gpb-modal--last-direction 1
  "Implementation detail of gpb-modal--next-VIM-WORD")

(defun gpb-modal--next-VIM-WORD (arg)
  "Move to the beginning of the next VIM-style word.\n
With a negative prefix argument, moves to beginning of text
object and continues moving backwards on consecutive calls."
  (interactive "p")
  ;; The S-SPC keybinding doesn't work in the terminal.
  (if (or (< arg 0) (and (eq last-command 'gpb-modal--next-VIM-WORD)
                         (eq gpb-modal--last-direction -1)))
      (progn
        (goto-beginning-of-text-object 'VIM-WORD (point)
                                       :count (abs arg))
        (setq gpb-modal--last-direction -1))
    (goto-next-text-object 'VIM-WORD (point) :count arg)
    (setq gpb-modal--last-direction 1)))

(defun gpb-modal--next-word (arg)
  (interactive "p")
  (goto-next-text-object 'word (point) :count arg))

(defun gpb-modal--to-matching-delimiter ()
  "From http://www.emacswiki.org/emacs/ParenthesisMatching#toc4"
  (interactive)
  (let ((syntax (char-syntax (following-char))))
    (cond
     ((= syntax ?\() (forward-sexp 1) (backward-char))
     ((= syntax ?\)) (forward-char) (backward-sexp 1))
     (t (re-search-forward "\\s(\\|\\s)")
        (goto-char (match-beginning 0))
        (gpb-modal--to-matching-delimiter)))))

(defun gpb-modal--yank-after ()
  "Paste text after the point."
  (interactive)
  (save-excursion (call-interactively 'yank)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Customizations and band-aids for a playing well with various major
;;  modes
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Visual cursor feedback for text object commands
(defun gpb-modal--show-bar-cursor () (setq cursor-type 'bar))
(defun gpb-modal--show-box-cursor () (setq cursor-type 'box))

(add-hook 'gpb-tobj--pre-read-text-object-hook 'gpb-modal--show-bar-cursor)
(add-hook 'gpb-tobj--post-read-text-object-hook 'gpb-modal--show-box-cursor)


(defun gpb-modal--use-major-mode-binding ()
  (interactive)
  (let* ((keys (this-command-keys))
         (binding (local-key-binding keys)))
    (message "keys: %s" keys)
    (message "binding: %s" binding)
    (if binding
        (call-interactively binding)
      (error "%s is not defined in the local keymap"
             (key-description (this-command-keys))))))


;; When a file is opened using emacsclient, the `find-file' is called
;; after the post command loop has already run.

(defun gpb-modal--find-file-hook ()
  (if (gpb-modal--insert-mode-buffer-p)
      (gpb-modal--enter-insert-mode)
    (gpb-modal--enter-command-mode)))

(add-hook 'find-file-hook 'gpb-modal--find-file-hook)

;; the minibuffer

(defun gpb-modal--in-minibuffer-p ()
  (minibuffer-window-active-p (selected-window)))

(add-to-list 'gpb-modal--insert-mode-buffer-predicates
             'gpb-modal--in-minibuffer-p)


(defun gpb-modal--find-comment-prefix (beg end)
  "Determine if every line in region starts with a comment string.
If every line in the region spanned by BEG END starts with the
same comment string, we return that common string.  Otherwise, we
return nil."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (beginning-of-line)
    (let* ((prefix (buffer-substring-no-properties
                    (point)
                    (progn (skip-syntax-forward "<") (point))))
           (regex (regexp-quote prefix)))
      (forward-line 1)
      (while (and (> (length prefix) 0) (< (point) end))
        (unless (looking-at-p regex) (setq prefix ""))
        (forward-line 1))
      (when (> (length prefix) 0) prefix))))


(defun gpb-modal--shift-region-right (beg end arg)
  (interactive "r\np")
  (save-mark-and-excursion
    (let* ((deactivate-mark nil)
           (beg (progn (goto-char beg)
                       (beginning-of-line)
                       (point)))
           (comment-prefix (gpb-modal--find-comment-prefix beg end)))

      (if comment-prefix
          (progn
            (let ((beg (copy-marker beg))
                  (end (copy-marker end)))
              (goto-char beg)
              (while (< (point) end)
                (delete-region (point) (+ (point) (length comment-prefix)))
                (forward-line 1))
              (unwind-protect
                  (gpb-modal--shift-region-right beg end arg)
                (goto-char beg)
                (while (< (point) end)
                  (insert comment-prefix)
                  (forward-line 1)))))

        (set-mark beg)
        (while (< (point) end)
          (when (and (< arg 0)
                     (< (current-indentation) (- arg))
                     (not (looking-at "[ \t]*$")))
            (user-error "Can't dedent region any further"))
          (forward-line))
        (indent-rigidly beg (point) arg)))))


(defun gpb-modal--shift-region-left (beg end arg)
  (interactive "r\np")
  (gpb-modal--shift-region-right beg end (- arg)))


(defun gpb-modal--narrow-to-region (beg end arg)
  (interactive "r\nP")
  (deactivate-mark)
  (with-current-buffer (clone-indirect-buffer nil nil)
    (narrow-to-region beg end)
    (if arg (switch-to-buffer-other-window (current-buffer)))
    (switch-to-buffer (current-buffer))))


(defun gpb-modal--quote-symbols-in-region (beg end &optional unquote)
  (interactive "r\nP")
  (let* ((quote-char (if (string-equal (this-command-keys) "'") "'" "\""))
         (end-marker (copy-marker end)))
    (message "this-command-keys: %S" (this-command-keys))
    (save-excursion
      (goto-char beg)
      (cond
       (unquote
        (while (re-search-forward "['\"]\\(\\_<\\(:?\\sw\\|\\s_\\)+\\_>\\)['\"]" end-marker t)
          (replace-match (match-string 1))))
       (t
        (while (re-search-forward "\\_<\\(:?\\sw\\|\\s_\\)+\\_>" end-marker t)
          (replace-match (concat quote-char (match-string 0) quote-char))))))))


(defun gpb-modal--wrap-in-code (beg end &optional link)
  "Wrap region in \\code{<region>}.
With a prefix argument, wrap in \\code{\\{link{<region>}}."
  (interactive "r\nP")
  (let* ((end-marker (copy-marker end)))
    (save-excursion
      (goto-char end)
      (if link (insert "}}") (insert "}"))
      (goto-char beg)
      (if link (insert "\\code{\\link{") (insert "\\code{")))))


;; interact nicely with edebug

(defadvice edebug-mode (after gpb-modal-compatibility (&optional arg) activate)
  "Allow edebug to interoperate with `gpb-modal-mode'.

One of the first things that `edebug-enter' does is disable
pre/post command hooks by `let'-binding the hook variables to
nil.  To work around this, we advice `edebug-mode'.  It appears
that this function is called in `edebug--recursive-edit' right
before the `recursive-edit' that correpsonds to the debugger
loop."
  (unless (and arg (< arg 0))
    (gpb-modal--log-forms 'edebug-mode 'post-command-hook '(current-buffer)
                   'source-buffer 'cursor-type)
    (unless (or (member 'gpb-modal--post-command-hook post-command-hook)
                (and (member t post-command-hook)
                     (member 'gpb-modal--post-command-hook
                             (default-value 'post-command-hook))))
      (add-to-list 'post-command-hook 'gpb-modal--post-command-hook))
    (gpb-modal--log-forms 'edebug-mode 'post-command-hook '(current-buffer)
                   'source-buffer 'cursor-type)
    (gpb-modal--enter-insert-mode)))


;; (defadvice edebug-enter (around gpb-modal-compatibility (function args body)
;;                                 activate)
;;   "Allow edebug to interoperate with `gpb-modal-mode'.

;; One of the first things that `edebug-enter' does is disable
;; pre/post command hooks by `let'-binding the hook variables to
;; nil, so we need to advise this function to reinstall the modal
;; pre/post command hooks if we want to the modal to work correctly
;; during edebug."
;;   (let ((source-buffer (marker-buffer (car (get function 'edebug)))))
;;     (gpb-modal--log-forms 'edebug-enter 'post-command-hook '(current-buffer)
;;                                  'source-buffer 'cursor-type)
;;     ;; (when (null pre-command-hook)
;;     ;;   (add-to-list 'pre-command-hook 'gpb-modal--pre-command-hook))
;;     (unless (or (member 'gpb-modal--post-command-hook post-command-hook)
;;                 (and (member t post-command-hook)
;;                      (member 'gpb-modal--post-command-hook
;;                              (default-value 'post-command-hook))))
;;       (add-to-list 'post-command-hook 'gpb-modal--post-command-hook))
;;     (with-current-buffer source-buffer (gpb-modal--enter-insert-mode))
;;     (unwind-protect
;;         ad-do-it
;;       (gpb-modal--enter-command-mode))))
;;     ;; (setq gpb-modal--just-exited-edebug t)))


(defun gpb-modal--in-edebug-buffer-p ()
  (and (boundp 'edebug-mode-map)
       (eq edebug-mode-map (current-local-map))))

(add-to-list 'gpb-modal--insert-mode-buffer-predicates
             'gpb-modal--in-edebug-buffer-p)


(defun gpb-modal--get-caller ()
  (catch 'found-caller
    (mapbacktrace
     (lambda (evald func args flags)
       (let ((fname (or (and (symbolp func) (symbol-name func)) "")))
         (and (string-match "^gpb-modal-" fname)
              (not (cl-some (lambda (x) (string-equal fname x))
                            '("gpb-modal--log-message"
                              "gpb-modal--log-forms"
                              "gpb-modal--get-caller"
                              "gpb-modal--with-disabled-overlay-keymap")))
              (throw 'found-caller fname)))))
    "<no caller>"))


(defun gpb-modal--log-message (message &rest args)
  "Evaluate FORMS and write value to \"*Modal Log*\" buffer.

The forms are only logged if logging has been enabled for the
function using `gpb-log--enable-logging'.  This is not a macro,
so forms must be quoted to prevent premature evaluation."
  (when gpb-modal--enable-logging
    (let ((caller (gpb-modal--get-caller))
          (inhibit-modification-hooks t))
      (when args (setq message (apply 'format message args)))
      (with-current-buffer (get-buffer-create "*Modal Log*")
        (save-excursion
          (goto-char (point-max))
          (insert (format "%s: %s\n" caller message)))))))


(defun gpb-modal--log-forms (&rest forms)
  "Evaluate FORMS and write value to \"*Modal Log*\" buffer.

The forms are only logged if logging has been enabled for the
function using `gpb-log--enable-logging'.  This is not a macro,
so forms must be quoted to prevent premature evaluation."
  (when gpb-modal--enable-logging
    (dolist (form forms)
      (let* ((value (condition-case exc (eval form)
                      ('error (format "Error: %s" exc)))))
        (gpb-modal--log-message "%S = %S" form value)))))


;; Interact nicely with commands issued by emacsclient outside of the
;; usual command loop
;; (defadvice server-process-filter (around gpb-modal-compatibility activate)
;;   "Allow emacslient to interoperate with `gpb-modal-mode'."
;;   (gpb-modal--pre-command-hook)
;;   ad-do-it
;;   (gpb-modal--post-command-hook))

;; comint mode

(defun gpb-modal--at-comint-prompt-p ()
  (let ((proc (get-buffer-process (current-buffer))))
    (and (derived-mode-p 'comint-mode)
         proc
         (>= (point) (process-mark proc)))))

(add-to-list 'gpb-modal--insert-mode-buffer-predicates
             'gpb-modal--at-comint-prompt-p)

(defun gpb-modal--enter-insert-in-comint ()
  (interactive)
  (when (and (derived-mode-p 'comint-mode)
             (eq text-object 'gpb-buffer))
    (gpb-modal--enter-insert-mode)))

(add-hook 'post-goto-end-of-text-object-hook 'gpb-modal--enter-insert-in-comint)

;;  Man-mode
(add-hook 'Man-mode-hook 'gpb-modal--enter-command-mode)


;; vc-mode integration --------------------------------------------------

;; (add-hook 'vc-dir-mode-hook 'gpb-modal--vc-dir-mode-hook)
(defun gpb-modal--vc-dir-mode-hook ()
  (gpb-modal--define-command-key "u" gpb-modal--use-major-mode-binding t)
  (gpb-modal--define-command-key "m" gpb-modal--use-major-mode-binding t)
  (gpb-modal--define-command-key "n" gpb-modal--use-major-mode-binding t)
  (gpb-modal--define-command-key "p" gpb-modal--use-major-mode-binding t)
  (gpb-modal--define-command-key "g" gpb-modal--use-major-mode-binding t))

;; (add-hook 'property-list-mode-hook 'gpb-modal--property-list-mode-hook)
;; (defun gpb-modal--property-list-mode-hook ()
;;   (gpb-modal--define-command-key "j" 'widget-forward t)
;;   (gpb-modal--define-command-key "l" 'widget-forward t)
;;   (gpb-modal--define-command-key "h" 'widget-backward t)
;;   (gpb-modal--define-command-key "k" 'widget-backward t)
;;   (gpb-modal--define-command-key " " nil t))

(add-hook 'Buffer-menu-mode-hook 'gpb-modal--Buffer-menu-mode-hook)
(defun gpb-modal--Buffer-menu-mode-hook ()
  (gpb-modal--define-command-key "d" 'gpb-modal--use-major-mode-binding t)
  (gpb-modal--define-command-key "x" 'gpb-modal--use-major-mode-binding t))


(eval-after-load 'eldoc
  '(eldoc-add-command-completions "gpb-modal--next-"
                                  "gpb-modal--beginning-of-"
                                  "gpb-modal--end-of-"))

;; Magit integration --------------------------------------------------

(defun gpb-magit-status-mode-setup ()
  (gpb-modal--define-command-key  [tab] 'magit-section-toggle t)
  (gpb-modal--define-command-key  [(backtab)] 'magit-section-cycle t))

(add-hook 'magit-status-mode-hook 'gpb-magit-status-mode-setup)
(add-hook 'magit-popup-mode-hook 'gpb-modal--enter-insert-mode)


;; Rectangle mark mode ------------------------------------------------

(gpb-modal:define-key 'rectangle-mark-mode "c" 'copy-rectangle-as-kill)
(gpb-modal:define-key 'rectangle-mark-mode "\C-c" 'copy-rectangle-as-kill)
(gpb-modal:define-key 'rectangle-mark-mode "d" 'delete-rectangle)
(gpb-modal:define-key 'rectangle-mark-mode "x" 'kill-rectangle)
(gpb-modal:define-key 'rectangle-mark-mode "\C-x" 'kill-rectangle)
(gpb-modal:define-key 'rectangle-mark-mode "s" 'string-rectangle)
(gpb-modal:define-key 'rectangle-mark-mode "v" 'yank-rectangle)
(gpb-modal:define-key 'rectangle-mark-mode "\C-v" 'yank-rectangle)
(gpb-modal:define-key 'rectangle-mark-mode " " 'clear-rectangle)


(provide 'gpb-modal)
