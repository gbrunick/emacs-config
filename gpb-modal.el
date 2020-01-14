;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  VIM-ish modal editing.  But not that much like VIM.
;;
;;  In insert mode, the map `gpb-modal--insert-mode-map' is merged with any
;;  text property keymaps to produce the top-level keymap.
;;
;;  In command mode, the following maps are merged and installed in the
;;  top-level keymap
;;
;;    * `gpb-modal--active-region-map' if `region-active-p'
;;    * Active keymaps from `gpb-modal--conditional-maps'
;;    * `gpb-modal--command-mode-map'
;;    * Any text property keymap that is active at the point
;;
;;  and the map `gpb-modal--weak-command-mode-map' is appended to the
;;  current buffer local map.
;;
;;  See `gpb-modal--get-top-level-map' for the details of the construction
;;  of the top-level map.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl-lib)
(require 'gpb-logging)
(require 'gpb-text-objects)

(defcustom gpb-modal-enable-logging nil
  "If non-nil, log information to *Modal Log* buffer."
  :type 'boolean
  :group 'gpb-modal)

(defcustom gpb-modal-overlay-priority 200
  "The priority on the overlay that carries the overriding modal keymap."
  :type 'integer
  :group 'gpb-modal)

(defcustom gpb-modal-command-cursor-type 'box
  ;; If `cursor-type' is nil, then emacs does not show a cursor in
  ;; either selected or nonselected windows (see get_window_cursor_type
  ;; in xdisp.c), so is better to use '(bar . 0) than nil.
  "Value for `cursor-type' when in command mode."
  :group 'gpb-modal)

(defcustom gpb-modal-insert-cursor-type 'bar
  "Value for `cursor-type' when in insert mode."
  :group 'gpb-modal)

(defcustom gpb-modal-enter-insert-mode-hook nil
  "Normal hook run when entering insert mode."
  :type 'hook
  :group 'gpb-modal)

(defcustom gpb-modal-enter-command-mode-hook nil
  "Normal hook run when entering command mode."
  :type 'hook
  :group 'gpb-modal)

(defcustom gpb-modal-enter-insert-mode-after
  `(gpb-latex-insert-item kmacro-start-macro-or-insert-counter)
  "The function `gpb-modal--post-command-hook' automatically
switches to insert mode after one of these commands.  This may be
convenient for commands that usually precede the entering of
text."
  :type '(repeat function)
  :group 'gpb-modal)

(defcustom gpb-modal-enter-command-mode-after
  `(gpb-eval-prev-defun eval-defun set-mark-command
    save-buffer gpb-latex-compile-document
    gpb-lisp-eval-something gpb-execute-shell-script)
  "The function `gpb-modal--post-command-hook' automatically
switches to command mode after one of these commands.  This may
be convenient for commands that usually follow the completion of
editing or precede cursor movement like saving the file or
setting the mark in transient mark mode."
  :type '(repeat function)
  :group 'gpb-modal)

(defvar gpb-modal--current-mode nil
  "The current global editing mode.

Warning: this use of the word \"mode\" is similiar to VIM and
local to the gpb-modal package.  In particular, this use of the
word \"mode\" does not agree with the usual meaning of the word
\"mode\" in Emacs such as `major-mode' and `minor-mode-alist'.")

(defvar gpb-modal--previous-buffer nil
  "The function `gpb-modal--post-command-hook' uses this variable
  to determine if the current buffer has changed.")

(defvar gpb-modal--previous-window nil
  "The function `gpb-modal--post-command-hook' uses this variable
  to determine if the current window has changed.")

(defvar gpb-modal--mode-changed nil
  "Has the mode just changed?\n
The functions `gpb-modal-enter-command-mode' and
`gpb-modal-enter-insert-mode' set this flag so that
`gpb-modal--post-command-hook' does not undo these changes.")

(defvar-local gpb-modal--keymap-overlay nil
  "The buffer local overlay that carries the top-level keymap.")
(put 'gpb-modal--keymap-overlay 'permanent-local t)


(define-minor-mode gpb-modal-mode
  "A global modal editting mode for cursor movement loosely based on vim.

Use the command \\[gpb-model-describe-bindings] to see the
current active keybindings."
  :global t
  (if gpb-modal-mode
      (progn
        (add-hook 'post-command-hook 'gpb-modal--post-command-hook)
        (gpb-modal-enter-command-mode))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when gpb-modal--keymap-overlay
          (delete-overlay gpb-modal--keymap-overlay)
          (setq gpb-modal--keymap-overlay nil))))
    (remove-hook 'post-command-hook 'gpb-modal--post-command-hook)))


(defun gpb-model-describe-bindings (&optional buf)
  (interactive)
  (let ((buf (or buf (current-buffer))))
    (help-setup-xref (list #'gpb-model-describe-bindings buf)
                     (called-interactively-p 'interactive))
    (with-help-window (help-buffer)
      (with-current-buffer buf
        (let ((insert-map (gpb-modal--get-top-level-map 'insert))
              (command-map (gpb-modal--get-top-level-map 'command)))
          (princ "Modal Key Bindings\n")
          (princ "==================\n\n")
          (princ "Insert mode bindings:\n\n")
          (princ (substitute-command-keys "\\{insert-map}"))
          (princ "\nCommand mode bindings:\n\n")
          (princ (substitute-command-keys "\\{command-map}")))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Keymaps and keymap management functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun gpb-modal-define-conditional-key (pred key def)
  "Bind KEY to DEF in command mode when PRED is defined and non-nil."
  (let ((map (or ;; If there is already a keymap associted with PRED, us it.
                 (alist-get pred gpb-modal--conditional-maps)
                 ;; Otherwise, create a new keymap and assocaite it with PRED.
                 (let ((new-map (make-sparse-keymap)))
                   (push (cons pred new-map)
                         gpb-modal--conditional-maps)
                   new-map))))
    (define-key map key def)))


(defvar gpb-modal--insert-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?j)] 'gpb-modal-enter-command-mode)
    (define-key map [?\C- ] 'gpb-modal-enter-command-mode)
    (fset 'gpb-modal--insert-mode-map map)
    map)
  "The global keymap for insert mode.")


(defvar gpb-modal--command-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "i" 'gpb-modal-enter-insert-mode)
    (define-key map "\M-i" 'gpb-modal--execute-one-command)

    (define-key map "j" 'next-line)
    (define-key map "k" 'previous-line)
    (define-key map "h" 'backward-char)
    (define-key map "l" 'forward-char)

    (define-key map " " 'gpb-modal--next-VIM-WORD)
    (define-key map [(shift ?\ )] 'gpb-modal--beginning-of-VIM-WORD)

    (define-key map [(control tab)] 'gpb-next-window)
    (define-key map [(control shift iso-lefttab)] 'gpb-previous-window)
    (define-key map [(control shift tab)] 'gpb-previous-window)
    (define-key map "\C-w" 'gpb-kill-buffer)

    ;; This binding provides access to the weak command map.
    (define-key map [(control ?j)] 'gpb-modal--weak-command-mode-map)

    (fset 'gpb-modal--command-mode-map map)
    map))


(defvar gpb-modal--weak-command-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap self-insert-command] 'gpb-modal--not-defined)

    (dolist (key '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
      (define-key map key 'digit-argument))

    (define-key map "t" 'isearch-forward)
    (define-key map "T" 'isearch-backward)

    (define-key map "a" 'repeat)
    (define-key map "^" 'back-to-indentation)
    (define-key map "%" 'gpb-modal--to-matching-delimiter)
    (define-key map "J" 'scroll-up-command)
    (define-key map "K" 'scroll-down-command)
    (define-key map "$" 'ispell-word)
    (define-key map "!" 'shell-command)
    (define-key map "&" 'async-shell-command)

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

    (define-key map "q" 'fill-paragraph)
    (define-key map "z" 'undo)
    (define-key map "u" 'undo)
    (define-key map "v" 'yank)
    (define-key map "V" 'gpb-modal--yank-after)
    ;; (define-key map "\t" 'indent-for-tab-command)

    (define-key map [remap describe-mode] 'gpb-modal--describe-mode)


    ;; This allows us to use the symbol
    ;; `gpb-modal--command-mode-map' in keymaps.
    (fset 'gpb-modal--weak-command-mode-map map)

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
    (define-key map "n" 'narrow-to-region)

    (define-key map [(meta u)] 'upcase-region)
    (define-key map [(meta l)] 'downcase-region)
    (define-key map [(meta c)] 'capitalize-region)

    (define-key map "\"" 'gpb-modal--quote-symbols-in-region)
    (define-key map "'" 'gpb-modal--quote-symbols-in-region)
    (define-key map "`" 'gpb-modal--quote-symbols-in-region)

    ;; TODO: This should really only be bound in the ESS major modes.
    (define-key map "C" 'gpb-modal--wrap-in-code)

    (fset 'gpb-modal--active-region-map map)
    map)
  "This keymap is added when the region is active.")


(defvar gpb-modal--conditional-maps nil
  "An alist of conditional command mode keymaps.

Don't set this variable directly; use `gpb-model:define-key'.
Each car is a symbol giving a major or minor mode name.  The map
is activated in command mode when the mode is active.")


(defun gpb-modal-enter-command-mode ()
  "Enter command mode."
  (interactive)
  (setq gpb-modal--current-mode 'command
        gpb-modal--mode-changed t)
  (let ((symbol 'gpb-modal--weak-command-mode-map)
        (local-map (current-local-map)))
    ;; Ensure there is a local map.
    (when (null local-map)
      (setq local-map (make-sparse-keymap))
      (use-local-map local-map))
    ;; The following sexp modifies the `local-map' list in-place to ensure
    ;; that the symbol `gpb-modal--weak-command-mode-map' appears exactly
    ;; once at the end of the list.
    (nconc (delq symbol local-map) (list symbol)))
  (run-hooks 'gpb-modal-enter-command-mode-hook)
  (when (called-interactively-p)
    (unless (minibuffer-window-active-p (selected-window)))))


(defun gpb-modal-enter-insert-mode ()
  "Enter insert mode."
  (interactive)
  (setq gpb-modal--current-mode 'insert
        gpb-modal--mode-changed t)
  ;; The following uses the fact that the symbol
  ;; `gpb-modal--weak-command-mode-map' is never the first entry in the
  ;; current local map list.
  (delq 'gpb-modal--weak-command-mode-map (current-local-map))
  (run-hooks 'gpb-modal-enter-insert-mode-hook)
  (when (called-interactively-p)
    (unless (minibuffer-window-active-p (selected-window)))))


(defun gpb-modal--get-top-level-map (&optional mode)
  "Get the current top-level keymap.

MODE should be the symbol insert or command.  The map returned is
the top-level keymap that is added to the overlay keymap in each
buffer."
  (gpb-modal--with-disabled-overlay-keymap
    (case (or mode gpb-modal--current-mode)
      (insert
       (make-composed-keymap `(gpb-modal--insert-mode-map)
                             (get-char-property (point) 'keymap)))

      (command
       (let ((region-map (when (region-active-p)
                           'gpb-modal--active-region-map))
             (cond-maps (mapcar (lambda (x)
                                  (when (or (and (boundp (car x))
                                                 (symbol-value (car x)))
                                            (derived-mode-p (car x)))
                                    (cdr x)))
                                gpb-modal--conditional-maps))
             (global-map 'gpb-modal--command-mode-map)
             ;; The overlay keymap will override any other overlay or text
             ;; properties, so we find the keymap we are covering and put it
             ;; last in the list below.
             (text-prop-map (get-char-property (point) 'keymap))
             map-list map)

         (setq map-list `(,region-map ,@cond-maps ,global-map ,text-prop-map)
               map (make-composed-keymap (delq nil map-list)))
         (gpb-modal--log-forms 'region-map 'cond-maps 'global-map 'keymap 'map)
         map))

      (t (error "Invalid mode %s" mode)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  The main functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun gpb-modal--update-overlay ()
  "Update the top-level keymap according to `gpb-modal--current-mode'.

Ensures that `gpb-modal--keymap-overlay' is defined and updates
the keymap on this overlay."
  (gpb-modal--log-forms '(current-buffer)
                        'gpb-modal--keymap-overlay
                        '(overlay-buffer gpb-modal--keymap-overlay))

  ;; Its not entirely clear what causes this to happen. -gpb
  (when (and (overlayp gpb-modal--keymap-overlay)
             (null (overlay-buffer gpb-modal--keymap-overlay)))
    (delete-overlay gpb-modal--keymap-overlay)
    (gpb-modal--log-message "Deleted corrupted overlay in %s" (current-buffer))
    (setq gpb-modal--keymap-overlay nil))

  ;; If there is no overlay, create one.
  (when (null gpb-modal--keymap-overlay)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'priority gpb-modal-overlay-priority)
      (setq gpb-modal--keymap-overlay ov))
    (gpb-modal--log-message "Created new overlay in %s" (current-buffer)))

  (let ((modal-map (gpb-modal--get-top-level-map)))
    ;; Ensure that the overlay covers the entire buffer.
    (save-restriction
      (widen)
      (move-overlay gpb-modal--keymap-overlay (point-min) (point-max)))
    ;; Update the overlay keymap.
    (overlay-put gpb-modal--keymap-overlay 'keymap modal-map)
    (gpb-modal--log-forms 'gpb-modal--current-mode 'modal-map)))


(defmacro gpb-modal--with-disabled-overlay-keymap (&rest body)
  "Temporary disable the modal key map while evaluating BODY."
  (declare (indent 0) (debug t))
  `(let ((keymap (and gpb-modal--keymap-overlay
                      (overlay-get gpb-modal--keymap-overlay 'keymap))))
     (overlay-put gpb-modal--keymap-overlay 'keymap nil)
     (unwind-protect
         (progn ,@body)
       (when (and gpb-modal--keymap-overlay keymap)
         (overlay-put gpb-modal--keymap-overlay 'keymap keymap)))))


(defun gpb-modal--describe-mode ()
  "Disable modal keybindings and then call `describe-mode'."
  (interactive)
  (gpb-modal--with-disabled-overlay-keymap (describe-mode)))


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
           ;; If the mode has been changed by `gpb-modal-enter-command-mode'
           ;; or `gpb-modal-enter-insert-mode' during the last command, then
           ;; we don't change it again.
           (gpb-modal--mode-changed)

           ;; It seems more reliable to test `this-original-command'
           ;; than `this-command'.
           ((member this-original-command gpb-modal-enter-command-mode-after)
            (gpb-modal--log-message "case 1")
            (gpb-modal-enter-command-mode))

           ((member this-original-command gpb-modal-enter-insert-mode-after)
            (gpb-modal--log-message "case 2")
            (gpb-modal-enter-insert-mode))

           ((or buffer-changed window-changed)
            (with-current-buffer next-buffer
              (cond
               ;; Enter insert mode when changing to the minibuffer.
               ((minibuffer-window-active-p (selected-window))
                (gpb-modal--log-message "case 3")
                (gpb-modal-enter-insert-mode))
               (t
                (gpb-modal--log-message "case 4")
                (gpb-modal-enter-command-mode))))))

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
                                  'cursor-type)))

      (error
       (with-current-buffer (get-buffer-create "*Modal Log*")
         (save-excursion
           (goto-char (point-max))
           (insert (format "Error in gpb-modal--post-command-hook:\n  %S\n" exc))
           (setq gpb-modal-enable-logging t)))))

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
  "Move to the beginning of the next VIM-style word.

With a prefix argument, moves to beginning of text object and
continues moving backwards on consecutive calls."
  (interactive "P")
  ;; The S-SPC keybinding doesn't work in the terminal.
  (if (or arg (and (eq last-command 'gpb-modal--next-VIM-WORD)
                   (eq gpb-modal--last-direction -1)))
      (progn
        (goto-beginning-of-text-object 'VIM-WORD (point))
        (setq gpb-modal--last-direction -1))
    (goto-next-text-object 'VIM-WORD (point))
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


;; When a file is opened using emacsclient, the `find-file' is called
;; after the post command loop has already run.

(add-hook 'find-file-hook 'gpb-modal-enter-command-mode)

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
  (let* ((quote-char (cond
                      ((string-equal (this-command-keys) "`") "`")
                      ((string-equal (this-command-keys) "'") "'")
                      (t "\"")))
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
                          'source-buffer 'cursor-type)))


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
  (when gpb-modal-enable-logging
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
  (when gpb-modal-enable-logging
    (dolist (form forms)
      (let* ((value (condition-case exc (eval form)
                      ('error (format "Error: %s" exc)))))
        (gpb-modal--log-message "%S = %S" form value)))))


;;  Man-mode
(add-hook 'Man-mode-hook 'gpb-modal-enter-command-mode)

(eval-after-load 'eldoc
  '(eldoc-add-command-completions "gpb-modal--next-"
                                  "gpb-modal--beginning-of-"
                                  "gpb-modal--end-of-"))

;; Magit integration --------------------------------------------------

(add-hook 'magit-status-mode-hook 'gpb-magit-status-mode-setup)
(add-hook 'magit-popup-mode-hook 'gpb-modal-enter-insert-mode)


;; Rectangle mark mode ------------------------------------------------

(cl-flet ((define-cond-key 'gpb-modal-define-conditional-key))
  (define-cond-key 'rectangle-mark-mode "c" 'copy-rectangle-as-kill)
  (define-cond-key 'rectangle-mark-mode "\C-c" 'copy-rectangle-as-kill)
  (define-cond-key 'rectangle-mark-mode "d" 'delete-rectangle)
  (define-cond-key 'rectangle-mark-mode "x" 'kill-rectangle)
  (define-cond-key 'rectangle-mark-mode "\C-x" 'kill-rectangle)
  (define-cond-key 'rectangle-mark-mode "s" 'string-rectangle)
  (define-cond-key 'rectangle-mark-mode "v" 'yank-rectangle)
  (define-cond-key 'rectangle-mark-mode "\C-v" 'yank-rectangle)
  (define-cond-key 'rectangle-mark-mode " " 'clear-rectangle)
  (define-cond-key 'rectangle-mark-mode "i" 'string-rectangle))


;; Polymode integration --------------------------------------------------

(defun gpb-modal--polymode-after-switch-buffer-hook (prebuf postbuf)
  "Hook for integration with polymode.

Polymode copies all the overlays from PREBUF to POSTBUF before
calling this hook.  This causes problems because we end up with
two conflicting modal keymap overlays in POSTBUF, so we delete
the overlay that came from PREBUF below."
  (with-current-buffer prebuf
    (unless (null gpb-modal--keymap-overlay)
      (delete-overlay gpb-modal--keymap-overlay)
      (setq gpb-modal--keymap-overlay nil))))

(add-hook 'polymode-after-switch-buffer-hook
          'gpb-modal--polymode-after-switch-buffer-hook)


(provide 'gpb-modal)
