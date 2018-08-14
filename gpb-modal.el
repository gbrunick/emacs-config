;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  VIM-ish modal editing.  But not that much like VIM.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'gpb-logging)
(require 'gpb-text-objects)

(when nil
  (progn
    (gpb-log--enable-logging 'gpb-modal--pre-command-hook)
    (gpb-log--enable-logging 'gpb-modal--post-command-hook)
    (gpb-log--enable-logging 'gpb-modal--show-fake-cursor))
  (progn
    (gpb-log--disable-logging 'gpb-modal--pre-command-hook)
    (gpb-log--disable-logging 'gpb-modal--post-command-hook)
    (gpb-log--disable-logging 'gpb-modal--show-fake-cursor)))

(defcustom gpb-modal-command-cursor-type 'box
  ;; If `cursor-type' is nil, then emacs does not show a cursor in
  ;; either selected or nonselected windows (see get_window_cursor_type
  ;; in xdisp.c), so is better to use '(bar . 0) than nil.
  "Value for `cursor-type' when in command mode.")

(defcustom gpb-modal-insert-cursor-type 'bar
  "Value for `cursor-type' when in insert mode.")

(defcustom gpb-modal-nonselected-cursor-type '(bar . 1)
  "Value for `cursor-in-non-selected-windows' which determines
how the cursor appears in nonselected window.")

(defface gpb-modal-command-cursor-face
  '((((type tty)) (:inverse-video t :inherit default)))
  "Face that is added to the character following the current
point to indicate that the the modal editing state is command
mode.  If there are no characters following the current point, a
space is displayed with this face using overlays.  This was added
to that we can simulate a box cursor in a terminal where Emacs
does not have control over the appearance of the cursor.  In a
graphical display, it is better to use `gpb-modal-command-cursor-type'
to control the appear of the cursor in command mode.")

(defface gpb-modal-insert-cursor-face nil
  "Face that is added to the character following the current
point to indicate that the the modal editing state is insert
mode.  See `gpb-modal-command-cursor-face' and
`gpb-modal-insert-cursor-type' for more information.")

(defvar gpb-modal-cursor-overlay nil)

(defvar gpb-modal-cursor-marker nil
  "Used to store the current value of the point between commands.\n
What would really want is the top-level value of the point in a
window ignoring any current `save-excursions', but the
documentation of `window-point' suggests that we can't get our
ands on this information.  Instead, we set this marker to the
current value of the point in a `post-command-hook' and then hope
that is stays in sync with the point if the buffer is modified
between commands.")

(defvar gpb-modal--last-direction 1)

(define-minor-mode gpb-modal-mode
  "A global modal editting mode for cursor movement loosely based on vim.

  Keymap in \"insert\" mode:

\\{gpb-modal--insert-mode-local-map}
  Keymap in \"command\" mode:

\\{gpb-modal--command-mode-local-map}"
  :global t
  (if gpb-modal-mode
      (progn
        (add-hook 'pre-command-hook 'gpb-modal--pre-command-hook)
        (add-hook 'post-command-hook 'gpb-modal--post-command-hook)
        ;; (add-hook 'after-change-functions 'gpb-modal--show-fake-cursor)
        (message "after-change-functions: %S" after-change-functions)
        (unless (member 'gpb-modal--keymap-alist
                        (default-value 'emulation-mode-map-alists))
          (setq-default emulation-mode-map-alists
                        (cons 'gpb-modal--keymap-alist
                              (default-value 'emulation-mode-map-alists))))
        (gpb-modal--enter-command-mode))
    ;; (setq cursor-type cursor-type
    ;;       cursor-in-non-selected-windows gpb-modal--save-cursor-type
    ;;       gpb-modal--cursor-type 'bar)
    (when (member 'gpb-modal--keymap-alist
                  (default-value 'emulation-mode-map-alists))
      (setq-default emulation-mode-map-alists
                    (delq 'gpb-modal--keymap-alist
                          (default-value 'emulation-mode-map-alists))))
    (remove-hook 'pre-command-hook 'gpb-modal--pre-command-hook)
    (remove-hook 'post-command-hook 'gpb-modal--post-command-hook)
    ;; (remove-hook 'after-change-functions 'gpb-modal--show-fake-cursor)
    )
  (message "after-change-functions: %S" after-change-functions))

;; ;; Insert before cua mode
;; (add-to-ordered-list 'emulation-mode-map-alists 'gpb-modal--keymap-alist 300)

(defvar gpb-modal--keymap-alist nil)
(make-variable-buffer-local 'gpb-modal--keymap-alist)
(put 'gpb-modal--keymap-alist 'permanent-local t)

(defvar gpb-modal--current-mode nil
  "The current editing mode.\n Warning: this use of the word
\"mode\" is similiar to VIM and local the the gpb-modal package.
In particular, this use of the word \"mode\" does not agree with
the usual meaning of the word \"mode\" in Emacs such as `major-mode'
and `minor-mode-alist'.")

(defvar gpb-modal--overriding-maps-alist nil
  "Keymaps that should override the modal keymaps.")

(defvar gpb-modal--enter-insert-mode-hook nil
  "Functions run after entering insert mode")

(defvar gpb-modal--enter-command-mode-after
  `(gpb-eval-prev-defun eval-defun set-mark-command gpb-set-mark-command
    gpb-eval-buffer save-buffer gpb-latex-compile-document
    gpb-lisp-eval-something gpb-execute-shell-script)
  "The function `gpb-modal--post-command-hook' automatically
switches to command mode after one of these commands.  This may
be convenient for commands that usually follow the completion of
editing or preceed cursor movement like saving the file or
setting the mark in trnasient mark mode.")

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

;; (defvar gpb-modal--pending-post-command-hook-command nil
;;   "Use this variable to send a message to `gpb-modal--post-command-hook'.

;; The functions `gpb-modal--enter-command-mode' and
;; `gpb-modal--enter-insert-mode' both set this flag to
;; 'dont-change-mode to prevent the post command hook from changing
;; the mode a second time and overriding the intentions of the
;; function that called `gpb-modal--enter-command-mode' or
;; `gpb-modal--enter-insert-mode'.")

(defvar gpb-modal--previous-buffer nil
  "The function `gpb-modal--post-command-hook' uses this variable
  to determine if the current buffer has changed.")

(defvar gpb-modal--previous-window nil
  "The function `gpb-modal--post-command-hook' uses this variable
  to determine if the current window has changed.")

(defvar gpb-modal--previous-frame nil
  "The function `gpb-modal--post-command-hook' uses this variable
  to determine if the current frame has changed.")

(defvar gpb-modal--mode-changed nil
  "Has the mode just changed?\n
The functions `gpb-modal--enter-command-mode' and
`gpb-modal--enter-insert-mode' set this flag so that
`gpb-modal--post-command-hook' does not undo these changes.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Setup insert mode keymaps
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar gpb-modal--insert-mode-global-map (make-sparse-keymap)
  "The global keymap for insert mode.")

(defvar gpb-modal--insert-mode-local-map (make-sparse-keymap)
  "The the buffer local keymap for insert mode.

This variable should never accessed directly.  Instead use
`gpb-modal--define-insert-key'")

(set-keymap-parent gpb-modal--insert-mode-local-map
                   gpb-modal--insert-mode-global-map)

(defun gpb-modal--define-insert-key (key function &optional local)
  "Define a key binding in the insert keymap"
  (if local
      (progn
        (unless (local-variable-p 'gpb-modal--insert-mode-local-map)
          (set (make-variable-buffer-local 'gpb-modal--insert-mode-local-map)
               (copy-keymap gpb-modal--insert-mode-local-map)))
        (define-key gpb-modal--insert-mode-local-map key function))
    (define-key gpb-modal--insert-mode-global-map key function)))

(gpb-modal--define-insert-key [(control ?j)] 'gpb-modal--enter-command-mode)
;; This just makes exiting the minibuffer annoying
;; (gpb-modal--define-insert-key [(control ?g)] 'gpb-modal--enter-command-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Setup command mode keymaps
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar gpb-modal--command-mode-global-map (make-sparse-keymap)
  "The global keymap for command mode.")

(defvar gpb-modal--command-mode-local-map
  (let ((map (make-sparse-keymap)))
    ;; The following key binding provides access to the global command map
    ;; from the local command map.  This allows us access bindings in the
    ;; parent map that we intentionally hide in the local command map by
    ;; calling (gpb-modal--define-command-key key nil t).  We may choose to
    ;; hide these bindings in a given buffer so that they don't conflict
    ;; with important major mode bindings.  By placing this binding in the
    ;; local map, rather than the global command map, we avoid a recursive
    ;; loop in keymap (which probably doesn't matter but seems unpleasant.
    (define-key map "\C-j" gpb-modal--command-mode-global-map)
    map)
  "The the buffer local keymap for insert mode.

This variable should never accessed directly.  Instead use
`gpb-modal--define-command-key'.")

(set-keymap-parent gpb-modal--command-mode-local-map
                   gpb-modal--command-mode-global-map)

(defun gpb-modal--define-command-key (key function &optional local)
  "Define a key binding in the command keymap"
  (if local
      (progn
        (unless (local-variable-p 'gpb-modal--command-mode-local-map)
          (let ((map (copy-keymap gpb-modal--command-mode-local-map)))
            (set (make-variable-buffer-local 'gpb-modal--command-mode-local-map)
                 map)))
        (define-key gpb-modal--command-mode-local-map key function))
    (define-key gpb-modal--command-mode-global-map key function)))


(gpb-modal--define-command-key [remap self-insert-command]
                               'gpb-modal--not-defined)

(gpb-modal--define-command-key "0" 'digit-argument)
(gpb-modal--define-command-key "1" 'digit-argument)
(gpb-modal--define-command-key "2" 'digit-argument)
(gpb-modal--define-command-key "3" 'digit-argument)
(gpb-modal--define-command-key "4" 'digit-argument)
(gpb-modal--define-command-key "5" 'digit-argument)
(gpb-modal--define-command-key "6" 'digit-argument)
(gpb-modal--define-command-key "7" 'digit-argument)
(gpb-modal--define-command-key "8" 'digit-argument)
(gpb-modal--define-command-key "9" 'digit-argument)

(gpb-modal--define-command-key "i" 'gpb-modal--enter-insert-mode)
(gpb-modal--define-command-key "\M-i" 'gpb-modal--execute-one-command)
;; (gpb-modal--define-command-key "I" 'gpb-modal--enter-overwrite-mode)
;; (gpb-modal--define-command-key [(control ?\;)] 'gpb-modal--enter-insert-mode)

(gpb-modal--define-command-key "j" 'next-line)
(gpb-modal--define-command-key "k" 'previous-line)
(gpb-modal--define-command-key "h" 'backward-char)
(gpb-modal--define-command-key "l" 'forward-char)

(gpb-modal--define-command-key "t" 'isearch-forward)
(gpb-modal--define-command-key "T" 'isearch-backward)

(gpb-modal--define-command-key "a" 'repeat)
(gpb-modal--define-command-key "^" 'back-to-indentation)
(gpb-modal--define-command-key "%" 'gpb-modal--to-matching-delimiter)
(gpb-modal--define-command-key "J" 'scroll-up-command)
(gpb-modal--define-command-key "K" 'scroll-down-command)
(gpb-modal--define-command-key "$" 'end-of-line)

(gpb-modal--define-command-key "m" 'mark-text-object)
;; (gpb-modal--define-command-key "M" 'gpb-reg-reselect-region)
(gpb-modal--define-command-key "c" 'copy-text-object)
(gpb-modal--define-command-key "C" 'copy-append-text-object)

(gpb-modal--define-command-key "b" 'goto-beginning-of-text-object)
(gpb-modal--define-command-key "e" 'goto-end-of-text-object)
(gpb-modal--define-command-key "n" 'goto-next-text-object)
(gpb-modal--define-command-key "f" 'isearch-for-text-object)
(gpb-modal--define-command-key "F" 'isearch-backward)
;;(gpb-modal--define-command-key "f" 'goto-next-text-object)
(gpb-modal--define-command-key "=" 'indent-text-object)
(gpb-modal--define-command-key ";" 'comment/uncomment-text-object)
(gpb-modal--define-command-key ":" 'gpb-exec-command/eval-expression)

(gpb-modal--define-command-key "w" 'gpb-modal--next-word)
(gpb-modal--define-command-key "W" 'gpb-modal--beginning-of-word)
(gpb-modal--define-command-key "s" 'gpb-modal--next-symbol)
(gpb-modal--define-command-key "S" 'gpb-modal--beginning-of-symbol)
(gpb-modal--define-command-key " " 'gpb-modal--next-VIM-WORD)
(gpb-modal--define-command-key [(shift ?\ )] 'gpb-modal--beginning-of-VIM-WORD)
(gpb-modal--define-command-key "p" 'gpb-modal--next-paragraph)
(gpb-modal--define-command-key "P" 'gpb-modal--beginning-of-paragraph)
(gpb-modal--define-command-key "." 'gpb-modal--next-sentence)
(gpb-modal--define-command-key "," 'gpb-modal--beginning-of-sentence)

(gpb-modal--define-command-key "(" 'gpb-modal--beginning-of-parenthesis)
(gpb-modal--define-command-key ")" 'gpb-model--end-of-parenthesis)
(gpb-modal--define-command-key "[" 'gpb-modal--beginning-of-square-braces)
(gpb-modal--define-command-key "]" 'gpb-model--end-of-square-braces)
(gpb-modal--define-command-key "{" 'gpb-modal--beginning-of-curly-braces)
(gpb-modal--define-command-key "}" 'gpb-model--end-of-curly-braces)
(gpb-modal--define-command-key "<" 'gpb-modal--beginning-of-indented-block)
(gpb-modal--define-command-key ">" 'gpb-model--end-of-indented-block)

(gpb-modal--define-command-key "d" 'delete-text-object)
(gpb-modal--define-command-key "x" 'kill-text-object)
(gpb-modal--define-command-key "X" 'kill-and-append-text-object)
(gpb-modal--define-command-key "r" 'replace-text-object-with-clipboard)
(gpb-modal--define-command-key "E" 'execute-text-object)

(gpb-modal--define-command-key "z" 'undo)
(gpb-modal--define-command-key "u" 'undo)
(gpb-modal--define-command-key "v" 'yank)
(gpb-modal--define-command-key "V" 'gpb-modal--yank-after)
;; (gpb-modal--define-command-key "\t" 'indent-for-tab-command)

(gpb-modal--define-command-key [remap describe-mode] 'gpb-modal--describe-mode)


;; (gpb-modal--define-command-key "qp" 'fill-paragraph)
;; (gpb-modal--define-command-key "i" 'gpb-modal--enter-insert-mode)
;; (gpb-modal--define-command-key "I" 'gpb-modal--enter-overwrite-mode)


;; (defun gpb-modal--ensure-local-keymaps-are-defined ()
;;   "Ensure that the local keymaps have been initialized."
;;   (unless gpb-modal--command-mode-local-map
;;     (setq gpb-modal--command-mode-local-map (make-sparse-keymap))
;;     (set-keymap-parent gpb-modal--command-mode-local-map
;;                        gpb-modal--command-mode-global-map))
;;   (unless gpb-modal--insert-mode-local-map
;;     (setq gpb-modal--insert-mode-local-map (make-sparse-keymap))
;;     (set-keymap-parent gpb-modal--insert-mode-local-map
;;                        'gpb-modal--insert-mode-global-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  The main functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun gpb-modal--describe-mode ()
  "Disable modal keybindings and then call `describe-mode'."
  (interactive)
  (let ((gpb-modal--keymap-alist nil))
    (describe-mode)))


(defun gpb-modal--execute-one-command ()
  "Disable modal keymaps for one command and then turn them back on.\n
You can use this function to get access to keybindings that have
been hidden by the modal mode keybindings."
  (interactive)
  (let* ((cursor-type gpb-modal-insert-cursor-type)
         (gpb-modal--keymap-alist nil)
         (key-seq (read-key-sequence nil))
         (command (key-binding key-seq)))
    ;; `read-key-sequence' does not update `last-command-event'.  This
    ;; causes problems with, for example, `self-insert-command' which uses
    ;; `last-command-event'.
    (setq last-command-event last-input-event)
    (call-interactively command)))


(defun gpb-modal--enter-command-mode (&optional arg)
  (interactive "P")
  (setq gpb-modal--current-mode 'command
        gpb-modal--keymap-alist `((gpb-modal-mode
                                   . ,gpb-modal--command-mode-local-map))
        ;; cursor-type 'box
        ;; gpb-modal--cursor-type 'box
        ;; gpb-modal--pending-post-command-hook-command 'dont-change-mode
        gpb-modal--mode-changed t)
  (run-hooks 'gpb-modal--enter-command-mode-hook)
  (when (called-interactively-p)
    (unless (minibuffer-window-active-p (selected-window))
      (message "Entering command mode..."))))

(defun gpb-modal--enter-insert-mode (&optional arg)
  "Enter insert mode.\n
With a prefix argument, disable all modal keymaps for one command
and then reenter command mode."
  (interactive "P")
  ;; (gpb-modal--ensure-local-keymaps-are-defined)
  (if arg
      (gpb-modal--execute-one-command)
    (setq gpb-modal--current-mode 'insert
          gpb-modal--keymap-alist `((gpb-modal-mode
                                     . ,gpb-modal--insert-mode-local-map))
          ;; cursor-type 'bar gpb-modal--cursor-type 'bar
          ;; gpb-modal--pending-post-command-hook-command 'dont-change-mode
          gpb-modal--mode-changed t)
    (run-hooks 'gpb-modal--enter-insert-mode-hook)
    (when (called-interactively-p)
      (unless (minibuffer-window-active-p (selected-window))
        (message "Entering insert mode...")))))

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

(defun gpb-modal--next-VIM-WORD  (arg)
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

(defun gpb-modal--not-defined ()
  (interactive)
  (error "This key is not bound in command mode"))

(defun gpb-modal--pre-command-hook ()
  (remove-hook 'after-change-functions 'gpb-modal--show-fake-cursor t)
  (when gpb-modal-cursor-overlay
    (gpb-log-forms 'gpb-modal--pre-command-hook
                   'this-command
                   '(overlay-start gpb-modal-cursor-overlay)
                   '(overlay-end gpb-modal-cursor-overlay)
                   '(overlay-buffer gpb-modal-cursor-overlay)
                   '(overlay-get gpb-modal-cursor-overlay 'face)
                   '(overlay-get gpb-modal-cursor-overlay 'display)
                   '(overlay-get gpb-modal-cursor-overlay 'after-string))
    (delete-overlay gpb-modal-cursor-overlay)))

(defun gpb-modal--post-command-hook ()
  "See `edebug-enter' for more on interactions with edebug"
  (let* ((next-frame (selected-frame))
         (next-window (selected-window))
         (next-buffer (window-buffer next-window))
         (frame-changed (not (eq gpb-modal--previous-frame next-frame)))
         (window-changed (not (eq gpb-modal--previous-window next-window)))
         (buffer-changed (not (eq gpb-modal--previous-buffer next-buffer)))
         (continue t))
    (gpb-log-forms 'gpb-modal--post-command-hook
                   'this-command 'this-original-command
                   'edebug-active 'edebug-execution-mode
                   'edebug-stop 'arg-mode
                   'gpb-modal--previous-buffer 'gpb-modal--previous-window
                   'gpb-modal--previous-frame
                   '(current-buffer) 'next-buffer 'next-window 'next-frame
                   'frame-changed 'window-changed 'buffer-changed
                   ;; 'gpb-modal--pending-post-command-hook-command
                   'gpb-modal--mode-changed
                   ;;'gpb-modal--keymap-alist
                   )
    (unwind-protect
        (cond
         ;; If the mode has been changed by `gpb-modal--enter-command-mode'
         ;; or `gpb-modal--enter-insert-mode' during the last command, then
         ;; we don't change it again.
         (gpb-modal--mode-changed)

         ;; It seems more reliable to test `this-original-command'
         ;; than `this-command'.
         ((member this-original-command gpb-modal--enter-command-mode-after)
          (gpb-log-message 'gpb-modal--post-command-hook "case 1")
          (gpb-modal--enter-command-mode))

         ((member this-original-command gpb-modal--enter-insert-mode-after)
          (gpb-log-message 'gpb-modal--post-command-hook "case 2")
          (gpb-modal--enter-insert-mode))

         ((or buffer-changed window-changed frame-changed)
          (with-current-buffer next-buffer
            (cond
             ((gpb-modal--insert-mode-buffer-p)
              (gpb-log-message 'gpb-modal--post-command-hook "case 3")
              (gpb-modal--enter-insert-mode))
             (t
              (gpb-log-message 'gpb-modal--post-command-hook "case 4")
              (gpb-modal--enter-command-mode))))))

      (setq gpb-modal--previous-frame (selected-frame)
            gpb-modal--previous-window (selected-window)
            gpb-modal--previous-buffer (current-buffer)
            gpb-modal--mode-changed nil
            cursor-type t
            cursor-in-non-selected-windows gpb-modal-nonselected-cursor-type)

      (with-current-buffer next-buffer
        (setq cursor-type (case gpb-modal--current-mode
                            (command gpb-modal-command-cursor-type)
                            (insert gpb-modal-insert-cursor-type)
                            (t (error "Runtime error")))
              gpb-modal-cursor-marker (or gpb-modal-cursor-marker
                                          (make-marker)))
        (set-marker-insertion-type gpb-modal-cursor-marker t)
        (move-marker gpb-modal-cursor-marker (point) (current-buffer))
        (gpb-modal--show-fake-cursor)
        (gpb-log-forms 'gpb-modal--post-command-hook
                       'gpb-modal--current-mode 'cursor-type
                       'cursor-in-non-selected-windows
                       '(overlay-start gpb-modal-cursor-overlay)
                       '(overlay-end gpb-modal-cursor-overlay)
                       '(overlay-buffer gpb-modal-cursor-overlay)
                       '(overlay-get gpb-modal-cursor-overlay 'face)
                       '(overlay-get gpb-modal-cursor-overlay 'display)
                       '(overlay-get gpb-modal-cursor-overlay 'after-string))
        (add-hook 'after-change-functions 'gpb-modal--show-fake-cursor t t)))))

(defun gpb-modal--show-fake-cursor (&rest args)
  "Simulate a fake block cursor by adding text properties to the
character that follows `gpb-modal-cursor-marker'.  This function
is temporarily added to `after-change-functions' between commands
to keep the cursor updates when there are asynchronous insertions
into the buffer (e.g. comint-mode)."
  (let ((inhibit-modification-hooks t))
    (save-excursion
      (gpb-log-forms 'gpb-modal--show-fake-cursor
                     '(point) 'gpb-modal-cursor-marker
                     '(window-point))
      (goto-char gpb-modal-cursor-marker)
      (let* ((inhibit-modification-hooks t)
             (beg (point))
             (end (min (1+ beg) (save-restriction (widen) (point-max))))
             (ov (or (and gpb-modal-cursor-overlay
                          (move-overlay gpb-modal-cursor-overlay
                                        beg end (current-buffer)))
                     (make-overlay beg end (current-buffer))))
             (face-symbol (case gpb-modal--current-mode
                            (command 'gpb-modal-command-cursor-face)
                            (insert 'gpb-modal-insert-cursor-face)
                            (t (error "Runtime error"))))
             (eob-placeholder (propertize " " 'cursor t
                                          'face face-symbol))
             (eol-placeholder (concat (propertize " " 'cursor t
                                                  'face face-symbol)
                                      "\n")))
        (overlay-put ov 'face nil)
        (overlay-put ov 'display nil)
        (overlay-put ov 'after-string nil)
        (overlay-put ov 'window (selected-window))
        (overlay-put ov 'priority (1+ show-paren-priority))
        (overlay-put ov 'front-sticky t)
        (cond
         ((eobp) (overlay-put ov 'after-string eob-placeholder))
         ((eolp) (overlay-put ov 'display eol-placeholder))
         (t (overlay-put ov 'face face-symbol)))
        (setq gpb-modal-cursor-overlay ov)))))


;; (defun gpb-modal--update-display ()
;;   (save-excursion
;;     (ignore-errors (forward-char))
;;     (redisplay t)))


;; (defun gpb-modal--add-overlay ()
;;   (let ((ov gpb-modal-cursor-overlay)
;;         (face-symbol 'gpb-modal-command-cursor-face))
;;   (cond
;;    ((and (eq gpb-modal--current-mode 'command) (eobp))
;;     (setq ov (gpb-modal--make-or-move-overlay ov (point) (point))
;;           cursor-type nil)
;;     (overlay-put ov 'face nil)
;;     (overlay-put ov 'display nil)
;;     (overlay-put ov 'after-string
;;                  (propertize " " 'face face-symbol))
;;     (overlay-put ov 'window (selected-window)))

;;    ((eq gpb-modal--current-mode 'command)
;;     (setq ov (gpb-modal--make-or-move-overlay ov (point) (1+ (point)))
;;           cursor-type nil)
;;     (overlay-put ov 'face nil)
;;     (overlay-put ov 'display nil)
;;     (overlay-put ov 'after-string nil)
;;     (if (eolp)
;;         (overlay-put ov 'display (concat
;;                                   (propertize " " 'face face-symbol) "\n"))
;;       (overlay-put ov 'face face-symbol))
;;     (overlay-put ov 'window (selected-window))
;;     (overlay-put ov 'priority (1+ show-paren-priority))
;;     )

;;    (t
;;     (when ov
;;       (delete-overlay ov)
;;       (setq ov nil
;;             cursor-type t))))
;;   (setq gpb-modal-cursor-overlay ov)))


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
(gpb-tobj--global-set-key "\C-j" 'gpb-tobj--cancel-command)


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
    (gpb-log-forms 'edebug-mode 'post-command-hook '(current-buffer)
                   'source-buffer 'cursor-type)
    (unless (or (member 'gpb-modal--post-command-hook post-command-hook)
                (and (member t post-command-hook)
                     (member 'gpb-modal--post-command-hook
                             (default-value 'post-command-hook))))
      (add-to-list 'post-command-hook 'gpb-modal--post-command-hook))
    (gpb-log-forms 'edebug-mode 'post-command-hook '(current-buffer)
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
;;     (gpb-log-forms 'edebug-enter 'post-command-hook '(current-buffer)
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


;; Interacte nicely with commands issued by emacsclient outside of the
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


;; help-mode

(add-hook 'help-mode-hook 'gpb-modal--init-help-buffer)
(defun gpb-modal--init-help-buffer ()
  ;; (gpb-modal--define-command-key "b" nil t)
  ;; (gpb-modal--define-command-key "f" nil t)
  (gpb-modal--define-command-key "q" nil t))

(add-hook 'vc-dir-mode-hook 'gpb-modal--vc-dir-mode-hook)
(defun gpb-modal--vc-dir-mode-hook ()
  (gpb-modal--define-command-key "u" nil t)
  (gpb-modal--define-command-key "m" nil t)
  (gpb-modal--define-command-key "n" nil t)
  (gpb-modal--define-command-key "p" nil t)
  (gpb-modal--define-command-key "g" nil t)
  (gpb-modal--define-command-key "q" nil t))

(add-hook 'property-list-mode-hook 'gpb-modal--property-list-mode-hook)
(defun gpb-modal--property-list-mode-hook ()
  (gpb-modal--define-command-key "j" 'widget-forward t)
  (gpb-modal--define-command-key "l" 'widget-forward t)
  (gpb-modal--define-command-key "h" 'widget-backward t)
  (gpb-modal--define-command-key "k" 'widget-backward t)
  (gpb-modal--define-command-key " " nil t))

(add-hook 'Buffer-menu-mode-hook 'gpb-modal--Buffer-menu-mode-hook)
(defun gpb-modal--Buffer-menu-mode-hook ()
  (gpb-modal--define-command-key "d" nil t)
  (gpb-modal--define-command-key "q" nil t)
  (gpb-modal--define-command-key "x" nil t))


(eval-after-load 'eldoc
  '(eldoc-add-command-completions "gpb-modal--next-"
                                  "gpb-modal--beginning-of-"
                                  "gpb-model--end-of-"))

(provide 'gpb-modal)
