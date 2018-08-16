;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Code related to interacting with the active region (selection)
;;   Includes cut and paste related customization
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'mule-util) ;; for `truncate-string-to-width'
(ignore-errors (require 'gpb-context-menu))

(defvar gpb-reg--default-shift-size 4)

(define-minor-mode gpb-region-keymap-mode
  "A global minor mode that enables a keymap when the region is active.

Use `gpb-reg--define-key' to define key bindings in the keymap
that becomes activate when the region is activated.  The keymap
is placed in `emulation-mode-map-alists' so it will generally all
minor mode keymaps.

The following key bindings are bound in the current buffer when
the region becomes active:

\\{gpb-reg--when-mark-active-local-map}
The following key bindings are bound in every buffer when
the region becomes active:

\\{gpb-reg--when-mark-active-global-map}"
  :global t
  :init-value nil
  (cond
   (gpb-region-keymap-mode
    ;; Add the appropriate keymap to `emulation-mode-map-alists'
    (add-to-list 'emulation-mode-map-alists 'gpb-reg--emulation-mode-map-alist)
    ;; Setup hooks to track the state of the mark
    (add-hook 'activate-mark-hook 'gpb-reg--update-mark-active)
    (add-hook 'deactivate-mark-hook 'gpb-reg--update-mark-active)
    (add-hook 'minibuffer-setup-hook 'gpb-reg--update-mark-active)
    (add-hook 'minibuffer-exit-hook 'gpb-reg--update-mark-active)
    ;; Show a lighter when the region keymap is active
    (add-to-list 'minor-mode-alist '(gpb-reg--mark-active " Reg")))
   (t
    (setq emulation-mode-map-alists
          (delq 'gpb-reg--emulation-mode-map-alist emulation-mode-map-alists))
    (remove-hook 'activate-mark-hook 'gpb-reg--update-mark-active)
    (remove-hook 'deactivate-mark-hook 'gpb-reg--update-mark-active)
    (remove-hook 'minibuffer-setup-hook 'gpb-reg--update-mark-active)
    (remove-hook 'minibuffer-exit-hook 'gpb-reg--update-mark-active)
    (remove 'minor-mode-alist 'gpb-reg--mark-active))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Setup the implementation variables
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar gpb-reg--mark-active nil
  "The `gpb-reg--mark-active' minor mode generally tracks
`mark-active' but is slightly different.  In some situations, we
want to disable to the `gpb-reg--when-mark-active-global-map'
keymap so that we can access the keybindings (like \\C-x and
\\C-c) that are are hidden behind the region keymap.")
(make-variable-buffer-local 'gpb-reg--mark-active)

(defvar gpb-reg--when-mark-active-local-map 'no-local-keybindings
  "The buffer local keymap that is active when the mark is active.

Bindings should never be added to this keymap directly by the
user.  Instead, use the function `gpb-reg--define-key' which
correctly handles the management of buffer local variables.")

(defvar gpb-reg--when-mark-active-global-map (make-sparse-keymap)
  "The global keymap that is active when the mark is active.")

;; You must define this variable after `gpb-reg--when-mark-active-local-map'
(defvar gpb-reg--emulation-mode-map-alist
  `((gpb-reg--mark-active . ,gpb-reg--when-mark-active-global-map))
  "Contains the local and global region keymaps.

This symbol is added to `emulation-mode-map-alists' when
`gpb-region-keymap-mode' is enabled.  The function
`gpb-reg--define-key' makes this variable buffer local when the
first local region keybinding is defined in a given buffer.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Various region-related functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun gpb-reg--align-region ()
  "Seems to be broken"
  (interactive)
  (when (region-active-p)
    (setq current-prefix-arg '(4))
    (call-interactively 'align)
    ;; (indent-region (region-beginning) (region-end))
    (goto-char (region-end))
    (setq deactivate-mark t)))


(defun gpb-reg--align-region-1 (beg end)
  (interactive "r")
  (gpb-align-region-1 beg end "\\s +" " ")
  (deactivate-mark))


(defun gpb-reg--align-region-2 (beg end sep-regex sep-string)
  (interactive "r")
  (let ((lines (mapcar (lambda (x)
                         (gpb-util-split-string
                          (gpb-util-strip-string x) sep-regex))
                       (gpb-util-split-string
                        (gpb-util-strip-string
                         (buffer-substring-no-properties beg end)) "\n")))
        (col-widths (make-vector 1000 0))
        i indent)
    (dolist (l lines)
      (setq i 0)
      (dolist (part l)
        (aset col-widths i (max (elt col-widths i) (length part)))
        (incf i)))
    (delete-region beg end)
    (dolist (l lines)
      (setq i 0)
      (if (eq (point) beg)
          (setq indent (current-column))
        (insert (make-string indent ?\ )))
      (dolist (part l)
        (unless (string-equal part "")
          (insert part)
          (insert (make-string (- (elt col-widths i) (length part)) ?\ ))
          (insert sep-string)
          (incf i)))
      (insert "\n"))
    ;; (backward-delete-char 1)
    ))


(defun gpb-reg--buffer-is-narrowed-p (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (let ((min (point-min))
          (max (point-max)))
      (save-restriction
        (widen)
        (not (and (eq min (point-min)) (eq max (point-max))))))))


(defun gpb-reg--copy-region (begin end arg)
  "With a prefix argument append to current killed text."
  (interactive "r\nP" )
  (let ((last-command (if arg 'kill-region last-command))
        (str (buffer-substring-no-properties begin end)))
    (kill-ring-save begin end)
    ;; If the message is printed first, it screws up cuts
    ;; from the message buffer.
    (message "Copied%s: \"%s\""
             (if arg " and appended" "")
             (truncate-string-to-width
              (replace-regexp-in-string "\n" "\\\\n" str) 60))))


(defun gpb-reg--define-key (key command &optional local)
  (when (and local (not (local-variable-p 'gpb-reg--when-mark-active-local-map)))
    (set (make-variable-buffer-local 'gpb-reg--when-mark-active-local-map)
         (make-sparse-keymap))
    (set (make-variable-buffer-local 'gpb-reg--emulation-mode-map-alist)
      `((gpb-reg--mark-active . ,gpb-reg--when-mark-active-local-map)
        (gpb-reg--mark-active . ,gpb-reg--when-mark-active-global-map))))
  (if local
      (define-key gpb-reg--when-mark-active-local-map key command))
  (define-key gpb-reg--when-mark-active-global-map key command))


(defun gpb-reg--delete-region-or-next-char (beg end)
  (interactive "r")
  (if (use-region-p) (delete-region beg end)
    (deactivate-mark)
    (delete-char 1)))

(defun gpb-reg--delete-region-or-previous-char (beg end)
  (interactive "r")
  (if (use-region-p) (delete-region beg end)
    (deactivate-mark)
    (delete-backward-char 1)))


(defun gpb-reg--disable-when-mark-active-mode ()
  (interactive)
  (setq gpb-reg--mark-active nil)
  (force-mode-line-update))


(defun gpb-reg--ensure-region-is-full-lines ()
  (interactive)
  (let ((p (point)) deactivate-mark)
    (if (equal p (region-beginning))
        ;; Point at start of region
        (progn
          (goto-char (region-end))
          (unless (bolp) (forward-line 1) (set-mark (point)))
          (goto-char p)
          (forward-line 0))
      ;; Point at end of region
      (goto-char (region-beginning))
      (forward-line 0)
      (set-mark (point))
      (goto-char p)
      (unless (bolp) (forward-line 1)))))


(defun gpb-reg--get-synonyms (beg end)
  "This doesn't work"
  (interactive "r")
  (require 'synonyms)
  (setq synonyms-file        "~/usr-common/docs/thesaurus/mthesaur.txt")
  (setq synonyms-cache-file  "~/.synonyms-cache")
  (synonyms nil (buffer-substring-no-properties beg end)))


(defun gpb-reg--indent-region ()
  (interactive)
  (when (region-active-p)
    (save-excursion
      (indent-region (region-beginning) (region-end))
      (goto-char (region-end)))
    (setq deactivate-mark t)))


(defun gpb-reg--ispell-region ()
  (interactive)
  (when (region-active-p)
    (ispell-region (region-beginning) (region-end))
    (setq deactivate-mark t)))


(defun gpb-reg--kill-region (begin end arg)
  "With prefix argument append to current killed text."
  (interactive "r\nP" )
  (let ((last-command (if arg 'kill-region last-command))
        (str (buffer-substring-no-properties begin end)))
    (kill-region begin end)
    ;; If the message is printed first, it screws up
    ;; cuts from the message buffer.
    (message "Cut%s: \"%s\""
             (if arg " and appended" "")
             (truncate-string-to-width
              (replace-regexp-in-string "\n" "\\\\n" str) 60))))


(defun gpb-reg--narrow-to-region (beg end arg)
  (interactive "r\nP")
  (deactivate-mark)
  (with-current-buffer (clone-indirect-buffer nil nil)
    (narrow-to-region beg end)
    (if arg (switch-to-buffer-other-window (current-buffer)))
    (switch-to-buffer (current-buffer))))


(defun gpb-reg--not-defined ()
  (interactive)
  (error "This key is not bound when the region is active"))


(defun gpb-reg--query-replace ()
  (interactive)
  (let* ((from (buffer-substring (region-beginning) (region-end)))
         (to (query-replace-read-to from "Replace all occurrences of" nil)))
    (deactivate-mark)
    (goto-char (point-min))
    (perform-replace from to t nil nil)))


(defun gpb-reg--region-is-commented-p ()
  "Is the region all comments"
  ;; If you make this interactive it cancels the mark
  (when (use-region-p)
    (comment-normalize-vars)
    (save-excursion
      (let ((beg (region-beginning))
            (end (region-end)))
        (goto-char beg)
        (comment-forward (point-max))
        (<= end (point))))))


(defun gpb-reg--shift-region-left (arg)
  (interactive "p")
  (gpb-reg--ensure-region-is-full-lines)
  (let ((beg (region-beginning))
        (end (region-end))
        (left-margin arg))
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (when (not (looking-at "[[:space:]]*$"))
          (setq left-margin (min left-margin (current-indentation))))
        (forward-line)))
    (when (<= left-margin 0)
      (setq deactivate-mark nil mark-active t)
      (error "No room to shift region left."))
    (indent-rigidly beg end (- left-margin))
    (setq deactivate-mark nil mark-active t)))


(defun gpb-reg--shift-region-right (arg)
  (interactive "p")
  (gpb-reg--ensure-region-is-full-lines)
  (indent-rigidly (region-beginning) (region-end) arg)
  (setq deactivate-mark nil mark-active t))


(defun gpb-reg--update-mark-active ()
  (setq gpb-reg--mark-active mark-active)
  ;; The following breaks things
  (force-mode-line-update))


(defvar gpb-reg--save-before-wrap "(")
(defvar gpb-reg--save-after-wrap ")")

(defun gpb-reg--wrap-region (before after)
  (interactive "sInsert before: \nsInsert after: ")
  (assert (use-region-p))
  (setq gpb-reg--save-before-wrap before
        gpb-reg--save-after-wrap after)
  (let ((beg (region-beginning)) (end (region-end)))
    (goto-char end)
    (insert after)
    (goto-char beg)
    (insert before)))

(defun gpb-reg--wrap-region-again ()
  (interactive)
  (assert (use-region-p))
  (let ((beg (region-beginning)) (end (region-end)))
    (goto-char end)
    (insert gpb-reg--save-after-wrap)
    (goto-char beg)
    (insert gpb-reg--save-before-wrap)))


(defmacro gpb-reg--make-wrapper (before after name &optional binding)
  (let ((func-name (intern (concat "gpb-reg--" name "-wrapper")))
        (binding (or binding (substring-no-properties before 0 1))))
    `(progn
       (defun ,func-name ()
         (interactive)
         (let ((pt (make-marker))
               (beg (region-beginning))
               (end (region-end)))
           (set-marker pt (point))
           ;; If we are the end, move past the wrapper
           (when (= pt end)
             (set-marker-insertion-type pt t))
           (goto-char end)
           (insert ,after)
           (goto-char beg)
           (insert ,before)
           (goto-char pt)
           (setq pt nil)
           ))
       (define-key (symbol-function 'gpb-reg--when-mark-active-global-map)
         ,(concat "\C-w" binding) ',func-name))))

;; (gpb-reg--make-wrapper "(" ")" "paren")
;; (gpb-reg--make-wrapper "[" "]" "square-brace")
;; (gpb-reg--make-wrapper "{" "}" "curly-brace")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Setup the default global keymap
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; CUA bindings and a way to cancel them
(gpb-reg--define-key "\C-x" 'gpb-reg--kill-region)
(gpb-reg--define-key "\C-c" 'gpb-reg--copy-region)
(gpb-reg--define-key "\C-q" 'gpb-reg--disable-when-mark-active-mode)

;; Delete the region when the use does anything delete-ish
(gpb-reg--define-key [backspace] 'gpb-reg--delete-region-or-previous-char)
(gpb-reg--define-key [remap delete-char] 'gpb-reg--delete-region-or-next-char)
(gpb-reg--define-key [remap backward-delete-char] 'gpb-reg--delete-region-or-previous-char)
(gpb-reg--define-key [remap delete-backward-char] 'gpb-reg--delete-region-or-previous-char)
(gpb-reg--define-key [remap backward-delete-char-untabify] 'gpb-reg--delete-region-or-previous-char)
(gpb-reg--define-key [remap c-electric-delete-forward] 'gpb-reg--delete-region-or-next-char)

;; Useful commands that act on the region
(gpb-reg--define-key [(?<)] 'gpb-reg--shift-region-left)
(gpb-reg--define-key [(?>)] 'gpb-reg--shift-region-right)
;; (gpb-reg--define-key "\C-m" 'gpb-isearch-forward)
(gpb-reg--define-key [(shift return)] 'gpb-isearch-backward)
(gpb-reg--define-key [(control n)] 'gpb-reg--narrow-to-region)
(gpb-reg--define-key "\M-$" 'gpb-reg--ispell-region)
(gpb-reg--define-key [(meta s)] 'gpb-reg--get-synonyms)
;; (gpb-reg--define-key [(meta \;)] 'comment-or-uncomment-region)
(gpb-reg--define-key [(tab)] 'indent-for-tab-command)
(gpb-reg--define-key "\C-o" 'exchange-point-and-mark)
(gpb-reg--define-key [(control w)] 'gpb-reg--wrap-region)
(gpb-reg--define-key [(meta w)] 'gpb-reg--wrap-region-again)
(gpb-reg--define-key [(meta u)] 'upcase-region)
(gpb-reg--define-key [(meta l)] 'downcase-region)
(gpb-reg--define-key [(meta c)] 'capitalize-region)
;; (gpb-reg--define-key [(meta \%)] 'gpb-reg--query-replace)
;;  In the interest of safety (and sanity)
(gpb-reg--define-key [remap self-insert-command] 'gpb-reg--not-defined)


(provide 'gpb-region)
