;; Some values must be set before `evil' is loaded.  You would think you
;; could use :init for this below, but it doesn't seem to work.
(setq evil-want-keybinding nil
      evil-want-integration t
      evil-respect-visual-line-mode t
      evil-want-Y-yank-to-eol t)

(require 'evil)
(require 'evil-collection)
(require 'evil-goggles)
(require 'evil-surround)

(evil-mode 1)
(evil-goggles-mode 1)
(global-evil-surround-mode 1)

(setq ;; evil-move-beyond-eol t
      ;; evil-want-C-u-scroll t
      ;; evil-want-C-d-scroll t
      ;; evil-want-minibuffer t
      ;; evil-move-cursor-back t
      evil-cross-lines t
      evil-visual-state-cursor 'hollow
      evil-emacs-state-cursor 'bar
      evil-shift-width 2
      evil-kill-on-visual-paste nil)

(evil-select-search-module 'evil-select-search-mode 'evil-search)
(evil-set-undo-system 'undo-redo)

;; Normal state
(evil-define-key 'normal 'global "\C-b" 'switch-to-buffer)
(evil-define-key 'normal 'global "\C-z" 'evil-undo)
(evil-define-key 'normal 'global "\C-g" 'gpb-evil-keyboard-quit)
(evil-define-key 'normal 'global (kbd "TAB") 'indent-for-tab-command)

;; I get paste on \C-v with VIM in the terminal.
(evil-define-key 'normal 'global "\C-v" 'yank)
(evil-define-key 'normal 'global [remap yank-pop] nil)
(evil-define-key 'normal 'global "\M-v" 'yank-pop)

(evil-define-key 'normal 'global "H" 'gpb-backward-page)
(evil-define-key 'normal 'global "L" 'gpb-forward-page)

;; Insert state
(evil-define-key 'insert 'global "\C-h" 'backward-delete-char-untabify)
(evil-define-key 'insert 'global "\C-v" 'yank)
(evil-define-key 'insert 'global "\M-v" 'yank-pop)
(evil-define-key 'insert 'completion-in-region-mode "\C-g" 'keyboard-quit)

;; Undo region when in visual state.  We have advised `undo' and `redo' to
;; preserve the mark elsewhere.
(evil-define-key 'visual 'global "\C-z" 'undo)
(evil-define-key 'visual 'global "\C-R" 'redo)
(evil-define-key 'visual 'global "\C-v" 'yank)

;; Motion state
(evil-define-key 'motion 'global (kbd "C-w" ) nil)
(evil-define-key 'motion 'global (kbd "RET" ) nil)
; Move `evil-jump-forward' off of TAB
(evil-define-key 'motion 'global (kbd "TAB") nil)
(evil-define-key 'motion 'global (kbd "M-o") 'evil-jump-forward)

;; Press \ twice to stay in emacs state.  Then C-g to leave.
(evil-define-key 'emacs 'global "\\" 'evil-emacs-state)
(evil-define-key 'emacs 'global "\C-g" 'evil-normal-state)

;; Never start in insert or emacs state.
(setq evil-insert-state-modes nil
      evil-emacs-state-modes nil)

;; Free up some bindings
(setq evil-collection-key-blacklist '("C-w" "RET" "C-b"))
(evil-collection-init)

;; Make some commands repeatable with one keypress.
(with-eval-after-load 'gpb-util
  (gpb-make-repeatable #'evil-execute-macro
                       #'evil-forward-section-begin
                       #'evil-backward-section-begin
                       #'evil-collection-unimpaired-insert-newline-above
                       #'evil-collection-unimpaired-insert-newline-below))


;; Define a `defun' text object.

(evil-define-text-object evil-a-defun (count &optional beg end type)
  "Select a defun."
  ;; `evil-select-an-object; doesn't seem to work, so we use
  ;; `evil-select-inner-object' and then append any trailing blank lines.
  (cl-multiple-value-bind (beg end)
      (evil-select-inner-object 'defun beg end type count)
    (save-excursion
      (goto-char end)
      (skip-chars-forward " \n\t")
      (if (eobp)
          ;; There was no trailing whitespace, so prepend any initial
          ;; whitespace.
          (save-excursion
            (goto-char beg)
            (unless (bobp)
              (skip-chars-backward " \n\t")
              (forward-line 1))
            (list (point) end))
        (list beg (point))))))

(evil-define-text-object evil-inner-defun (count &optional beg end type)
  "Select inner defun."
  (evil-select-inner-object 'defun beg end type count))

(define-key evil-outer-text-objects-map "d" 'evil-a-defun)
(define-key evil-inner-text-objects-map "d" 'evil-inner-defun)

(defun gpb-evil-keyboard-quit ()
  (interactive)
  (evil-ex-nohighlight)
  (keyboard-quit))


;; elisp-mode

(evil-define-key 'normal emacs-lisp-mode-map "gr" 'revert-buffer)


;; Start a search immediately from visual mode.

(defun gpb-search-visual-selection (&rest args)
  (interactive)
  (when (eq evil-state 'visual)
    (let* ((txt (buffer-substring-no-properties
                 evil-visual-beginning evil-visual-end))
           (cmd (concat (this-command-keys) txt)))
      ;; (message "Command: %S" cmd)
      (evil-normal-state)
      (execute-kbd-macro cmd))))

(evil-define-key 'visual 'global "?" 'gpb-search-visual-selection)
(evil-define-key 'visual 'global "/" 'gpb-search-visual-selection)

;; Remap / to F in the package menu.
(defun gpb-package-menu-mode-setup ()
  (evil-define-key 'normal 'package-menu-mode-map "F"
    (lookup-key package-menu-mode-map "/")))

(add-hook 'package-menu-mode-hook #'gpb-package-menu-mode-setup)

;; Configure `prat' bindings.

(with-eval-after-load 'prat (require 'prat-evil))


;; Associate a default text object with SPC

(evil-define-key 'operator 'global " " #'evil-inner-symbol)
(evil-define-key 'visual   'global " " #'evil-inner-symbol)

(defun gpb-set-default-text-object (obj)
  (evil-define-key 'operator 'local " " obj)
  (evil-define-key 'visual 'local " " obj))


;; Give programming modes an "!" operator.

(defvar-local gpb-eval-code-function nil
  "A function accepting (BEG END).
This function will be called by the operator ! in programming modes.")

(evil-define-operator gpb-eval-code-operator (count beg end)
  "Evaluate code hunk in range BEG END"
  :repeat nil
  :keep-visual nil
  ;; :line 'block
  (interactive "<c><r>")
  (message "count: %S" count)
  (unless (or (null count) (= count 1))
    (error "`count' must be 1"))
  (unless (and (boundp 'gpb-eval-code-function)
               gpb-eval-code-function)
    (error "`gpb-eval-code-function' is not defined"))
  (funcall gpb-eval-code-function beg end))

(defun gpb-define-eval-code-operator (f &optional key)
  (let ((key (or key "!")))
    (setq-local gpb-eval-code-function f)
    (evil-define-key 'normal 'local key #'gpb-eval-code-operator)
    (evil-define-key 'visual 'local key #'gpb-eval-code-operator)))

(add-hook 'emacs-lisp-mode-hook
          (lambda () (gpb-define-eval-code-operator #'eval-region)))


(provide 'gpb-evil)
