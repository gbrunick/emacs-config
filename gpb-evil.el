;; Some values must be set before `evil' is loaded.  You would think you
;; could use :init for this below, but it doesn't seem to work.
(setq evil-want-keybinding nil
      evil-want-integration t
      evil-move-beyond-eol nil
      evil-want-C-i-jump nil
      evil-want-C-u-scroll t
      evil-want-C-d-scroll t 
      evil-want-Y-yank-to-eol t
      ;; evil-want-minibuffer t
      evil-respect-visual-line-mode t
      evil-move-cursor-back nil
      evil-cross-lines t)

(require 'evil)
(require 'evil-collection)
(require 'evil-goggles)
(require 'evil-surround)

(evil-mode 1)
(evil-goggles-mode 1)
(evil-surround-mode 1)

(setq evil-visual-state-cursor 'hollow
      evil-emacs-state-cursor 'hollow)

(evil-select-search-module 'evil-select-search-mode 'evil-search)
(evil-set-undo-system 'undo-redo)

;; Normal state
(evil-define-key 'normal 'global "\C-z" 'evil-undo)
(evil-define-key 'normal 'global "\M-u" 'universal-argument)
(evil-define-key 'normal 'global "\C-g" 'gpb-evil-keyboard-quit)

;; Insert state
(evil-define-key 'insert 'global "\C-h" 'backward-delete-char-untabify)
(evil-define-key 'insert 'global "\C-v" 'yank)
(evil-define-key 'insert 'global "\M-v" 'yank-pop)
(evil-define-key 'insert 'completion-in-region-mode "\C-g" 'keyboard-quit)

;; Undo region when in visual state.  We have advised `undo' and `redo' to
;; preserve the mark elsewhere.
(evil-define-key 'visual 'global "\C-z" 'undo)
(evil-define-key 'visual 'global "\C-R" 'redo)

;; Allow [y|d|c]io to be abbreviated as [y|d|c]o.
;; https://github.com/noctuid/general.el?tab=readme-ov-file#mapping-under-non-prefix-keys
(evil-define-key 'operator 'global "o" 'evil-inner-symbol)
(evil-define-key 'visual 'global "o" 'evil-inner-symbol)

;; Press \ twice to stay in emacs state.  Then C-Z to leave.
(evil-define-key 'emacs 'global "\\" 'evil-emacs-state)
(evil-define-key 'emacs 'global "\C-g" 'evil-normal-state)
  
;; Free up some bindings
(defvar gpb-evil-key-blacklist
  ;; '("C-w" "C-e" "RET" "!" "C-i" "<tab>")
  '("C-w" "RET") ;; "C-i" "<tab>")
  "Don't let `evil' conflict with these bindings.")

(setq evil-collection-key-blacklist gpb-evil-key-blacklist)

(dolist (keydef gpb-evil-key-blacklist)
  (define-key evil-motion-state-map (kbd keydef) nil))

(evil-collection-init)

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
     
(evil-set-initial-state 'completion-list-mode 'normal) 

(defun gpb-evil-keyboard-quit ()
  (interactive)
  (evil-ex-nohighlight)
  (keyboard-quit))


;; Reset to the initial state when you enter a window.

(defun gpb-window-selection-change-function (&optional frame)
  "Return to the initial mode when you switch into a buffer."
  (evil-change-to-initial-state))

(add-hook 'window-selection-change-functions
          'gpb-window-selection-change-function)


;; Start a search immediately from visual mode.

(defun gpb-search-visual-selection (&rest args)
  (interactive)
  (when (eq evil-state 'visual)
    (let* ((txt (buffer-substring-no-properties
                 evil-visual-beginning evil-visual-end))
           (cmd (concat (this-command-keys) txt "\n")))
      ;; (message "Command: %S" cmd)
      (evil-normal-state)
      (execute-kbd-macro cmd))))

(evil-define-key 'visual 'global "?" 'gpb-search-visual-selection)
(evil-define-key 'visual 'global "/" 'gpb-search-visual-selection)

;; Configure `prat' bindings.

(with-eval-after-load 'prat (require 'prat-evil))

(provide 'gpb-evil)
