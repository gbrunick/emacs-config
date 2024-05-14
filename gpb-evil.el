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
(require 'evil-visualstar)

;; (require 'better-jumper)

(evil-mode 1)
(evil-goggles-mode 1)
(global-evil-visualstar-mode t)

;; (better-jumper-mode 1)

(setq evil-visual-state-cursor 'hollow
      evil-emacs-state-cursor 'hollow)

(evil-select-search-module 'evil-select-search-mode 'evil-search)

;; Normal state
;; (evil-define-key 'normal 'fundamental-mode "\C-m" 'newline)
(evil-define-key 'normal 'global "\C-z" 'evil-undo)
(evil-define-key 'normal 'global "\M-u" 'universal-argument)
(evil-define-key 'normal 'global "\C-g" 'gpb-evil-keyboard-quit)

;; Insert state
(evil-define-key 'insert 'global "\C-h" 'backward-delete-char-untabify)
(evil-define-key 'insert 'global "\C-v" 'yank)
(evil-define-key 'insert 'global "\M-v" 'yank-pop)
(evil-define-key 'insert 'completion-in-region-mode "\C-g" 'keyboard-quit)

;; Undo region when in visual state.  We don't want `evil-undo' here
;; because it disables the selection.
(evil-define-key 'visual 'global "\C-z" 'undo)

;; Allow [y|d|c]io to be abbreviated as [y|d|c]o.
;; https://github.com/noctuid/general.el?tab=readme-ov-file#mapping-under-non-prefix-keys
(evil-define-key 'operator 'global "o" 'evil-inner-symbol)
(evil-define-key 'visual 'global "o" 'evil-inner-symbol)

;; Press \ twice to stay in emacs state.  Then C-Z to leave.
(evil-define-key 'emacs 'global "\\" 'evil-emacs-state)
(evil-define-key 'emacs 'global "\C-g" 'evil-normal-state)
  
;; (evil-define-key 'motion 'global "\C-o" 'better-jumper-jump-backward)
;; (evil-define-key 'motion 'global "\M-o" 'better-jumper-jump-forward)

;; Free up some bindings
(defvar gpb-evil-key-blacklist
  ;; '("C-w" "C-e" "RET" "!" "C-i" "<tab>")
  '("C-w" "RET" "C-i" "<tab>")
  "Don't let `evil' conflict with these bindings.")

(setq evil-collection-key-blacklist gpb-evil-key-blacklist)

(dolist (keydef gpb-evil-key-blacklist)
  (define-key evil-motion-state-map (kbd keydef) nil))

(evil-collection-init)

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

;; https://github.com/emacs-evil/evil/issues/874
(add-hook 'edebug-mode-hook 'evil-normalize-keymaps)

(setq evil-emacs-state-modes
      (delq 'completion-list-mode evil-emacs-state-modes))

(defun gpb-evil-keyboard-quit ()
  (interactive)
  (evil-ex-nohighlight)
  (keyboard-quit))

(with-eval-after-load 'prat (require 'prat-evil))

(provide 'gpb-evil)
