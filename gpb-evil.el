;; Some values must be set before `evil' is loaded.  You would think you
;; could use :init for this below, but it doesn't seem to work.
(setq evil-want-keybinding nil
      evil-want-integration t
      EVIL-move-beyond-eol nil
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

;; (require 'better-jumper)

(evil-mode 1)
(evil-goggles-mode 1)
;; (better-jumper-mode 1)

(setq evil-visual-state-cursor 'hollow
      evil-emacs-state-cursor 'hollow)

;; Normal state
(evil-define-key 'normal 'fundamental-mode "\C-m" 'newline)
(evil-define-key 'normal 'global "\C-z" 'evil-undo)
(evil-define-key 'normal 'global "\M-u" 'universal-argument)
;; "better" scrolling
(evil-define-key 'normal 'global "\C-d" 'gpb-forward-page-1)
(evil-define-key 'normal 'global "\C-u" 'gpb-backward-page-1)

;; (evil-define-key 'normal 'global " " 'self-insert-command)
;; (evil-define-key 'normal 'global " " (lambda ()
;;                                        (interactive)
;;                                        (insert-char ?\ )))

;; Insert state
(evil-define-key 'insert 'global "\C-h" 'backward-delete-char-untabify)
(evil-define-key 'insert 'global "\C-v" 'yank)
(evil-define-key 'insert 'global "\M-v" 'yank-pop)
(evil-define-key 'insert 'completion-in-region-mode "\C-g" 'keyboard-quit)

;; Allow [y|d|c]io to be abbreviated as [y|d|c]o.
;; https://github.com/noctuid/general.el?tab=readme-ov-file#mapping-under-non-prefix-keys
(evil-define-key 'operator 'global "o" 'evil-inner-symbol)
(evil-define-key 'visual 'global "o" 'evil-inner-symbol)

;; Press \ twice to stay in emacs state.  Then C-Z to leave.
(evil-define-key 'emacs 'global "\\" 'evil-emacs-state)

;; If you `isearch' when there is a visual selection, use that selection.
(evil-define-key 'visual 'global "\C-f"
  (lambda () (interactive)
    (let ((search-string (buffer-substring
                          (evil-range-beginning (evil-visual-range))
                          (evil-range-end (evil-visual-range)))))
      (evil-force-normal-state)
      (isearch-forward nil t)
      (isearch-yank-string search-string)
      (isearch-search-and-update))))

;; (evil-define-key 'motion 'global "\C-o" 'better-jumper-jump-backward)
;; (evil-define-key 'motion 'global "\M-o" 'better-jumper-jump-forward)

;; Free up some CUA bindings
(defvar gpb-evil-key-blacklist
  '("C-f" "C-b" "C-w" "C-e" "RET" "!" "C-i" "<tab>")
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

;; Evil binds "\C-g" in `evil-command-line-map' to `abort-minibuffers', but
;; this function is not defined?
;; (define-key evil-command-line-map "\C-g" 'abort-recursive-edit)
;; (define-key evil-command-line-map "\C-h" 'evil-ex-delete-backward-char)
;; (define-key evil-command-line-map "\M-h" 'backward-kill-word)

;; (evil-define-key 'insert 'global "\C-g" 'gpb-evil-insert-ctrl-g)
;;
;; (defun gpb-evil-insert-ctrl-g ()
;;   (interactive)
;;   (cond
;;    ((> (minibuffer-depth) 0)
;;     (abort-reththiscursive-edit))
;;    (t
;;     (evil-force-normal-state))))

(with-eval-after-load 'prat (require 'prat-evil))

(provide 'gpb-evil)
