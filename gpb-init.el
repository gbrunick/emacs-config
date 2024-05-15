;; We set this so we can debug a slow init process.
(setq debug-on-quit t)

;; Add the source tree rooted at the directory containing this file to the
;; load path.
(let* ((default-directory (file-name-directory
                           (or load-file-name (buffer-file-name)))))
  ;; Add current directory to load path without
  (add-to-list 'load-path (expand-file-name "."))
  ;; Add most subdirectories to `load-path'.  The directories
  ;; that contain files named .nosearch are ignored. 
  ;; `normal-top-level-add-subdirs-to-load-path').
  (normal-top-level-add-subdirs-to-load-path))

(require 'gpb-packages)
(gpb-ensure-packages-installed)
(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(use-package wgrep
  :config (setq wgrep-auto-save-buffer t)) 

(use-package dired
  :hook (dired-mode . dired-omit-mode)
  :bind ("M-o" . dired-omit-mode))

(use-package gpb-index
  :bind (("\C-ci" . 'gpb-show-index-buffer)))

(show-paren-mode 1)
(tool-bar-mode -1)
(menu-bar-mode 1)
(line-number-mode 1)
(column-number-mode 1)
(blink-cursor-mode 1)
(transient-mark-mode 1)

;; Control-c bindings
(global-set-key "\C-cn" 'gpb-new-document)
(global-set-key "\C-c$" 'ispell)
(global-set-key "\C-cg" 'gpb-grep)
        
;; Global, largely CUA-ish bindings
(global-set-key "\M-b" 'switch-to-buffer)
;; (global-set-key "\C-f" 'isearch-forward)
(global-set-key "\C-w" 'bury-buffer)
(global-set-key "\C-s" 'save-buffer)
;; (global-set-key "\C-v" 'yank)
;; (global-set-key "\M-v" 'yank-pop)
(global-set-key "\C-g" 'gpb-keyboard-quit)
(global-set-key [(control shift s)] 'write-file)
(global-set-key "\M-r" '(lambda ()
                          (interactive)
                          (revert-buffer nil t)
                          (message "Reverted %s" (current-buffer))))
(global-set-key [(control tab)] 'gpb-next-window)
(global-set-key [(control shift iso-lefttab)] 'gpb-previous-window)
(global-set-key [(control shift tab)] 'gpb-previous-window)
(global-set-key [remap backward-delete-char]
                'backward-delete-char-untabify)

(setq history-delete-duplicates         t
      comint-input-ignoredups           t
      delete-trailing-lines             t
      use-file-dialog                   nil
      print-escape-newlines             t
      mark-even-if-inactive             nil
      select-enable-clipboard           t
      inhibit-startup-screen            t
      icon-title-format                 "Emacs"
      scroll-margin                     0
      scroll-conservatively             101
      enable-recursive-minibuffers      t
      resize-mini-windows               t
      even-window-heights               nil
      pop-up-windows                    t
      delete-by-moving-to-trash         t
      compilation-debug                 t
      comint-move-point-for-output      nil
      comint-scroll-show-maximum-output t
      compilation-scroll-output         'first-error
      Buffer-menu-name-width            40
      ring-bell-function                'ignore
      apropos-do-all                    t
      backup-by-copying                 t
      enable-remote-dir-locals          t)

(setq-default indent-tabs-mode nil
              fill-column      75
              c-basic-offset   4
              truncate-lines   t)

;; Lots of syntax highlighting
(global-font-lock-mode 1)
(setq font-lock-maximum-decoration t)

;; Lots of undo storage
(setq undo-limit 200000)
(setq undo-strong-limit 300000)

(setq-default ediff-use-faces t
              ediff-highlighting-style 'face
              ediff-highlight-all-diffs t
              ediff-auto-refine 'on
              ediff-ignore-similar-regions t)

;; Never kill scratch or messages
(defun gpb-kill-buffer-query-function ()
  (let ((buffer (current-buffer)))
    (cond
     ((member (buffer-name) '("*scratch*" "*Messages*"))
      ;; Never kill *scratch*
      (error "You don't really want to kill the %s buffer."
               (buffer-name)))
     (t t))))
(add-to-list 'kill-buffer-query-functions 'gpb-kill-buffer-query-function)

;; Save every 5 minutes.
(recentf-mode 1)
(run-at-time (* 5 60) (* 5 60) 'recentf-save-list)
(setq recentf-max-saved-items 1000)

;; More convenience names for some interactive functions.
(defalias 'ediff-with-saved 'gpb-ediff-with-saved)

(setenv "PAGER" "cat")
(setenv "PYTHONUNBUFFERED" "1")

(require 'tramp)

;; Make `shell' in a remote buffer use bash instead of sh.
;; (setq explicit-shell-file-name "/bin/bash")

;; Some kind of styling
(eval-after-load 'poly-markdown
  '(oset poly-markdown-root-innermode :adjust-face 25))

;; My filtered lists
(require 'gpb-filtered-list)
(global-set-key [remap switch-to-buffer] 'gpb-switch-buffer-filtered)
(global-set-key "\C-cr" 'gpb-recentf-open-files-filtered)

;; Git integration bindings
(autoload 'prat-user-command-prefix-keymap "prat" nil nil 'keymap)
(global-set-key "\C-cv" 'prat-user-command-prefix-keymap)

(with-eval-after-load 'isearch
  (setq search-nonincremental-instead nil
        isearch-allow-scroll t)
  (define-key isearch-mode-map "\C-h" 'isearch-delete-char)
  (define-key isearch-mode-map "\C-f" 'isearch-repeat-forward)
  (define-key isearch-mode-map "\C-v" 'isearch-yank-kill))

(with-eval-after-load 'minibuffer
  (define-key minibuffer-local-map "\C-h" 'backward-delete-char-untabify)
  (define-key minibuffer-local-map "\C-f" 'forward-char)
  (define-key minibuffer-local-map "\C-b" 'backward-char)
  (define-key minibuffer-local-map "\C-p" 'previous-history-element)
  (define-key minibuffer-local-map "\C-n" 'next-history-element)
  (define-key minibuffer-local-map "\M-h" 'backward-kill-word)
  (define-key minibuffer-local-map "\C-v" 'yank)
  (define-key minibuffer-local-map "\C-z" 'undo)
  (define-key minibuffer-local-map [(control tab)] 'gpb-next-window)
  (define-key minibuffer-local-map [(control shift tab)] 'gpb-previous-window)
  (define-key minibuffer-local-filename-completion-map "\M-h"
    'gpb:delete-path-segment-backwards))

(define-key lisp-mode-shared-map "\t" 'completion-at-point)
(define-key lisp-interaction-mode-map [(control return)] 'eval-print-last-sexp)

(setq frame-title-format '(:eval (or (buffer-file-name) "emacs"))
      ;; Show the point in the mode line
      mode-line-position '(20 (:eval (format "(%%l,%%3c) %5d" (point)))))

;; Completion in M-x execute-extended-command.
(define-key read-expression-map "\t" 'completion-at-point)

;; Evil config
(require 'gpb-evil)
(require 'gpb-util)
(require 'gpb-r-mode)

(setq debug-on-quit nil)
