;; We set this so we can debug a slow init process.
(setq debug-on-quit t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Add the source tree rooted at the directory containing this file to the
;;  load path.
;;

(let* ((default-directory (file-name-directory
                           (or load-file-name (buffer-file-name)))))
  ;; Add current directory to load path without
  (add-to-list 'load-path (expand-file-name "."))
  ;; Add most subdirectories to `load-path'.  The directories
  ;; machine-specific and backports are excluded because they contain
  ;; empty .nosearch files (see
  ;; `normal-top-level-add-subdirs-to-load-path').
  (normal-top-level-add-subdirs-to-load-path))

;; Load YAS snippets when the package is available.
(eval-after-load 'yasnippet
  `(progn
     (setq yas-snippet-dirs (list
                             (concat (file-name-directory
                                      (or load-file-name (buffer-file-name)))
                                     "snippets")))
     (yas-reload-all)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Global key bindings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CUA-like bindings
(global-set-key "\C-v" 'yank)
(global-set-key "\M-v" 'yank-pop)
(global-set-key "\C-z" 'undo)
(global-set-key "\C-b" 'switch-to-buffer)

;; Space related bindings
(global-set-key "\M- " 'gpb-cycle-following-space)
; This is shift space?
(global-set-key [(shift return)] 'gpb-insert-newline-after)

;; Replace some standard interactive commands with my versions
(global-set-key [remap execute-extended-command]
                'gpb-exec-command/eval-expression)
(global-set-key [remap kill-line] 'gpb-kill-line)
(global-set-key [remap erase-buffer] 'gpb-erase-buffer)
(global-set-key [remap goto-line] 'gpb-goto-line)
(global-set-key [remap yank] 'gpb-yank)
(global-set-key [remap scroll-down-command] 'gpb-backward-page-1)
(global-set-key [remap scroll-up-command] 'gpb-forward-page-1)
(global-set-key [remap switch-to-buffer] 'gpb-switch-buffer-filtered)
(global-set-key [remap list-buffers] 'buffer-menu)
(global-set-key [remap apropos-command] 'apropos)

;; It seems more useful to show the buffer than to clone it.
(global-set-key "\C-x4c" 'gpb:show-buffer-other-window)
(global-set-key "\C-x4C" 'clone-indirect-buffer-other-window)

;; Control-c bindings
(global-set-key "\C-ci" 'gpb:create-imenu-index-buffer)
(global-set-key "\C-c4i" 'gpb:create-imenu-index-buffer-other-window)
(global-set-key "\C-cn" 'gpb-new-document)
(global-set-key "\C-c$" 'gpb-ispell)
(global-set-key "\C-cg" 'gpb-grep)
(global-set-key "\C-cr" 'recentf-open-files)
(global-set-key "\C-cv" 'magit-status)

;; The next binding does nothing because escape is now remapped to C-g
;; at the input-decode level
(global-set-key [escape] 'keyboard-quit)

;; Some CUA standard bindings for common operations
(global-set-key "\C-o" 'gpb-ffap)
(global-set-key "\C-f" 'isearch-forward)
(global-set-key "\C-s" 'save-buffer)
(global-set-key [(control shift s)] 'write-file)
(global-set-key "\C-w" 'gpb-kill-buffer)
(global-set-key [(f5)] 'gpb-reload-buffer)

;; Sexp movement
(global-set-key [(meta right)] 'forward-sexp)
(global-set-key [(meta left)]  'backward-sexp)
(global-set-key [(meta up)]    'backward-up-list)
(global-set-key [(meta down)]  'down-list)

;; Use C-h and friends for backspace like in the terminal
(global-set-key "\C-h" 'backward-delete-char)
(global-set-key [(meta h)] 'backward-kill-word)
(global-set-key [remap backward-delete-char]
                'backward-delete-char-untabify)
(setq backward-delete-char-untabify-method nil)

;; Bookmarks
(global-set-key (kbd "<C-f2>") 'bm-toggle)
(global-set-key (kbd "<f2>")   'bm-next)
(global-set-key (kbd "<S-f2>") 'bm-previous)

;;  Some more movement commands
(global-set-key [(control tab)] 'gpb-next-window)
(global-set-key [(control shift iso-lefttab)] 'gpb-previous-window)
(global-set-key [(control shift tab)] 'gpb-previous-window)

;; "Goto" bindings
(global-set-key "\M-gc" 'goto-char)
(global-set-key "\M-gl" 'goto-line)
(global-set-key "\M-gd" 'gpb-lisp-goto-definition)

;; Remove useless printing options from menu
(let ((file-menu (lookup-key global-map [menu-bar file])))
  (define-key file-menu [new-file] nil)
  (define-key file-menu [open-file] nil)
  (define-key file-menu [separator-print] nil)
  (define-key file-menu [print-buffer] nil)
  (define-key file-menu [print-region] nil)
  (define-key file-menu [ps-print-buffer] nil)
  (define-key file-menu [ps-print-region] nil)
  (define-key file-menu [ps-print-buffer-faces] nil)
  (define-key file-menu [ps-print-region-faces] nil))

;; Add two useful entries to file menu
(define-key global-map [menu-bar file open-file]
  (cons "Open File..." 'find-file))

(define-key-after global-map [menu-bar file open-dir]
  (cons "Open Dir..." 'dired) 'open-file)

(define-key-after global-map [menu-bar file gpb-new-document]
  (cons "New Buffer" 'gpb-new-document) 'open-dir)


;; Convenience function ---------------------------------------------------

(defun load-safe (filename)
  "Issue warnings rather than failing when things fail to load."
  (condition-case-unless-debug err
      (load filename)
    (error (message "Error loading %s: %S" filename err))))


;; Misc. customizations ---------------------------------------------------

(show-paren-mode 1)
(tool-bar-mode -1)
(menu-bar-mode 1)
(line-number-mode 1)
(column-number-mode 1)
(blink-cursor-mode 1)

(setq require-final-newline             nil
      history-delete-duplicates         t
      comint-input-ignoredups           t
      delete-trailing-lines             nil
      use-file-dialog                   nil
      default-tab-width                 4
      initial-scratch-message           ";; *scratch* buffer\n"
      print-escape-newlines             t
      Man-notify-method                 'pushy
      ;; Setting eldoc-idle-delay to zero seems to cause problems
      ;; eldoc-idle-delay                  0
      mouse-drag-copy-region            nil
      transient-mark-mode               t
      mark-even-if-inactive             nil
      set-mark-command-repeat-pop       t
      x-select-enable-clipboard         t
      yank-pop-change-selection         t
      inhibit-startup-screen            t
      initial-buffer-choice             nil
      icon-title-format                 "emacs"
      scroll-margin                     0
      scroll-conservatively             10000
      mouse-wheel-scroll-amount         '(1)
      mouse-wheel-progressive-speed     nil
      enable-recursive-minibuffers      t
      resize-mini-windows               t
      even-window-heights               nil
      pop-up-windows                    t
      delete-by-moving-to-trash         t
      compilation-debug                 t
      comint-move-point-for-output      nil
      comint-scroll-show-maximum-output t
      compilation-scroll-output         t
      Buffer-menu-name-width            50
      ring-bell-function                'ignore
      bm-highlight-style                'bm-highlight-only-fringe
      bm-cycle-all-buffers              t
      apropos-do-all                    t)

(setq frame-title-format '(:eval (or (buffer-file-name) "emacs"))
      ;; Show the point in the mode line
      mode-line-position '(20 (:eval (format "(%%l,%%3c)  pt:%5d" (point))))
      open-paren-in-column-0-is-defun-start nil)

(setq-default indent-tabs-mode nil
              fill-column      75)

(when (string-equal system-type "windows-nt")
  (setq backup-directory-alist '(("." . ".emacs-backups"))))

;; Setup ansi
(require 'ansi-color)
(setq ansi-color-names-vector ["black" "red2" "forest green"
                               "dark goldenrod" "blue" "purple4"
                               "dark cyan" "white"]
      ansi-color-map (ansi-color-make-color-map))

;; Don't disable these commands
(dolist (cmd '(narrow-to-region upcase-region downcase-region
               erase-buffer set-goal-column))
  (put cmd 'disabled nil))

;; Whitespace handling
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Lots of syntax highlighting
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; Lots of undo storage
(setq undo-limit 200000)
(setq undo-strong-limit 300000)

(setq-default ediff-use-faces t
              ediff-highlighting-style 'face
              ediff-highlight-all-diffs t
              ediff-auto-refine 'on
              ediff-ignore-similar-regions t)

;; Always set the mark in a buffer.  I find this useful for
;; `pop-global-mark` navigation.
(defun gpb-ensure-mark-exists ()
  (unless mark-ring (push-mark (point-min) t nil)))
(add-hook 'post-command-hook 'gpb-ensure-mark-exists)

;;  Uniquify buffer names
(when (require 'uniquify nil t)
  (setq uniquify-buffer-name-style 'post-forward)
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
  (setq uniquify-ignore-buffers-re "^\\*")) ; rename after killing uniquified

;; Use the correct browser on linux
(when (string-equal system-type "gnu/linux")
  (setq browse-url-generic-program (executable-find "xdg-open")
        browse-url-browser-function 'browse-url-generic))

;; Remove percentage from mode line
(when (ignore-errors (string= (cadar mode-line-position) "%p"))
  (setq mode-line-position (cdr mode-line-position)))


;; Frame related customization --------------------------------------------

;; Setup remap some keys using `input-decode-map'
(add-hook 'after-make-frame-functions 'gpb-setup-frame-input-maps)
;; The initialization files (i.e. .emacs) are read after the initial frame
;; is created, so we must apply the hooks in `after-make-frame-functions'
;; to the initial frame manually after initialization is complete.
(add-hook 'after-init-hook 'gpb-setup-frame-input-maps)

;; Setup the default frame attributes
(push '(vertical-scroll-bars . right) default-frame-alist)
(push '(menu-bar-lines . 1) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(cursor-type . bar) default-frame-alist)
(push '(mouse-color . "black") default-frame-alist)

;; Highlight the mode line when minibuffer is not active -------------------

(defvar gpb-blur-mode-line-save-buffer nil
  "Variable used by `gpb-highlight-mode-line' and
  `gpb-blur-mode-line'.")

(defun gpb-highlight-mode-line (&optional frame)
  (when (and gpb-blur-mode-line-save-buffer
             (buffer-live-p gpb-blur-mode-line-save-buffer))
    (with-current-buffer gpb-blur-mode-line-save-buffer
      (setq face-remapping-alist (delete `(mode-line mode-line-inactive)
                                         face-remapping-alist)))))

(defun gpb-blur-mode-line ()
  (with-current-buffer
      (window-buffer (or (minibuffer-selected-window) (selected-window)))
    (add-to-list 'face-remapping-alist `(mode-line mode-line-inactive))
    (setq gpb-blur-mode-line-save-buffer (current-buffer))))

(gpb-highlight-mode-line)
(add-hook 'minibuffer-setup-hook 'gpb-blur-mode-line)
(add-hook 'minibuffer-exit-hook 'gpb-highlight-mode-line)

;; Highlighting for errors
(setq next-error-highlight nil
      next-error-highlight-no-select nil
      next-error-recenter nil)

(defun gpb-grep/occur-hook ()
  (setq truncate-lines t))

(add-hook 'next-error-hook 'gpb-highlight-next-error)
(add-hook 'grep-mode-hook 'gpb-grep/occur-hook)
(add-hook 'occur-mode-hook 'gpb-grep/occur-hook)


;; Improve vertical scrolling -------------------------------------------

(defadvice save-buffer (around preserve-column activate)
  "Prevent saving the buffer from moving the point."
  (let ((col (current-column)))
    ad-do-it
    (move-to-column col t)))


;; Never kill scratch or messages -----------------------------------------

(defun gpb-kill-buffer-query-function ()
  (let ((buffer (current-buffer)))
    (cond
     ((member (buffer-name) '("*scratch*" "*Messages*"))
      ;; Never kill *scratch*
      (error "You don't really want to kill the %s buffer."
               (buffer-name)))
     (t t))))

(add-to-list 'kill-buffer-query-functions 'gpb-kill-buffer-query-function)


;; Tab completion when reading expressions. -------------------------------

(define-key read-expression-map "\t" 'gpb-lisp-complete-symbol)

(defun gpb-lisp-complete-symbol ()
  ;; Show all symbols
  (interactive)
  (lisp-complete-symbol (lambda (x) t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  auto-mode-alist, autoloads, and eval-after-loads
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.xrc\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

(autoload 'gpb-ispell "gpb-ispell" "" 'interactive)
(autoload 'gpb-open-ielm-here "gpb-lisp-init" "" t)
(autoload 'octave-mode "octave-mode" nil t)
(autoload 'run-bash "gpb-readline" "Run bash" t)

(eval-after-load 'dired  '(load-safe "gpb-dired"))
(eval-after-load 'help-mode '(load-safe "gpb-help"))
(eval-after-load 'ispell '(load-safe "gpb-ispell"))
(eval-after-load 'compile '(define-key compilation-mode-map "\C-o" nil))
(eval-after-load 'latex '(load-safe "gpb-latex"))
(eval-after-load 'python '(load-safe "gpb-python2"))
(eval-after-load 'ess-site '(load-safe "gpb-ess"))
(eval-after-load 'git-commit '(global-git-commit-mode -1))

;; Minibuffer customization -----------------------------------------------

(eval-after-load 'minibuffer
  '(progn
     (define-key minibuffer-local-map [remap keyboard-quit] 'abort-recursive-edit)
     (define-key minibuffer-local-map [(control n)] 'next-history-element)
     (define-key minibuffer-local-map [(control p)] 'previous-history-element)
     (define-key minibuffer-local-map [(control tab)] 'gpb-next-window)))


;; Completion buffer customization ----------------------------------------

(define-key completion-list-mode-map "\t" 'next-completion)
(define-key completion-list-mode-map [(backtab)] 'previous-completion)


;; Interactive search -----------------------------------------------------

(eval-after-load "isearch"
  '(progn
     (setq search-nonincremental-instead nil
           isearch-allow-scroll t)
     (define-key isearch-mode-map "\C-h" 'isearch-delete-char)
     (define-key isearch-mode-map [(control ?f)] 'isearch-repeat-forward)
     (define-key isearch-mode-map [(control ?v)] 'isearch-yank-kill)
     (define-key isearch-mode-map [(control ?s)] 'gpb-isearch-yank-symbol)
     (define-key isearch-mode-map [(control g)] 'isearch-abort)
     (define-key isearch-mode-map "\r" 'isearch-exit)
     (define-key isearch-mode-map [tab] 'isearch-repeat-forward)
     (define-key isearch-mode-map [backtab] 'isearch-repeat-backward)
     (define-key isearch-mode-map [remap isearch-exit] 'gpb-isearch-exit)
     (define-key minibuffer-local-isearch-map [tab] 'isearch-complete-edit)))

(defun gpb-isearch-exit ()
  (interactive)
  (when isearch-forward (isearch-repeat-backward))
  (isearch-exit))

;; Regular expression builder ---------------------------------------------

(eval-after-load 're-builder
  '(progn
     (define-key reb-mode-map [(control escape)] 'reb-quit)
     (define-key reb-mode-map "\C-c\M-%" 'reb-query-replace-this-regxp)
     (defadvice re-builder (after make-dedicated activate)
       (set-window-dedicated-p (selected-window) t))))


;; Octave (Matlab) mode ---------------------------------------------------

(eval-after-load "octave-mode"
  '(progn
     (defun gpb-octave:disable-remove-trailing-newlines ()
       (setq-local before-save-hook nil))
     (add-hook 'octave-mode-hook 'gpb-octave:disable-remove-trailing-newlines)))


;; Shell scripts ----------------------------------------------------------

(eval-after-load "sh-script"
  '(define-key sh-mode-map [(control c)(control c)] 'gpb-execute-shell-script))


;; Info mode --------------------------------------------------------------

(eval-after-load "info"
    '(progn
       (define-key Info-mode-map "\C-cb" 'Info-history-back)
       (define-key Info-mode-map "\C-cf" 'Info-history-forward)))


;; Improve alignment for dollar amounts

(eval-after-load "align"
  '(aput 'align-rules-list 'text-dollar-figure
         '((regexp . "\\$?\\(\\s-+-?[0-9,]+\\)\\.")
           (modes . align-text-modes)
           (justify . t)
           (run-if lambda nil
                   (eq '- current-prefix-arg)))))


;; CC mode ----------------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(defun gpb-cc-tab-command ()
  (interactive)
  (if (looking-back "\\sw\\|\\s_\\|\\s.")
      (progn
        (auto-complete)
        (run-at-time 0.1 nil 'gpb-ac-complete))
    ;; (setq unread-command-events (nconc unread-command-events
    ;;                                    '(tab))))
    (indent-according-to-mode)))

(setq c-default-style "linux")

(defun gpb-cc-mode-setup ()
  (require 'yasnippet nil t)
  (require 'auto-complete-clang nil 1)
  ;; (require 'auto-complete-clang-async)
  (setq ;; ac-clang-complete-executable
        ;; "~/lib/common/emacs-lisp/emacs-clang-complete-async/clang-complete"
        ;; ac-sources '(ac-source-clang-async)
        ac-sources '(ac-source-clang)
        ;;ac-clang-async-do-autocompletion-automatically nil
        c-basic-offset 4
        )
  (c-set-offset 'inline-open 0)
  ;; (ac-clang-launch-completion-process)
  ;; (auto-complete-mode t)
  (gpb-modal--define-command-key "\C-i" 'indent-according-to-mode t)
  ;; (local-set-key "\C-i" 'gpb-cc-tab-command t)
  (local-set-key "\C-c\C-c" '(lambda () (interactive)
                               (save-buffer)
                               (recompile))))

(add-hook 'c-mode-common-hook 'gpb-cc-mode-setup)


;; unit testing -----------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.elk\\'" . elk-test-mode))
(autoload 'elk-test-mode "elk-test" nil t)
(eval-after-load "elk-test"
  `(define-key elk-test-mode-map [(control c)(control c)]
     'elk-test-run-buffer))


;; Browse the kill ring  --------------------------------------------------

(defun gpb-browse-kill-ring-forward (&optional arg)
  (interactive "p")
  (let ((save show-paren-mode))
    (show-paren-mode -1)
    (browse-kill-ring-forward arg)
    (if save
        (show-paren-mode 1)
      (show-paren-mode -1))))

(eval-after-load "browse-kill-ring"
 '(progn (setq browse-kill-ring-highlight-current-entry t
               browse-kill-ring-highlight-inserted-item t
               browse-kill-ring-show-preview nil
               browse-kill-ring-display-duplicates nil
               browse-kill-ring-quit-action 'save-and-restore
               browse-kill-ring-separator "--------------------")
         (let ((map browse-kill-ring-mode-map))
           (define-key map [remap browse-kill-ring-forward]
             'gpb-browse-kill-ring-forward)
           (define-key map [(escape)] 'browse-kill-ring-quit)
           (define-key map [(control g)] 'browse-kill-ring-quit)
           (define-key map "j" 'browse-kill-ring-forward)
           (define-key map "\t" 'browse-kill-ring-forward)
           (define-key map "k" 'browse-kill-ring-previous)
           (define-key map [(backtab)] 'browse-kill-ring-previous)
           (define-key map "" 'browse-kill-ring-insert-move-and-quit))))


;; Version control customization ---------------------------------------

(defvar gpb-vc-save-configuration nil)
(setq vc-hg-log-switches '("-v"))

(defun gpb-vc-next-action ()
  "Prevent annoying closure of all windows."
  (interactive)
  (setq gpb-vc-save-configuration (current-frame-configuration))
  (vc-next-action nil))

(defun gpb-log-edit-done ()
  (interactive)
  (log-edit-done)
  (set-frame-configuration gpb-vc-save-configuration))

(eval-after-load "log-edit"
  '(define-key log-edit-mode-map [(control c)(control c)] 'gpb-log-edit-done))

(eval-after-load "vc-hooks"
  '(progn
     (define-key vc-prefix-map "e" 'ediff-revision)
     (define-key vc-prefix-map "v" 'gpb-vc-next-action)))


;; Tags customization --------------------------------------------------

(defadvice find-tag (after gpb-recenter activate)
  (recenter 10))

(defadvice find-tag-other-window (after gpb-recenter activate)
  (recenter 10))

;; Emacs server customization ------------------------------------------

(eval-after-load "server"
  '(let ((map (make-sparse-keymap)))
     ;; The following keybindings are available in a buffers where
     ;; `server-buffer-clients' is non-nil.
     (define-key map [(control c)(control c)] 'server-edit)
     (aput 'minor-mode-map-alist 'server-buffer-clients map)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  My customization files and modes
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'gpb-misc)
(require 'gpb-util)
(require 'gpb-filtered-list)

;; These should probably be an auto load ...
(ignore-errors (load-safe "gpb-lisp"))
(ignore-errors (load-safe "gpb-xml"))

(require 'gpb-comint)
(add-hook 'comint-mode-hook 'gpb-comint:enable-bold-prompt-mode t)
(let ((map comint-mode-map))
  (define-key map "\C-m" 'gpb-comint:goto-error-or-send)
  (define-key map "\C-cd" 'gpb-comint:delete-output)
  (define-key map "\C-ck" 'comint-kill-subjob)
  (define-key map "\C-ce" 'comint-send-eof)
  (define-key map "\C-n" 'comint-next-input)
  (define-key map "\C-p" 'gpb-comint:previous-input)
  (define-key map "\C-d" nil))

(require 'gpb-yank-rigidly)
(gpb-yank-rigidly-mode 1)

;; (require 'gpb-mark-ring)
;; (gpb-mark-ring-mode -1)

(require 'gpb-modal)
(gpb-modal-mode 1)

;; (require 'gpb-region)
;; (gpb-region-keymap-mode 1)

;; Rectangle mark mode
;; (autoload 'gpb-rect--begin-rect-command "gpb-rect-commands")

(recentf-mode 1)

;; More convenience names for some interactive functions.
(defalias 'ediff-with-saved 'gpb-ediff-with-saved)

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
;; (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(gpb-util:add-global-compilation-errors
 `((gpb-pdb-location
    "^>? *\\([^(\n]+\\.py\\)[(:]\\([0-9]+\\)[): ]" 1 2)))

(setenv "PAGER" "cat")
(setenv "PYTHONUNBUFFERED" "1")

(defun gpb:kill-path-segment-backwards ()
  (interactive)
  (kill-region (point)
               (save-excursion
                 (save-match-data
                   (when (looking-back "[/\\]") (backward-char))
                   (if (re-search-backward "[/\\]" nil t)
                       (forward-char)
                     (move-beginning-of-line nil))
                   (point)))))

(define-key minibuffer-local-filename-completion-map "\M-h"
  'gpb:kill-path-segment-backwards)

(eval-after-load 'magit-mode
  '(progn
     (setq magit-display-buffer-function
           'magit-display-buffer-same-window-except-diff-v1

           magit-display-file-buffer-function
           (lambda (buf)
             (pop-to-buffer buf 'other-window)
             (message "selected window: %s" (selected-window))
             (run-at-time 0.1 nil 'recenter)))))

(setq debug-on-quit nil)
