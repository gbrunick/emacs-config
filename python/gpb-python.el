;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Customization for python mode.
;;
;;  This file should be loaded after the file which defined `python-mode'
;;  and changes the behavior of emacs in a number of ways.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'python)
(require 'yasnippet)
;; (require 'jedi)

(require 'gpb-util)
(require 'gpb-comint)
(require 'gpb-python-jedi)
(require 'gpb-ipython)

;; (require 'gpb-ipython-echo)
;; (require 'gpb-ipython-completion)


(define-key python-mode-map "\t" 'gpb-py:tab-command)
(define-key python-mode-map "\n" 'newline-and-indent)
(define-key python-mode-map "\C-cb" 'gpb-py:copy-breakpoint)
(define-key python-mode-map "\C-cj" 'gpb-py:copy-jump)
(define-key python-mode-map "\C-c8" 'gpb-py:run-flake8)

(define-key inferior-python-mode-map "\C-p" 'comint-previous-input)
(define-key inferior-python-mode-map "\C-n" 'comint-next-input)

(add-hook 'python-mode-hook 'gpb-py:python-mode-hook)
(add-hook 'comint-output-filter-functions
          'python-pdbtrack-comint-output-filter-function)

;; (add-hook 'inferior-python-mode-hook 'gpb-py:inferior-python-mode-hook)

(setq python-shell-enable-font-lock nil)
(when (boundp 'python-indent-trigger-commands)
  (add-to-list 'python-indent-trigger-commands 'indent-according-to-mode))

;; This is a global mode.
;; (gpb-ipython-echo-mode 1)

;; Convenient names for interactive functions
(defalias 'run-ipython 'gpb-ipy:start-ipython)

;; Some text objects and commands
(defun gpb-py:forward-top-level-block (n)
  "Move forward to end of current top level block.

If N < 0, we move to the beginning of the current top level
block.  If |N|, we repeat this operation |N| times."
  (cond
   ((> n 0)
    (goto-char
     (save-excursion
       ;; Move forward to a line containing code.  This could be the current
       ;; line.
       (forward-line 0)
       (while (and (not (eobp))
                   (looking-at-p "^[ \t]*\\(#.*\\)?$"))
         (forward-line 1))
       (when (eobp) (throw 'search-failed
                           "There is no more code in the buffer"))
       (forward-line 1)
       ;; Now move forward to the beginning of a top level form by skipping
       ;; blank lines, indented lines, and comment lines.
       (while (and (not (eobp))
                   (looking-at-p "^\\(\n\\|[ \t#]\\)"))
         (forward-line 1))
       ;; Finally skip backward over blank lines and comments
       (while (looking-back "^\\([ \t]*#.*\\)?\n")
         (forward-line -1))
       (point)))
    (gpb-py:forward-top-level-block (- n 1)))
   ((< n 0)
    (re-search-backward "^[a-zA-Z]")
    (gpb-py:forward-top-level-block (+ n 1)))))


;; (eval-after-load 'gpb-text-objects
;;   '(progn
;;      (gpb-tobj--define-command gpb-py:execute-text-object (obj beg end)
;;         "Execute the text object as python code."
;;         (gpb-tobj--flash-region beg end)
;;         (display-buffer (gpb-py-exec:execute-code beg end))
;;         ;; (goto-char end)
;;         )

;;      (gpb-tobj--define-flat-text-object top-level-python-block
;;        "a top level block of python code."
;;        :forward-func gpb-py:forward-top-level-block
;;        :key-binding (root "j")
;;        :key-binding (root "" :backwards t))))

(when (require 'gpb-python-exec nil t)
  (setq gpb-py-exec:execute-code-in-interpreter-function
        'gpb-ipy:send-string)
  (define-key python-mode-map [(control c)(control c)]
    'gpb-py-exec:execute-code-command))


(eval-after-load "yasnippet"
  '(yas/define-snippets
    'python-mode
    `(("i" "import ")
      ("dt" "import datetime as dt\n")
      ("np" "import numpy as np\n")
      ("pd" "import pandas as pd\n")
      ("it" "import itertools as it\n")
      ("plt" "import matplotlib.pyplot as plt\n")
      ("mpl" "import matplotlib as mpl\n")
      (".f" ".format(**locals())")
      ("logger"
       ,(concat
         "# See https://docs.python.org/2/howto/logging.html#library-config\n"
         "import logging\n"
         "logger = logging.getLogger(__name__)\n"
         "logger.addHandler(logging.NullHandler())\n$0"))
      ("log" "log.debug('$1')$0")
      ("logvar" "logger.${1:debug}('$2=%r', ${2:variable name})\n$0")
      ("ifmain" "if __name__ == '__main__':\n    $0")
      ("nose" "import nose\nnose.main()$0")
      ("winpdb" "from gpb.debug import start_winpdb; start_winpdb")
      ("rpdb2" "import rpdb2; rpdb2.start_embedded_debugger(\"${1:password}\");$0")
      ("pdb" "import pdb; print(); pdb.set_trace()")
      ("pdspdb"
       "from ceg.pds.interop import open_pdb_prompt; open_pdb_prompt()")
      ("qtpdb" "import PyQt4.QtCore, pdb
               PyQt4.QtCore.pyqtRemoveInputHook(); pdb.set_trace()")
      ("qtipython"
       "from IPython.terminal.embed import InteractiveShellEmbed
       from PyQt4.QtCore import pyqtRemoveInputHook; pyqtRemoveInputHook()
       InteractiveShellEmbed.instance()()")

      ("qtipdb" "from PyQt4.QtCore import pyqtRemoveInputHook
                pyqtRemoveInputHook()
                import ipdb; ipdb.set_trace()")
      ;;("ipdb" "from IPython.Debugger import Tracer; Tracer().__call__()")
      ("ipdb" "from IPython.core.debugger import Tracer; Tracer()()")
      ("use_ipython" ,(concat "use_ipython = True\n"
                              "if use_ipython:\n"
                              "	from IPython.Shell import IPShellEmbed\n"
                              "	shell = IPShellEmbed()\n"
                              "	shell()\n"
                              "else:\n"
                              "	app.exec_()\n"))
      ;; ("ipython" "from IPython.Shell import IPShellEmbed; IPShellEmbed()()")
      ("ipython"
       "from IPython.terminal.embed import InteractiveShellEmbed\n$>InteractiveShellEmbed.instance()()")
      ("wxinspect" ,(concat "from wx.lib.inspection import InspectionTool; "
                            "InspectionTool().Show()")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Various functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar gpb-py:xmode-plain "get_ipython().run_line_magic('xmode', 'Plain')"
  "Produce parsable tracebacks in IPython")


;; (defun gpb-py:execute-code-in-interpreter (code)
;;   "Execute the CODE in a python interpreter."
;;   ;; I would prefer not to start spin up a process automatically.
;;   (let ((buffer (gpb-py:get-interpreter-buffer)))
;;     (unless buffer (error "You must first start an interpreter"))
;;     (comint-snapshot-last-prompt)
;;     (python-shell-send-string code)
;;     buffer))


(defun gpb-py:get-interpreter-buffer ()
  "Returns the buffer containing an active interpreter or nil"
  (cond
   ;; Fabian E. Gallina's `python-mode'
   ((and (= emacs-major-version 24)
         (string-equal (file-name-base (symbol-file 'python-mode)) "python"))
    ;; I would prefer not to start spin up a process automatically.
    (let ((proc (python-shell-get-process)))
      (and proc (process-buffer proc))))
   ;; Handle other cases when you must...
   (t
    (error "Not implemented"))))


(defun gpb-py:inferior-python-mode-hook ()
  "Customize inferior-python-mode-hook buffers."
  (gpb-ipython-completion-mode 1))
;;(set (make-variable-buffer-local 'compilation-mode-font-lock-keywords) nil))


(defun gpb-py:python-mode-hook ()
  "Customize python-mode buffers."
  (set (make-local-variable 'tab-width) 4)
  (set (make-local-variable 'yas/key-syntaxes) (list "w_" "w_." "^ "))
  (set (make-local-variable 'fill-column) 75)
  (set (make-local-variable 'outline-regexp)
       "[[:space:]]*\\_<\\(?:class\\|def\\)\\_>\\|if +__name__ +==")
  ;; (set (make-local-variable 'ac-use-comphist) nil)
  ;; (auto-complete-mode 1)
  ;; (gpb-jedi-mode 1)
  ;; (eldoc-mode 1)
  (cond
   ((member 'gpb-text-objects features)
    (setq-local execute-text-object-function 'gpb-ipy:execute-region))
   ((member 'gpb-modal features)
    (gpb-modal--define-command-key
     "\t" 'gpb-back-to-indentation-or-indent-according-to-mode t))
     ;; (gpb-modal--define-command-key "!" 'gpb-py:execute-text-object t)
   ;; The following key binding is redundant given the previous key binding
   ;; ((member 'gpb-region features)
   ;;  (gpb-reg--define-key "!" 'gpb-py:execute-region t))
   ))


(defun gpb-py:run-ipython ()
  "Run IPython as an inferior process"
  (interactive)
  (cond
   ((and (= emacs-major-version 24)
         (string-equal (file-name-base (symbol-file 'python-mode)) "python"))
    ;; The function `run-python' uses `python-util-clone-local-variables' to
    ;; clone all related buffer local variables so we set these variables in
    ;; a temporary buffer to get them installed in the python shell buffer.
    (let ((buffer (gpb-py:get-interpreter-buffer))
          ;; Suppress extraneous compilation-shell-minor-mode fontification.
          (compilation-mode-font-lock-keywords nil))
      (if buffer
          (pop-to-buffer buffer)
        (with-temp-buffer
          (set (make-local-variable 'python-shell-interpreter) "ipython")
          (set (make-local-variable 'python-shell-interpreter-args)
               "console --colors=LightBG")
          ;; (set (make-local-variable 'python-shell-interpreter) "ipython")
          ;; (set (make-local-variable 'python-shell-interpreter-args)
          ;;      "console --colors=LightBG")

          ;; (set (make-local-variable 'python-shell-buffer-name) "IPython")
          (set (make-local-variable 'python-shell-prompt-regexp)
               "In \\[[0-9]+\\]: ")
          (set (make-local-variable 'python-shell-prompt-output-regexp)
               "Out\\[[0-9]+\\]: ")
          (set (make-local-variable 'python-shell-completion-setup-code)
               "from IPython.core.completerlib import module_completion")
          (set (make-local-variable 'python-shell-completion-module-string-code)
               "';'.join(module_completion('''%s'''))\n")
          (set (make-local-variable 'python-shell-completion-string-code)
               "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
          (set (make-local-variable 'python-shell-process-environment)
               '("PAGER=more" "GPB=t"))
          (set (make-local-variable 'python-shell-setup-codes)
               (append (eval (car (get 'python-shell-setup-codes
                                       'standard-value)))
                       '(gpb-py:xmode-plain)))
          (call-interactively 'run-python)))))
   (t
    (error "Not implemented"))))


(defun gpb-py:tab-command (arg)
  "The multi-purpose command generally bound to the TAB key"
  (interactive "P")
  (cond
   (arg (company-complete-common))
   (t
    (let ((yas/fallback-behavior 'return-nil)
          (yas/indent-line 'fixed)
          (cmds '(gpb-try-to-indent-according-to-mode
                  gpb-py:try-to-complete-init
                  yas/expand
                  gpb-company-try-to-complete-common
                  ;; gpb-py:try-to-complete
                  ;; gpb-py:try-completion
                  ;; indent-for-tab-command
                  )))
      ;; Try each command until we have a success
      (while (and cmds (not (call-interactively (car cmds))))
        (setq cmds (cdr cmds)))))))


(defun gpb-py:try-to-complete ()
  (interactive)
  (with-syntax-table python-dotty-syntax-table
    (when (looking-back "\\sw\\|\\s_")
      (delete-region (point) (save-excursion (skip-syntax-forward "w_") (point)))
      (jedi:complete))))


(defun gpb-py:try-to-complete-init ()
  "Try to complete and __init__ function.

  The cursor should be sitting after def __init__(var1, var2):"
  (interactive)
  (when (looking-back (concat "def ?__init__(\\(.*\\)):"))
    (let ((args (gpb-util-split-string
                 ;; remove anything inside matching paranthesis
                 (gpb-util-reduce-string (match-string 1) "([^)])" "")
                 ",[ \t\n]" nil 'omit-blanks)))
      (unless (string-match-p "^self\\(=.*\\)?" (car args))
        (error "The first argument of __init__ should be self"))
      (insert "\n")
      (indent-according-to-mode)
      (dolist (arg (cdr args))
        ;; remove any default arguments
        (setq arg (gpb-util-reduce-string arg "=.*$" ""))
        (insert (format "self.%s = %s\n" arg arg))
        (indent-according-to-mode))
      t)))

;; (defun gpb-py:try-to-indent ()
;;   (interactive)
;;   (when (bolp) (indent-for-tab-command) t))

;; (defadvice python-pdbtrack-set-tracked-buffer (after gpb-py:recenter-pdb
;;                                                      activate)
;;   (message "python-pdbtrack-set-tracked-buffer: %s" ad-return-value)
;;   (run-at-time 0 nil 'gpb-recenter-buffer-display ad-return-value))
;; (ad-unadvise 'python-pdbtrack-set-tracked-buffer)


(defun gpb-py:copy-breakpoint ()
  "Copy the command to set pdb breakpoint to the kill ring.

  The breakpoint will be set on the current line of the current
  buffer."
  (interactive)
  (let ((file-name (buffer-file-name))
        txt)
    (when (null file-name)
      (error "No file associated with buffer."))
  (setq txt (format "b %s:%s" (expand-file-name file-name) (line-number-at-pos)))
  (kill-new txt)
  (message "Copied \"%s\" to kill ring" txt)))

(defun gpb-py:copy-jump ()
  "Copy the command to jump to a line in pdb."
  (interactive)
  (let ((file-name (buffer-file-name))
        txt)
    (when (null file-name)
      (error "No file associated with buffer."))
  (setq txt (format "j %s" (line-number-at-pos)))
  (kill-new txt)
  (message "Copied \"%s\" to kill ring" txt)))

(defun gpb-py:run-flake8 ()
  "Run flake8 on the current buffer."
  (interactive)
  (when (or (not (buffer-modified-p))
            (and (y-or-n-p "Would you like to save this buffer ")
                 (or (save-buffer) t)))
    (let ((buf (get-buffer-create "*flake8*"))
          (cmd (format "setpy 64 && flake8 %s" (buffer-file-name)))
          (compilation-save-buffers-predicate
           `(lambda ()
              (eq (current-buffer) ,(current-buffer)))))
      (compile cmd))))

;; There seems to be some kind of bug such that ansi-color-context just
;; grows larger and larger.
(defadvice gpb:python-pdbtrack-comint-output-filter-function-advice
           (around python-pdbtrack-comint-output-filter-function)
  (let ((ansi-color-context nil))
    ad-do-it))
(ad-activate 'gpb:python-pdbtrack-comint-output-filter-function-advice)
