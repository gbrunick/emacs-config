(require 'python)
(require 'yasnippet)
(require 'elpy)

(elpy-enable)

(add-hook 'python-mode-hook 'gpb:python-mode-hook)

(setq python-shell-interpreter "jupyter"
      python-shell-interpreter-args "console --simple-prompt")

(add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter")

(add-hook 'python-shell-first-prompt-hook #'gpb:init-ipython)

;; Track PDB in *shell* buffers.
(add-hook 'comint-output-filter-functions
          'python-pdbtrack-comint-output-filter-function)


(defun gpb:init-ipython ()
  "Initial an IPython interpreter.

This is very similar to `python-shell-send-setup-code'."
  (let ((process (python-shell-get-process)))
    ;; Apparently we need to send this line as a single line.  If we send
    ;; it as part of a larger block of code, it gets wrapped in an execfile
    ;; and IPython ignores it.
    (python-shell-send-string
     "from __future__ import absolute_import, division, print_function"
     process)
    (python-shell-send-string
     "
import sys
if '' in sys.path:
    sys.path.remove('')
for path in list(sys.path):
    if r'h:' in path:
        sys.path.remove(path)
    elif r'Windows' in path:
        sys.path.remove(path)
    elif 'CoreApps' in path:
        sys.path.remove(path)

def page_printer(data, start=0, screen_lines=0, pager_cmd=None):
     if isinstance(data, dict):
         data = data['text/plain']
     print(data)

assert '_new_paths' not in locals()
_new_paths = [r'C:\\Data\\python\\src',
              r'Y:\\bld\\tfs\\ProdDeploy\\PDS\\Python\\Prod']
sys.path = _new_paths + sys.path
# del _new_paths

import IPython.core.page
IPython.core.page.page = page_printer

import IPython
assert '_ipython_shell' not in locals()
_ipython_shell = IPython.get_ipython()
_ipython_shell.InteractiveTB.set_mode('Plain')
_ipython_shell.run_code(r'from __future__ import absolute_import, division, print_function')
# del _ipython_shell

_test_val = 1/3

"
     process)
    (python-shell-accept-process-output process)))

(define-key python-mode-map "\C-cb" 'gpb-py:copy-breakpoint)
(define-key python-mode-map "\C-cj" 'gpb-py:copy-jump)
(define-key python-mode-map "\C-c8" 'gpb-py:run-flake8)
(define-key python-mode-map "\C-c$" 'ispell-comments-and-strings)
(define-key python-mode-map "\t" 'gpb-py:tab-command)

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


(defun gpb:python-mode-hook ()
  "Customize python-mode buffers."
  (set (make-local-variable 'tab-width) 4)
  (set (make-local-variable 'yas/key-syntaxes) (list "w_" "w_." "^ "))
  (set (make-local-variable 'fill-column) 75)
  (set (make-local-variable 'outline-regexp)
       "[[:space:]]*\\_<\\(?:class\\|def\\)\\_>\\|if +__name__ +==")
  (if (member 'gpb-text-objects features)
      (setq-local execute-text-object-function 'gpb:send-region))

  (make-local-variable 'ispell-skip-region-alist)
  (add-to-list 'ispell-skip-region-alist `("`[^`]" "`[^`]"))
  (add-to-list 'ispell-skip-region-alist `("``" "``"))
  (add-to-list 'ispell-skip-region-alist `("\\$" "\\$"))
  ;; Example: :func:`filter`
  (add-to-list 'ispell-skip-region-alist `(":[a-zA-Z]+:" "`[a-zA-Z_.]+`"))
  (yas-minor-mode 1))


(defun gpb:send-region (beg end)
  "Send regions from `beg` and `end` to Python shell."
  (let* ((region (elpy-shell--region-without-indentation beg end))
         ;; We prefix the string with extra lines, so that the line numbers
         ;; that appear in the traceback correspond to the correct position
         ;; in the file.
         (line-number (save-restriction (line-number-at-pos beg)))
         ;; IPython appears to show extra prompts when it encounters blank
         ;; lines, so we use commented out lines to pad the line number.
         (prefix (apply 'concat (make-list (- line-number 1) "# filler\n"))))
    (when (string-match "\t" region)
      (message "Region contained tabs, this might cause weird errors"))
    (python-shell-send-string (concat prefix region))))


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
                  yas-expand
                  gpb-company-try-to-complete-common
                  ;; gpb-py:try-to-complete
                  ;; gpb-py:try-completion
                  ;; indent-for-tab-command
                  )))
      ;; Try each command until we have a success
      (while (and cmds (not (call-interactively (car cmds))))
        (setq cmds (cdr cmds)))))))


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
