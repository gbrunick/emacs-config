(require 'xml-rpc)
(eval-when-compile '(require cl))

(setenv "PYTHONUNBUFFERED" "True")

(defvar gpb-ipy:get-ipython-kernel-info-regex
  "--existing[ \t]+\\(kernel-[-a-zA-Z0-9.]+json\\)")

(defvar gpb-ipy:get-server-url-regex "\\(http://[-a-zA-Z0-9.:/]+\\)$")

(defvar gpb-ipy:ipython-server-source-filename "ipython_server.py")

(define-derived-mode gpb-ipython-inferior-mode comint-mode "IPython"
  (local-set-key "\t" 'gpb-ipy:ipython-tab-command)
  (local-set-key [(backtab)] 'gpb-ipy:ipython-backtab-command)
  (local-set-key "\C-c?" 'gpb-ipy:describe-object)
  (setq-local completion-at-point-functions
              '(gpb-ipy:completion-at-point-function))
  (add-hook 'kill-buffer-hook 'gpb-ipy:shutdown-ipython nil t)
  (when (fboundp 'python-pdbtrack-comint-output-filter-function)
    (add-hook 'comint-output-filter-functions
              'python-pdbtrack-comint-output-filter-function)))

(eval-after-load "yasnippet"
  '(yas/define-snippets
    'gpb-ipython-inferior-mode
    `(("dt" "import datetime as dt")
      ("np" "import numpy as np")
      ("pd" "import pandas as pd")
      ("plt" "import matplotlib.pyplot as plt")
      ("sns" "import seaborn as sns")
      ("wtf" "import wtforms as wtf")
      ("qt" "%matplotlib qt")
      ("qt4" "%matplotlib qt4"))))

(defvar gpb-ipy:current-error-marker nil)

(defvar gpb-ipy:truncate-output-code "\
import IPython.core.formatters

def _gpb_get_call_method():
    return IPython.core.formatters.PlainTextFormatter.__dict__['__call__']

if _gpb_get_call_method().__module__ == 'IPython.core.formatters':
    _gpb_save_call_method = _gpb_get_call_method()
del _gpb_get_call_method

def _gpb_format_as_text(self, obj):
    txt = _gpb_save_call_method(self, obj)
    max_width = 1000
    lines = [l if len(l) < max_width else l[:max_width] + '...'
             for l in txt.splitlines()]
    max_lines = 100
    if len(lines) > max_lines:
        n = int(max_lines / 2) - 3
        msg = 20 * ' ' + '[{} lines omitted]'.format(len(lines))
        lines = lines[:n] + ['', msg, ''] + lines[-n:]
    return '\\n'.join(lines)

setattr(IPython.core.formatters.PlainTextFormatter, '__call__',
        _gpb_format_as_text)
del _gpb_format_as_text

"
  "Code to truncate IPython output.  Too much output locks up Emacs.")

(defvar gpb-ipy:ipython-setup-code
  `("%colors NoColor"
    "%xmode Plain"
    "%load_ext autoreload"
    "%autoreload 1"
    "import numpy as np"
    "import pandas as pd"
    "import statsmodels.api as sm"
    "import matplotlib.pylot as plt"))

(defun gpb-ipy:run-ipython (&optional arg)
  "Run IPython as an inferior process.

Pass an argument force a process restart."
  (interactive "P")
  (if arg
      (let ((buf-names '("*IPython*" "*IPython (Kernel)*" "*IPython (IPC)*")))
        (while buf-names
          (with-current-buffer (car buf-names)
            (ignore-errors (kill-process))
            (kill-buffer))
          (setq buf-names (cdr buf-names)))))
  (let ((buf (gpb-ipy:get-ipython-buffer t)))
    (switch-to-buffer buf)))


(defun gpb-ipy:shutdown-ipython ()
  ;; Try to shut down IPython gracefully.
  (ignore-errors (gpb-ipy:send-string "exit"))
  (let ((count 5))
    (while (> count 0)
      (goto-char (point-max))
      (if (looking-back "finished[ \n]*")
          (setq count 0)
        (sleep-for 0.2)
        (setq count (1- count)))))
  ;; Kill off all the supporting buffers.
  (dolist (buf (list gpb-ipy:ipc-buffer gpb-ipy:kernel-buffer))
    (ignore-errors
      (with-current-buffer buf
        (ignore-errors (kill-process))
        (kill-buffer)))))


(defun kill-ipython-buffers ()
  "Convenience command for debugging."
  (interactive)
  (let* ((base-buffer-name "IPython")
         (buffer-name (format "*%s*" base-buffer-name))
         (ipc-buffer-name (format "*%s (IPC)*" base-buffer-name))
         (kernel-buffer-name (format "*%s (Kernel)*" base-buffer-name)))
    (dolist (buf-name (list buffer-name ipc-buffer-name kernel-buffer-name))
      (let* ((buf (get-buffer buf-name)))
        (when buf
          (while (and buf (get-buffer-process buf))
            (delete-process (get-buffer-process buf)))
          (with-current-buffer buf
            (remove-hook 'kill-buffer-hook 'gpb-ipy:shutdown-ipython)
            (remove-hook 'kill-buffer-hook 'gpb-ipy:shutdown-ipython t)
            (kill-buffer buf)))))))


(defun gpb-ipy:get-ipython-buffer (&optional create)
  (let* ((buf (get-buffer "*IPython*")))
    (if (and buf (comint-check-proc buf))
        buf
      (gpb-ipy:start-ipython "IPython"))))


(defun gpb-ipy:start-ipython (&optional cmd)
  "Start a new ipython process and return the associated buffer."
  (interactive
   (let ((str (read-string "Command (ipython kernel): " nil
                           (if (boundp 'gpb-ipy:run-ipython:cmd-hist)
                               'gpb-ipy:run-ipython:cmd-hist
                             (setq gpb-ipy:run-ipython:cmd-hist nil)
                             'gpb-ipy:run-ipython:cmd-hist))))
     (list (if (equal str "") "ipython kernel" str))))
  (let* ((base-buffer-name "IPython")
         (buffer-name (format "*%s*" base-buffer-name))
         (ipc-buffer-name (format "*%s (IPC)*" base-buffer-name))
         (kernel-buffer-name (format "*%s (Kernel)*" base-buffer-name)))

    ;; Kill any processes living in any of the buffers
    (dolist (bn (list buffer-name ipc-buffer-name kernel-buffer-name))
      (let* ((buf (get-buffer bn)))
        (while (and buf (get-buffer-process buf))
          (delete-process (get-buffer-process buf)))))

    ;; Now spin up new processes
    (let* ((process-environment (append '("PYTHONUNBUFFERED=True" "PAGER=cat")
                                        process-environment))
           (kernel-buffer
            ;; (make-comint-in-buffer
            ;;  "ipython-kernel" kernel-buffer-name
            ;;  "ipython" nil "kernel"))
            (cond
             ((eq system-type 'gnu/linux)
              (make-comint-in-buffer
               "ipython-kernel" kernel-buffer-name  "bash" nil "-c"
               (or cmd "ipython kernel")))
             ((eq system-type 'windows-nt)
              (make-comint-in-buffer
               "ipython-kernel" kernel-buffer-name "cmd.exe" nil "/c"
               (or cmd "ipython kernel")))
             (t
              (error "Unhandled system-type."))))
           (kernel-proc (get-buffer-process kernel-buffer))
           (frontend-buffer (get-buffer-create buffer-name))
           (ipc-buffer (make-comint-in-buffer
                        "ipython-ipc" ipc-buffer-name "python" nil
                        (locate-library gpb-ipy:ipython-server-source-filename)))
           (ipc-process (get-buffer-process ipc-buffer)))

      (switch-to-buffer frontend-buffer)

      (with-current-buffer kernel-buffer
        (save-excursion
          (goto-char 1)
          (insert-before-markers
           (format "%S\n" (process-command kernel-proc)))))

      ;; Set up interpreter buffer
      (with-current-buffer frontend-buffer
        (cond
         ;; ((fboundp 'inferior-python-mode)
         ;;  (setq-local python-shell-setup-codes nil)
         ;;  (put 'python-shell-setup-codes 'permanent-local t)
         ;;  (inferior-python-mode))
         (t
          (unless (derived-mode-p 'comint-mode) (gpb-ipython-inferior-mode))
          (setq-local compilation-mode-font-lock-keywords nil)
          (make-local-variable 'python-pdbtrack-buffers-to-kill)
          (make-local-variable 'python-pdbtrack-tracked-buffer)
          (compilation-shell-minor-mode 1)
          ;; (error "Not sure which python inferior process mode to use.")
          ))
        ;; Setup completion interface
        (company-mode 1)
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert (format "IPython kernel command: %s\n" cmd))
        (insert "Waiting for kernel to start up...\n")
        (goto-char (point-max))
        (setq-local gpb-ipy:kernel-buffer kernel-buffer)
        (setq-local gpb-ipy:ipc-buffer ipc-buffer)
        (setq-local gpb-ipy:ipc-url nil)
        (setq-local gpb-ipy:kernel-info nil)
        (setq-local comint-scroll-to-bottom-on-output nil)
        (setq-local next-error-function 'gpb-ipy:next-error-function)
        (add-hook 'comint-output-filter-functions
                  'gpb-comint:split-long-lines nil t))

      ;; Setup kernel process and buffer
      (with-current-buffer kernel-buffer
        (setq-local gpb-ipy:frontend-buffer frontend-buffer)
        (add-hook 'comint-output-filter-functions
                  'gpb-ipy:get-kernel-info-output-filter t t)
        (set-process-query-on-exit-flag kernel-proc nil))

      ;; Setup IPC process and buffer
      (with-current-buffer ipc-buffer
        (setq-local gpb-ipy:frontend-buffer frontend-buffer)
        (add-hook 'comint-output-filter-functions
                  'gpb-ipy:get-ipc-url-filter t t)
        (set-process-query-on-exit-flag ipc-process nil))

      ;; Return interpreter buffer
      frontend-buffer)))


(defun gpb-ipy:get-kernel-info-output-filter (string)
  "Scan kernel output for connection information.\n

This is a filter for use with `comint-output-filter-functions'.
It watches the output in the kernel buffer until is sees the
information that is needs to connect to the newly started
terminal."
  (when (string-match gpb-ipy:get-ipython-kernel-info-regex string)
    (let ((proc (get-buffer-process (current-buffer))))
      (funcall (process-filter proc) proc
               "[Emacs] Found kernel information.\n"))
    (gpb-log-forms 'gpb-ipy:get-kernel-info-output-filter
                   'string 'gpb-ipy:get-ipython-kernel-info-regex
                   '(match-string-no-properties 1 string))
    (let* ((kernel-info (match-string-no-properties 1 string))
           (process-environment (append '("PYTHONUNBUFFERED=True" "PAGER=cat")
                                        process-environment)))
      (make-comint-in-buffer
       "ipython-frontend" gpb-ipy:frontend-buffer "ipython" nil
       "console" "--existing" kernel-info)
      (remove-hook 'comint-output-filter-functions
                   'gpb-ipy:get-kernel-info-output-filter t)
      (with-current-buffer gpb-ipy:frontend-buffer
        (setq-local gpb-ipy:kernel-info kernel-info)
        (set-process-query-on-exit-flag
         (get-buffer-process (current-buffer)) nil)
        (gpb-log-forms 'gpb-ipy:get-kernel-info-output-filter
                       'gpb-ipy:ipc-url
                       'gpb-ipy:kernel-info)
        (if gpb-ipy:ipc-url (gpb-ipy:setup-ipc-connection))))))


(defun gpb-ipy:get-ipc-url-filter (string)
  "Scan the output from the completion server process for url information.\n
This filter is for use with `set-process-filter'."
  (when (string-match gpb-ipy:get-server-url-regex string)
    (let ((proc (get-buffer-process (current-buffer))))
      (funcall (process-filter proc) proc "[Emacs] Found url information.\n"))
    (remove-hook 'comint-output-filter-functions 'gpb-ipy:get-ipc-url-filter t)
    (with-current-buffer gpb-ipy:frontend-buffer
      (setq-local gpb-ipy:ipc-url (match-string-no-properties 1 string))
      (gpb-log-forms 'gpb-ipy:get-ipc-url-filter
                     'gpb-ipy:ipc-url
                     'gpb-ipy:kernel-info)
      (if gpb-ipy:kernel-info (gpb-ipy:setup-ipc-connection)))))


(defun gpb-ipy:setup-ipc-connection ()
  (gpb-ipy:call-rpc-function 'connect gpb-ipy:kernel-info)
  (dolist (code gpb-ipy:ipython-setup-code)
    (gpb-ipy:send-string code t))
  (let* ((comint-input-ring-file-name (concat (file-name-as-directory
                                               temporary-file-directory)
                                              "ipython-history-dump"))
         (comint-input-ring-separator "\n")
         (cmd (format "%%history -l %s -f \"%s\""
                      comint-input-ring-size comint-input-ring-file-name)))
    (if (file-exists-p comint-input-ring-file-name)
        (delete-file comint-input-ring-file-name))
    (gpb-ipy:send-string cmd t)
    (comint-read-input-ring)))


(defun gpb-ipy:send-string (string &optional silent)
  "Send a single line of text to the python interpreter.

Returns the buffer that contains the python interpreter to which
the string was sent. If SILENT is non-nil, do not show the string
or output in the interpreter buffer.  This is mainly useful for
sending initialization code to the interpreter without cluttering
up the interpreter buffer."
  (interactive
   (list (read-string "Python statement: "
                      (buffer-substring-no-properties
                       (save-excursion (back-to-indentation) (point))
                       (save-excursion (end-of-line) (point))))))
  (let ((buf (gpb-ipy:get-ipython-buffer)))
    (cond
     ((null buf)
      (error "No ipython interpreter buffer"))
     (silent
      (gpb-ipy:call-rpc-function 'execute-code string))
     (t
      (save-excursion
        (with-current-buffer buf
          (setq gpb-ipy:current-error-marker nil)
          (compilation-forget-errors)
          (goto-char (process-mark (get-buffer-process buf)))
          (let ((current-text (buffer-substring (point) (point-max))))
            (delete-region (point) (point-max))
            (unwind-protect
                (progn (insert string) (comint-send-input))
              (insert current-text)))))
      buf))))


(defun gpb-ipy:next-error-function (arg reset)
  (if (= arg 0)
      (compilation-next-error-function 0 reset)
    (let ((buf (gpb-ipy:get-ipython-buffer)) pt)
      (unless buf (error "No IPython buffer"))
      (save-excursion
        (with-current-buffer buf
          (goto-char
           (or (if (and (not reset)
                        gpb-ipy:current-error-marker
                        (eq (marker-buffer gpb-ipy:current-error-marker)
                            buf))
                   gpb-ipy:current-error-marker)
               (process-mark (get-buffer-process buf))))
          (compilation-next-error arg)
          (setq gpb-ipy:current-error-marker (point-marker))))
      (let ((compilation-current-error gpb-ipy:current-error-marker))
        (compilation-next-error-function 0 nil)))))


(defun gpb-ipy:debug-ipc-server ()
  "Send the ipc server into an IPython REPL loop for debugging purposes."
  (interactive)
  (let (buf url (url-show-status-save url-show-status))
    (setq buf (gpb-ipy:get-ipython-buffer))
    (unless buf (error "No IPython buffer"))
    (with-current-buffer buf
      (setq url (and (boundp 'gpb-ipy:ipc-url) gpb-ipy:ipc-url)))
    (unless url (error "Waiting for ipc server to start"))
    (unwind-protect
        (progn
          (setq url-show-status nil)
          (xml-rpc-method-call-async 'identity url 'debug))
      (setq url-show-status url-show-status-save))))

(defun gpb-ipy:call-rpc-function (func &rest args)
  (let ((url gpb-ipy:ipc-url) ;; Buffer local
        (url-show-status-save url-show-status))
    (unwind-protect
        (progn
          (setq url-show-status nil)
          (if url
              (apply 'xml-rpc-method-call url func args)
            (error "Waiting for ipc server to start")))
      (setq url-show-status url-show-status-save))))

(defvar gpb-ipy:get-ipython-completions-cache nil
  "Used for memoization of `gpb-ipy:get-ipython-completions'.")


(defun gpb-ipy:get-ipython-completions (process line)
  (setq gpb-ipy:get-ipython-completions-cache
        (and (boundp 'gpb-ipy:get-ipython-completions-cache)
             gpb-ipy:get-ipython-completions-cache))
  (let* ((prev-proc (first gpb-ipy:get-ipython-completions-cache))
         (prev-line (second gpb-ipy:get-ipython-completions-cache))
         (prev-line-length (and prev-line (length prev-line)))
         (beg-last-word (let ((beg 0))
                          (while (string-match "[ \t]" line beg)
                            (setq beg (match-end 0)))
                          beg)))
    (if (and (equal process prev-proc)
             ;; Is the current line longer than the cached line
             (>= (length line) prev-line-length)
             ;; Does the last symbol in the cached line agree with the last
             ;; symbol in the new line?  For example, if "imp" is cached,
             ;; we don't want to use the cached completions for "import o".
             (>= prev-line-length beg-last-word)
             (string-equal (substring line 0 prev-line-length) prev-line))
        (progn
          (message "Cache hit")
          (third gpb-ipy:get-ipython-completions-cache))
      (let ((response (gpb-ipy:call-rpc-function 'complete line)))
        (gpb-log-forms 'gpb-ipy:get-ipython-completions 'response)
        (setq response (mapcar (lambda (s)
                                 (let ((parts (split-string s "/")))
                                   (if (string-equal (car-safe (last parts)) "")
                                       (mapconcat 'identity (last parts 2) "/")
                                     (car-safe (last parts)))))
                               response))
        (gpb-log-forms 'gpb-ipy:get-ipython-completions 'response)
        response))))


(defvar gpb-ipy:describe-object-cache nil)

(defun gpb-ipy:describe-object (object-name)
  (interactive
   (let* ((obj (format "%s" (cond
                             ((boundp 'python-dotty-syntax-table)
                              (with-syntax-table python-dotty-syntax-table
                                (word-at-point)))
                             (t (word-at-point)))))
          (enable-recursive-minibuffers t)
          val)
     (setq val (read-string (if (and obj (not (string-equal obj "")))
                                (format "Describe object (default %s): " obj)
                              "Describe object: ")))
     (list (if (equal val "") obj val))))

  (multiple-value-bind (cache-object-name info-alist)
      gpb-ipy:describe-object-cache
    (unless (string-equal object-name cache-object-name)
      (let ((buf (gpb-ipy:get-ipython-buffer)))
        (unless buf (error "No IPython buffer."))
        (with-current-buffer buf
          (setq info-alist (gpb-ipy:call-rpc-function 'get-info object-name)
                gpb-ipy:describe-object-cache (list object-name info-alist)))))

    ;; INFO-ALIST now contains information about OBJECT-NAME
    (help-setup-xref (list #'gpb-ipy:describe-object object-name)
                     (called-interactively-p 'interactive))
    (save-excursion
      (with-help-window (help-buffer)
        (with-current-buffer standard-output
          (insert (propertize "Object:" 'face 'font-lock-function-name-face
                              'font-lock-face 'font-lock-function-name-face))
          (insert (format " %s\n" object-name))
          (insert (propertize "Type:" 'face 'font-lock-function-name-face
                              'font-lock-face 'font-lock-function-name-face))
          (insert (format " %s\n" (aget info-alist "type_name")))
          (let ((file (aget info-alist "file" t)))
            (when file
              (insert
               (propertize "File:" 'face 'font-lock-function-name-face
                           'font-lock-face 'font-lock-function-name-face))
              (insert (format " %s\n" file))))
          (insert (format "\n%s" (aget info-alist "docstring")))
          ;; Return the text we displayed.
          (buffer-string))))))

(defun gpb-ipy:ipython-tab-command (arg)
  (interactive "P")
  (let* ((proc (get-buffer-process (current-buffer)))
         (proc-mark (ignore-errors (process-mark proc))))
  (cond
   ;; ((and proc-mark (> (point) proc-mark) arg)
   ;;  (gpb-ipy:ipython-complete-at-point-in-window)
   ;;  (next-completion 1))
   ((and proc-mark (> (point) proc-mark))
    (or (let ((yas/fallback-behavior 'return-nil)
              (yas/indent-line 'fixed))
          (call-interactively 'yas/expand))
        (completion-at-point)))
    ;; (gpb-ipy:ipython-complete-at-point)
    ;; (gpb-ipy:ipython-company-complete-at-point)
   ((get-text-property (point) 'keymap)
    (condition-case exc (gpb-ipy:forward-text-property 2 'keymap)
      (error (goto-char (point-max)))))
   (t
    (condition-case exc (gpb-ipy:forward-text-property 1 'keymap)
      (error (goto-char (point-max))))))))


(defun gpb-ipy:forward-text-property (count prop)
  (while (> count 0)
    (decf count)
    (goto-char (next-single-char-property-change (point) prop)))
  (while (< count 0)
    (incf count)
    (goto-char (previous-single-char-property-change (point) prop))))


(defun gpb-ipy:ipython-backtab-command (arg)
  (interactive "P")
  (let* ((proc (get-buffer-process (current-buffer)))
         (proc-mark (ignore-errors (process-mark proc))))
  (cond
   ((and proc-mark (> (point) proc-mark))
    (gpb-ipy:ipython-complete-at-point))
   ((get-text-property (max (1- (point)) (point-min)) 'keymap)
    (condition-case exc (gpb-ipy:forward-text-property -3 'keymap)
      (error (goto-char (point-max)))))
   (t
    (condition-case exc (gpb-ipy:forward-text-property -2 'keymap)
      (error (goto-char (point-max))))))))


(defun gpb-ipy:ipython-company-complete-at-point ()
  (interactive)
  (company-begin-backend 'gpb-ipy:ipython-complete-company-interface)
  (ignore-errors (company-complete-common)))

(defun gpb-ipy:completion-at-point-function ()
  (let* ((proc (get-buffer-process (current-buffer)))
         (proc-mark (ignore-errors (process-mark proc))))
    (list
     (save-excursion (skip-chars-backward "a-zA-Z_0-9.") (point))
     (point)
     (gpb-ipy:get-ipython-completions
      proc (buffer-substring-no-properties proc-mark (point))))))

(defun gpb-ipy:ipython-complete-at-point ()
  (interactive)
  (completion-in-region
   (save-excursion (skip-chars-backward "a-zA-Z.") (point))
   (point)
   (gpb-ipy:get-ipython-completions
    proc (buffer-substring-no-properties proc-mark (point)))))


(defun gpb-ipy:ipython-complete-at-point-in-window ()
  (interactive)
  (ignore-errors (kill-buffer "*Completions*"))
  (completion-in-region
   (save-excursion (skip-chars-backward "a-zA-Z.") (point))
   (point)
   (gpb-ipy:get-ipython-completions
    proc (buffer-substring-no-properties proc-mark (point))))
  (when (get-buffer "*Completions*")
    (pop-to-buffer "*Completions*")))


(defun gpb-ipy:ipython-complete-company-interface (command &optional arg)
  "Interface to company mode."
  (case command
    (prefix
     (let ((beg (if (save-excursion (re-search-backward "[ \t\n/]" nil t))
                    (match-end 0)
                  (point))))
       (cond
        ((< beg (point)) (buffer-substring-no-properties beg (point)))
        ((looking-back "[/]") ""))))
    (candidates
     (let ((proc (get-buffer-process (current-buffer))))
       (gpb-ipy:get-ipython-completions proc
                                       (buffer-substring-no-properties
                                        (save-excursion (beginning-of-line) (point))
                                        (point)))))))


(defun gpb-ipy:escape-string-for-python (str)
  "Returns a valid python string literal that evaluates to STR."
  (let ((str (replace-regexp-in-string "'''" "\"\"\"" str nil t)))
    (concat "r'''" str "'''")))


(defun gpb-ipy:make-region-temp-file (start end)
  "Save the code region as a temp file for execution and return filename.

Constructs a thin wrapper around the code block to make exception
reporting more natural."
  (interactive "r")
  (let* ((current-buffer (current-buffer))
         (buffer-name (buffer-name current-buffer))
         (first-code-line (line-number-at-pos start))
         ;; We don't include the last line if it is the start of a new line.
         (last-code-line (line-number-at-pos (1- end)))
         (code (buffer-substring-no-properties
                ;; Ensure that we only send whole lines of code.
                (save-excursion (goto-char start) (forward-line 0) (point))
                (save-excursion (goto-char end)
                                (unless (bolp) (end-of-line 1))
                                (point))))
         ;; The following is essentially static variables
         (region-file-name (or (ignore-errors
                                 gpb-ipy:make-region-temp-file:save-rfn)
                               (setq gpb-ipy:make-region-temp-file:save-rfn
                                     (concat temporary-file-directory
                                             "emacs_region.py"))))
         (filename (ignore-errors
                     (expand-file-name (buffer-file-name current-buffer)))))
    ;; We reuse temp files to avoid littering the system with files
    (with-current-buffer (find-file-noselect region-file-name t)
      (erase-buffer)
      (insert
       (concat
        "# This region of code is taken from:\n"
        (format "# buffer: %s\n# lines: %s-%s\n\n"
                buffer-name first-code-line last-code-line)
        "from textwrap import dedent\n\n"
        (if filename (format "__file__ = \"%s\"\n" filename) "")
        "__name__ = '__main__'\n\n"
        (format "code = compile(%s * '\\n' + dedent(%s),\n"
                ;; There is no file, just line up with the sting in the
                ;; temporary file.
                (if filename first-code-line 8)
                (gpb-ipy:escape-string-for-python (concat "\n" code "\n\n")))
        (if filename (format "filename='''%s''',\nmode='exec')\n\n" filename)
          (format "filename='''%s''',\nmode='exec')\n\n" region-file-name))
        "try:\n"
        "    exec code\n"
        "except Exception as exc:\n"
        "    from sys import exc_type, exc_value, exc_traceback\n"
        "    from traceback import print_exception\n"
        (if filename
            (concat
             "    # We remove the top entry on the traceback because it\n"
             "    # points to this wrapper code.\n"
             "    exc_traceback = exc_traceback.tb_next\n") "")
        "    print_exception(exc_type, exc_value, exc_traceback.tb_next)\n"))
      (save-buffer))
    region-file-name))


(defun gpb-ipy:execute-region (&optional beg end)
  "Execute code in an inferior python interpreter.

Always includes entire lines of text."
  (interactive "r")
  (let* ((beg (or beg (point-min)))
         (end (or end (point-max)))
         (code (or
                ;; If there is a single line of code, just execute that
                ;; line of code with no indentation.
                (and (save-excursion
                       (goto-char beg) (end-of-line) (>= (point) end))
                     (buffer-substring-no-properties
                      (save-excursion
                        (goto-char beg) (back-to-indentation) (point))
                      (save-excursion
                        (goto-char end) (end-of-line) (point))))
                ;; If we are executing the entire buffer, and the file
                ;; associated with the buffer is up to date, execute this
                ;; file.
                (save-restriction
                  (widen)
                  (and (= beg (point-min)) (= end (point-max))
                       (buffer-file-name) (not (buffer-modified-p))
                       (format "execfile(r'''%s''') # %s"
                               (buffer-file-name) (buffer-name))))
                ;; Otherwise, create and execute a temporary file.
                (format "run %s-i %s # %s %s-%s"
                        (if current-prefix-arg "-d " "")
                        (gpb-ipy:make-region-temp-file beg end)
                        (buffer-name)
                        (line-number-at-pos beg)
                        ;; We don't include the last line if it is the
                        ;; start of a new line.
                        (line-number-at-pos (1- end))))))
    (gpb-ipy:send-string code)))



(provide 'gpb-ipython)

