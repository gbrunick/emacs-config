;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  This file provides facilities for executing Python code from Emacs
;;
;;  The main interactive function is `gpb-py-exec:execute-code-command'.
;;  You may want to bind this to \C-c\C-C or something similar.  The main
;;  function for use in Emacs Lisp code is `gpb-py-exec:execute-code'.
;;
;;  There are a number of different `python-mode's for Emacs and they all
;;  handle inferior python interpreters slightly differently.  This code is
;;  implementation agnostic, so the user must define the variable
;;  `gpb-py-exec:execute-code-in-interpreter-function'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'cl))

(defvar gpb-py-exec:code-destination :comint
  "Where the code should be sent.\n
This variable should hold one of the following values:

:comint       execute code as a new process in a `comint-mode' buffer
:compile      execute code as a new process in a `compile-mode' buffer
:interpreter  send code to an existing inferior python interpreter process.")


(defvar gpb-py-exec:execute-code-shell-command "python \"%f\""
  "The command used to execute python code in a shell.\n
This variable is used by `gpb-py-exec:execute-code-in-shell-command'.")


(defvar gpb-py-exec:execute-code-shell-command-alist
  '("python \"%f\"" "python -m nose --pdb --pdb-failure -v" "ipython \"%f\"" "winpdb \"%f\"")
  "List of suggestions for `gpb-py-exec:execute-code-shell-command'")


(defvar gpb-py-exec:execute-code-in-interpreter-function nil
  "The function used to execute code in an inferior python interpreter.\n
This function should except a single argument that is a single
line of text with no terminating newline.  The function should
send this code to an inferior python interpreter and return the
buffer associated with this process.")

;; (defvar gpb-py-exec:jump-to-first-error nil)

(defvar gpb-py-exec:source-file nil)

(defvar gpb-py-exec:save-buffer-on-execution t)

(defvar gpb-py-exec:clear-output-buffer t)

(defvar gpb-py-exec:switch-to-interpreter-buffer nil)

(defvar gpb-py-exec:display-interpreter-buffer t)

(defvar gpb-py-exec:auto-jump-to-exception nil)

(defvar gpb-py-exec:execute-code-shell-command-history nil)

(defvar gpb-py-exec:working-directory nil)

(defvar gpb-py-exec:pythonpath nil
  "The PYTHONPATH environment variable is set to this string when
  it is non-nil.")

(defun gpb-py-exec:get-file-for-execution (&optional beg end)
  "Returns the filename that corresponds to buffer or region BEG END.\n
This is usually just the file name associated with the buffer,
but could also be a master file or a temporary region file.  If
you provide BEG you must also provide END."
  (cond
   ;; If mark is active, use the active region
   (beg
    (assert end)
    (gpb-py-exec:make-region-temp-file beg end))
   ;; If we are in an indirect buffer, go to the base buffer and preserve any narrowing
   ((buffer-base-buffer)
    (setq beg (or beg (point-min))
          end (or end (point-max)))
    (with-current-buffer (buffer-base-buffer)
      (gpb-py-exec:get-file-for-execution beg end)))
   ;; If another file has been specified
   (gpb-py-exec:source-file (expand-file-name gpb-py-exec:source-file))
   ;; If the current buffer is associated with a file, return this file.
   ((and (buffer-file-name) (not (buffer-modified-p)))
    (buffer-file-name))
   ;; Otherwise, treat the entire buffer as a region.
   (t
    (gpb-py-exec:make-region-temp-file (point-min) (point-max)))))


(defun gpb-py-exec:escape-string-for-python (str)
  "Returns a valid python string literal that evaluates to STR."
  (let ((str (replace-regexp-in-string "'''" "\"\"\"" str nil t)))
    (concat "r'''" str "'''")))


(defun gpb-py-exec:execute-code (&optional beg end)
  "Execute some python code.\n
Uses `gpb-py-exec:code-destination' to determine how to run the
code.  Uses `gpb-py-exec:execute-code-in-interpreter-function' to
execute code in the interpreter and
`gpb-py-exec:execute-code-shell-command' to execute code in an
inferior shell.  Returns the buffer where the code is being
executed, but does not display or switch to this buffer."
  (when (and gpb-py-exec:save-buffer-on-execution (buffer-file-name))
    (save-buffer))
  (let ((buffer (case gpb-py-exec:code-destination
                  (:interpreter
                   (gpb-py-exec:execute-code-in-interpreter beg end))
                  (:comint
                   (gpb-py-exec:execute-code-in-comint
                    gpb-py-exec:execute-code-shell-command beg end))
                  (:compile
                   (gpb-py-exec:execute-code-in-compile
                    gpb-py-exec:execute-code-shell-command beg end))
                  (t
                   (error "Not implemented")))))
    buffer))


(defun gpb-py-exec:execute-code-command (arg)
  "Execute some python code.\n
Opens a settings buffer when given a prefix argument."
  (interactive "P")
  (if arg (progn (require 'gpb-settings) (gpb-py-exec:show-settings))
    (let ((buffer (if (use-region-p)
                      (gpb-py-exec:execute-code (region-beginning) (region-end))
                    (gpb-py-exec:execute-code))))
      (cond
       ((and gpb-py-exec:display-interpreter-buffer
             gpb-py-exec:switch-to-interpreter-buffer)
        (let ((window (display-buffer buffer)))
          (select-frame-set-input-focus (window-frame window))
          (select-window window)))
       (gpb-py-exec:switch-to-interpreter-buffer
        (switch-to-buffer buffer))
       (gpb-py-exec:display-interpreter-buffer
        (display-buffer buffer))))))

  ;; (if (and (equal arg '(16)) (require 'gpb-settings nil t))
  ;;     (gpb-py-exec:show-settings)
  ;;   (let ((buffer (if (use-region-p)
  ;;                     (gpb-py-exec:execute-code (region-beginning) (region-end))
  ;;                   (gpb-py-exec:execute-code))))
  ;;     (cond
  ;;      ((equal arg '(4)) (pop-to-buffer buffer))
  ;;      (t (display-buffer buffer))))))


(defun gpb-py-exec:execute-code-in-interpreter (&optional beg end)
  "Execute code in an inferior python interpreter."
  (interactive)
  (let* ((beg-line (and beg (line-number-at-pos beg)))
         (end-line (and end (line-number-at-pos
                             (save-excursion (goto-char end)
                                             (when (bolp)
                                               (forward-line -1))
                                             (point)))))
         (code (or
                ;; If there is a single line of code, just execute that
                ;; line of code.
                (and beg-line end-line (= beg-line end-line)
                     (buffer-substring-no-properties
                      (save-excursion (goto-char beg)
                                      (back-to-indentation)
                                      (point))
                      (save-excursion (goto-char beg)
                                      (end-of-line 1)
                                      (point))))
                ;; If there is a region, convert it to a temporary file
                (and beg-line end-line
                     (format "execfile('%s') # %s %s-%s"
                             (gpb-py-exec:get-file-for-execution beg end)
                             (buffer-name) beg-line end-line))
                ;; Otherwise, execute the entire buffer
                (format "execfile('%s') # %s"
                        (gpb-py-exec:get-file-for-execution)
                        (buffer-name)))))
    (funcall gpb-py-exec:execute-code-in-interpreter-function code)))


(defun gpb-py-exec:execute-code-in-compile (shell-command &optional beg end)
  "Execute code SHELL-COMMAND in a stand alone shell.

Returns the buffer where the code is executed.  SHELL-COMMAND is
expanded usng `gpb-py-exec:expand-shell-command'."
  ;; This is a common mistake that I tend to make
  (let* ((src-buffer (buffer-name))
         (buffer-name (concat "*" (buffer-name) ": output*"))
         (compilation-auto-jump-to-first-error gpb-py-exec:auto-jump-to-exception)
         (compilation-mode-font-lock-keywords nil)
         (shell-command (gpb-py-exec:expand-shell-command
                         shell-command beg end)))
    (with-current-buffer (compilation-start shell-command nil
                                            (lambda (x) buffer-name))
      (set (make-local-variable 'compilation-auto-jump-to-first-error)
           gpb-py-exec:auto-jump-to-exception)
      (set (make-variable-buffer-local 'compilation-mode-font-lock-keywords) nil)
      (current-buffer))))

(defun gpb-py-exec:get-output-buffer-name ()
  (cond
   ((ignore-errors
      (= (string-match ".*python +-m +\\(.*\\)"
                       gpb-py-exec:execute-code-shell-command)
         0))

    (concat "*" (match-string 1 gpb-py-exec:execute-code-shell-command) "*"))
   (gpb-py-exec:source-file
      (concat "*" (file-name-nondirectory gpb-py-exec:source-file) ": output*"))
   (t
    (concat "*" (buffer-name) ": output*"))))

(defun gpb-py-exec:execute-code-in-comint (shell-command &optional beg end)
  "Execute code SHELL-COMMAND in a stand alone shell.

Returns the buffer where the code is executed.  SHELL-COMMAND is
expanded usng `gpb-py-exec:expand-shell-command'."
  ;; This is a common mistake that I tend to make
  (let* ((src-buffer (buffer-name))
         (buffer (get-buffer-create (gpb-py-exec:get-output-buffer-name)))
         (current-proc (get-buffer-process buffer))
         (shell-command (gpb-py-exec:expand-shell-command shell-command beg end))
         (cmdlist (split-string-and-unquote shell-command))
         (cwd gpb-py-exec:working-directory)
         (process-environment process-environment))
    ;; Now start process
    (with-current-buffer buffer
      (when cwd (setq default-directory cwd))
      (when (and (boundp 'python-shell-extra-pythonpaths)
                 python-shell-extra-pythonpaths)
        (setenv "PYTHONPATH"
                (concat (mapconcat #'identity
                                   python-shell-extra-pythonpaths ";")
                        ";"
                        (getenv "PYTHONPATH"))))
      (let ((inhibit-read-only t))
        (if gpb-py-exec:clear-output-buffer
            (erase-buffer)
          (progn
            (goto-char (point-max))
            (insert "\n")))
        (insert (format "Executing %s...\n\n" shell-command))
        (unless (derived-mode-p 'comint-mode) (comint-mode))
        (set (make-local-variable 'comint-scroll-to-bottom-on-output) nil)
        (set (make-local-variable 'comint-move-point-for-output) nil)
        (set (make-local-variable 'comint-scroll-show-maximum-output) nil)
        (set (make-local-variable 'compilation-mode-font-lock-keywords) nil)
        (add-hook 'comint-output-filter-functions
                  'python-pdbtrack-comint-output-filter-function t t)
        (make-local-variable 'python-pdbtrack-buffers-to-kill)
        (make-local-variable 'python-pdbtrack-tracked-buffer)
        (compilation-shell-minor-mode 1)
        (comint-exec (current-buffer) (format "Python: %s" src-buffer)
                     (car cmdlist) nil (cdr cmdlist))))
    buffer))


(defun gpb-py-exec:expand-shell-command (shell-command &optional beg end)
  "Expand placeholders in a shell command.

%f: The file to to execute.  This is generally the correct thing
    to use.
%F: The source filename.  This will differ from %f when the user
    is executing a region or a buffer with no file attached.
%l: The line number of the first line the source filename."
  (when (string-match "^ipython .* -u" shell-command)
    (error "The command %S will upgrade ipython"))
  (let ((file-name (gpb-py-exec:get-file-for-execution beg end)))
    (while (string-match (regexp-quote "%f") shell-command)
      (setq shell-command (replace-match file-name
                                         nil t shell-command))))
  (while (string-match (regexp-quote "%F") shell-command)
    (assert (buffer-file-name))
    (setq shell-command (replace-match (buffer-file-name)
                                       nil t shell-command)))
  (while (string-match (regexp-quote "%l") shell-command)
    (setq shell-command (replace-match (number-to-string (line-number-at-pos))
                                       nil t shell-command)))
  shell-command)


(defun gpb-py-exec:layout-settings-buffer ()
  (gpb-settings:insert-text "\n")
  (gpb-settings:insert-text "Execute python code in:\n\n")
  (dolist (symbol-label '((:comint "a comint buffer")
                          (:compile "a compile buffer")
                          (:interpreter "a python interpreter")))
    (multiple-value-bind (symbol label) symbol-label
      (gpb-settings:insert-text "   ")
      (gpb-settings:insert-radiobox
       (eq gpb-py-exec:code-destination symbol)
       `(lambda () (setq gpb-py-exec:code-destination ,symbol))
       label)
      (gpb-settings:insert-text "\n")))

  (when (member gpb-py-exec:code-destination '(:comint :compile))
    (gpb-settings:insert-text
     (concat "\n\nCode is executed by issuing the shell command: "
             (let ((len (length gpb-py-exec:execute-code-shell-command))
                   (min-width 20))
               (propertize
                (concat gpb-py-exec:execute-code-shell-command
                        (make-string (max 0 (- min-width len)) ?\ ))
                'face '((background-color . "grey90"))))))

    (gpb-settings:insert-text "\n\n")
    (dolist (cmd gpb-py-exec:execute-code-shell-command-alist)
      (gpb-settings:insert-text "   ")
      (gpb-settings:insert-radiobox
       (string-equal gpb-py-exec:execute-code-shell-command cmd)
       `(lambda () (setq gpb-py-exec:execute-code-shell-command ,cmd))
       cmd)
      ;; (gpb-settings:insert-text " ")
      ;; (gpb-settings:insert-text cmd)
      (gpb-settings:insert-text "\n"))
    (gpb-settings:insert-text "   ")
    (gpb-settings:insert-radiobox
     (not (member gpb-py-exec:execute-code-shell-command
                  gpb-py-exec:execute-code-shell-command-alist))
     `(lambda () (setq gpb-py-exec:execute-code-shell-command
                       (read-string
                        "Custom command: " nil
                        'gpb-py-exec:execute-code-shell-command-history)))
     "use custom command")
    (gpb-settings:insert-text
     "\n\n\nThe follow strings are expanded in a shell command:\n\n")
    (gpb-settings:insert-text
     "   %f - filename of buffer or region (use this)\n")
    (gpb-settings:insert-text "   %F - buffer filename\n")
    (gpb-settings:insert-text "   %l - first line number of region\n\n"))

  (gpb-settings:insert-text "\nAdditional options:\n")
  (gpb-settings:insert-text "\n   ")
  (gpb-settings:insert-checkbox gpb-py-exec:source-file
                                `(lambda (val)
                                     (setq gpb-py-exec:source-file
                                           (and val
                                               (read-file-name "Python source file: ")))))
  (if gpb-py-exec:source-file
      (gpb-settings:insert-text
       (concat " Execute file: "
               (when gpb-py-exec:source-file
                 (propertize gpb-py-exec:source-file
                             'face '((background-color . "grey90"))))))
    (gpb-settings:insert-text " Execute another file"))


  (gpb-settings:insert-text "\n   ")
  (gpb-settings:insert-checkbox gpb-py-exec:save-buffer-on-execution
                          `(lambda (val)
                             (setq gpb-py-exec:save-buffer-on-execution val)))
  (gpb-settings:insert-text " Save buffer on execution")

  (gpb-settings:insert-text "\n   ")
  (gpb-settings:insert-checkbox gpb-py-exec:display-interpreter-buffer
                          `(lambda (val)
                             (setq gpb-py-exec:display-interpreter-buffer val)))
  (gpb-settings:insert-text " Display interpreter buffer on execution")

  (gpb-settings:insert-text "\n   ")
  (gpb-settings:insert-checkbox gpb-py-exec:switch-to-interpreter-buffer
                          `(lambda (val)
                             (setq gpb-py-exec:switch-to-interpreter-buffer val)))
  (gpb-settings:insert-text " Switch to interpreter buffer on execution")

  (gpb-settings:insert-text "\n   ")
  (gpb-settings:insert-checkbox gpb-py-exec:clear-output-buffer
                                `(lambda (val)
                                   (setq gpb-py-exec:clear-output-buffer val)))
  (gpb-settings:insert-text " Clear output buffer")

  (gpb-settings:insert-text "\n   ")
  (gpb-settings:insert-checkbox gpb-py-exec:auto-jump-to-exception
                          `(lambda (val)
                             (setq gpb-py-exec:auto-jump-to-exception val)))
  (gpb-settings:insert-text " Automatically jump to exception location [not implemented yet]")

  (gpb-settings:insert-text "\n   ")
  (gpb-settings:insert-checkbox (not (null gpb-py-exec:working-directory))
                          `(lambda (val)
                             (if val
                                 (setq gpb-py-exec:working-directory
                                       (file-name-as-directory (read-directory-name "Working directory: ")))
                               (setq gpb-py-exec:working-directory nil))))
  (gpb-settings:insert-text
   (if gpb-py-exec:working-directory
       (concat " Execute script in directory: "
               (let ((len (length gpb-py-exec:working-directory))
                     (min-width 20))
                 (propertize
                  (concat gpb-py-exec:working-directory
                          (make-string (max 0 (- min-width len)) ?\ ))
                  'face '((background-color . "grey90")))))
     " Execute script in another directory"))
  (gpb-settings:insert-text "\n   ")

  (gpb-settings:insert-checkbox (not (null gpb-py-exec:pythonpath))
                                `(lambda (val)
                                   (if val
                                       (setq gpb-py-exec:pythonpath
                                             (read-string "New PYTHONPATH: "
                                                          (cons (getenv "PYTHONPATH") 0)))
                                     (setq gpb-py-exec:pythonpath nil))))
  (gpb-settings:insert-text
   (if gpb-py-exec:pythonpath
       (concat " PYTHONPATH: "
               (let ((len (length gpb-py-exec:pythonpath)) (min-width 20))
                 (propertize
                  (concat gpb-py-exec:pythonpath
                          (make-string (max 0 (- min-width len)) ?\ ))
                  'face '((background-color . "grey90")))))
     " Set the PYTHONPATH before execution."))


  (when (eq gpb-py-exec:code-destination :compile)
    (gpb-settings:insert-text "   ")
    (gpb-settings:insert-checkbox gpb-py-exec:auto-jump-to-exception
                            `(lambda (val)
                               (setq gpb-py-exec:auto-jump-to-exception val)))
    (gpb-settings:insert-text " Jump to the first exception")))


(defun gpb-py-exec:make-region-temp-file (start end)
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
                                 gpb-py-exec:make-region-temp-file:save-rfn)
                               (make-temp-file "region-" nil ".py")))
         (filename (expand-file-name
                    (or (buffer-file-name current-buffer)
                        region-file-name)))
         true-start line-of-code)
    ;; We reuse temp files to avoid littering the system with files
    (setq gpb-py-exec:make-region-temp-file:save-rfn region-file-name)
    (with-current-buffer (find-file-noselect region-file-name t)
      (erase-buffer)
      (insert
       (concat
        "# This region of code is taken from:\n"
        (format "# buffer: %s\n# lines: %s-%s\n\n"
                buffer-name first-code-line last-code-line)
        "from textwrap import dedent\n\n"
        (format "__file__ = \"%s\"\n" filename)
        "__name__ = '__main__'\n\n"
        (format "code = compile(%s * '\\n' + dedent(%s),\n"
                (1- first-code-line)
                (gpb-py-exec:escape-string-for-python (concat code "\n\n")))
        (format "filename='''%s''',\nmode='exec')\n\n" filename)
        "try:\n"
        "    exec code\n"
        "except Exception as exc:\n"
        "    from sys import exc_type, exc_value, exc_traceback\n"
        "    from traceback import print_exception\n"
        "    # We remove the top entry on the traceback because it points to\n"
        "    # this wrapper code.\n"
        "    tb = exc_traceback.tb_next\n"
        "    print_exception(exc_type, exc_value, tb)\n"))
      (save-buffer))
    region-file-name))

(defun gpb-py-exec:show-settings ()
  (interactive)
  (gpb-settings:make-settings-buffer 'gpb-py-exec:layout-settings-buffer))

(provide 'gpb-python-exec)
