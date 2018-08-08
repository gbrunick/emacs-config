(require 'xml-rpc)

(eval-when-compile '(require cl))

(defvar gpb-py:get-ipython-kernel-info-regex "--existing \\([-a-zA-Z0-9.]+\\)")

(defvar gpb-py:get-server-url-regex "\\(http://[-a-zA-Z0-9.:/]+\\)$")

(defvar gpb-py:ipython-server-source-filename "ipython_completion_server.py")

(defvar gpb-ipython-completion-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'gpb-py:ipython-tab-command)
    (define-key map [(backtab)] 'gpb-py:ipython-backtab-command)
    map))


(define-minor-mode gpb-ipython-completion-mode
  "A minor mode completion in an ipython buffer.

This minor mode must be activated by a `inferior-python-mode-hook'
function to be successful.  This mode depends upon and activates
`company-mode'.

This minor mode communicates with the primary IPython process
using the secondary python script \"ipython_completion_server.py\"
which should be located in the emacs `load-path'.

This mode defines the following keys:
\\{gpb-ipython-completion-mode-map}"
  :keymap gpb-ipython-completion-mode-map
  (cond
   ;; Activate
   (gpb-ipython-completion-mode
    (company-mode 1)
    ;; We add a filter function to look for the informatin that we need to
    ;; connect to the ipython kernel process.
    (add-hook 'comint-output-filter-functions
              'gpb-py:get-kernel-info-output-filter nil t))
   ;; Deactivate
   (t
    (company-mode -1)
    (remove-hook 'comint-output-filter-functions
                 'gpb-py:get-kernel-info-output-filter t))))

(defun gpb-py:call-rpc-function (func &rest args)
  (let ((url (process-get (get-buffer-process (current-buffer)) :xml-rpc-url))
        (url-show-status nil))
    (if url
        (apply 'xml-rpc-method-call url func args)
      (error "Waiting for completion server to start"))))

(defvar gpb-py:get-ipython-completions-cache nil
  "Used for memoization of `gpb-py:get-ipython-completions'.")

(defun gpb-py:get-ipython-completions (process line)
  (setq gpb-py:get-ipython-completions-cache
        (and (boundp 'gpb-py:get-ipython-completions-cache)
             gpb-py:get-ipython-completions-cache))
  (let* ((prev-proc (first gpb-py:get-ipython-completions-cache))
         (prev-line (second gpb-py:get-ipython-completions-cache))
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
          (third gpb-py:get-ipython-completions-cache))
      (gpb-py:call-rpc-function 'complete line))))
      ;; (let ((url (process-get process :xml-rpc-url))
      ;;       (url-show-status nil))
      ;;   (if url
      ;;       (let ((result (xml-rpc-method-call url 'complete line)))
      ;;         (setq gpb-py:get-ipython-completions-cache (list process
      ;;                                                          line result))
      ;;         result)
      ;;     (error "Waiting for completion server to start"))))))


(defun gpb-py:get-kernel-info-output-filter (string)
  "Scan output for information about how to connect to the ipython kernel.

This is a filter for use with `comint-output-filter-functions'."
  (when (string-match gpb-py:get-ipython-kernel-info-regex string)
    (gpb-log-forms 'gpb-py:get-kernel-info-output-filter
                   'string 'gpb-py:get-ipython-kernel-info-regex
                   '(match-string-no-properties 1))
    (let* ((ipython-proc (get-buffer-process (current-buffer)))
           (kernel-info (match-string-no-properties 1 string))
           (buf (generate-new-buffer (format "*IPython-IPC:%s*" kernel-info)))
           (server-proc (start-process
                         "*ipython completions*" buf "python"
                         (locate-library gpb-py:ipython-server-source-filename)
                         kernel-info)))
      (set-process-filter server-proc 'gpb-py:get-server-url-filter)
      (set-process-query-on-exit-flag server-proc nil)
      (process-put ipython-proc :kernel-info kernel-info)
      (process-put ipython-proc :completion-server server-proc)
      (process-put ipython-proc :completion-server-buffer buf)
      (process-put server-proc :ipython-process ipython-proc)))
  string)


(defun gpb-py:get-server-url-filter (process string)
  "Scan the output from the completion server process for url information.

The is filter for use with `set-process-filter'."
  (with-current-buffer (process-buffer process)
    (save-excursion
      (goto-char (point-max))
      (insert string))
    (when (string-match gpb-py:get-server-url-regex string)
      (process-put (process-get process :ipython-process)
                   :xml-rpc-url (match-string-no-properties 1 string)))))


(defun gpb-py:ipython-tab-command (arg)
  (interactive "P")
  (let* ((proc (get-buffer-process (current-buffer)))
         (proc-mark (ignore-errors (process-mark proc))))
  (cond
   ((and proc-mark (> (point) proc-mark) arg)
    (gpb-py:ipython-complete-at-point-in-window)
    (next-completion 1))
   ((and proc-mark (> (point) proc-mark))
    (gpb-py:ipython-company-complete-at-point))
   ((get-text-property (point) 'keymap)
    (condition-case exc (gpb-py:forward-text-property 2 'keymap)
      (error (goto-char (point-max)))))
   (t
    (condition-case exc (gpb-py:forward-text-property 1 'keymap)
      (error (goto-char (point-max))))))))


(defun gpb-py:forward-text-property (count prop)
  (while (> count 0)
    (decf count)
    (goto-char (next-single-char-property-change (point) prop)))
  (while (< count 0)
    (incf count)
    (goto-char (previous-single-char-property-change (point) prop))))


(defun gpb-py:ipython-backtab-command (arg)
  (interactive "P")
  (let* ((proc (get-buffer-process (current-buffer)))
         (proc-mark (ignore-errors (process-mark proc))))
  (cond
   ((and proc-mark (> (point) proc-mark))
    (gpb-py:ipython-complete-at-point))
   ((get-text-property (max (1- (point)) (point-min)) 'keymap)
    (condition-case exc (gpb-py:forward-text-property -3 'keymap)
      (error (goto-char (point-max)))))
   (t
    (condition-case exc (gpb-py:forward-text-property -2 'keymap)
      (error (goto-char (point-max))))))))


(defun gpb-py:ipython-company-complete-at-point ()
  (interactive)
  (company-begin-backend 'gpb-py:ipython-complete-company-interface)
  (company-complete-common))


(defun gpb-py:ipython-complete-at-point ()
  (interactive)
  (completion-in-region
   (save-excursion (skip-chars-backward "a-zA-Z.") (point))
   (point)
   (gpb-py:get-ipython-completions
    proc (buffer-substring-no-properties proc-mark (point)))))


(defun gpb-py:ipython-complete-at-point-in-window ()
  (interactive)
  (ignore-errors (kill-buffer "*Completions*"))
  (completion-in-region
   (save-excursion (skip-chars-backward "a-zA-Z.") (point))
   (point)
   (gpb-py:get-ipython-completions
    proc (buffer-substring-no-properties proc-mark (point))))
  (when (get-buffer "*Completions*")
    (pop-to-buffer "*Completions*")))


(defun gpb-py:ipython-complete-company-interface (command &optional arg)
  "Interface to company mode."
  (case command
    (prefix
     (let ((beg (save-excursion
                  (skip-chars-backward "a-zA-Z0-9.%_")
                  (point))))
       (when (< beg (point)) (buffer-substring-no-properties beg (point)))))
    (candidates
     (let ((proc (get-buffer-process (current-buffer))))
       (gpb-py:get-ipython-completions proc
                                (buffer-substring-no-properties
                                 (save-excursion (beginning-of-line) (point))
                                 (point)))))))

;; (defadvice comint-output-filter (before deubg-info (process string) activate)
;;   (message "comint-output-filter: %S %S" process string))

(defun gpb-py:run-ipython ()
  "Run IPython as an inferior process"
  (interactive)
  (let* ((buffer-name "*IPython*")
         (process-environment process-environment))
    (setenv "PYTHONUNBUFFERED" "True")
    (switch-to-buffer
     (or (and (comint-check-proc buffer-name) buffer-name)
         (make-comint-in-buffer "ipython" buffer-name "ipython" nil
                                "console")))
    (gpb-ipython-completion-mode)))


  ;; (cond
  ;;  ((and (= emacs-major-version 24)
  ;;        (string-equal (file-name-base (symbol-file 'python-mode)) "python"))
  ;;   ;; The function `run-python' uses `python-util-clone-local-variables' to
  ;;   ;; clone all related buffer local variables so we set these variables in
  ;;   ;; a temporary buffer to get them installed in the python shell buffer.
  ;;   (let ((buffer (gpb-py:get-interpreter-buffer))
  ;;         ;; Suppress extraneous compilation-shell-minor-mode fontification.
  ;;         (compilation-mode-font-lock-keywords nil))
  ;;     (if buffer
  ;;         (pop-to-buffer buffer)
  ;;       (with-temp-buffer
  ;;         (set (make-local-variable 'python-shell-interpreter) "ipython")
  ;;         (set (make-local-variable 'python-shell-interpreter-args)
  ;;              "console --colors=LightBG")
  ;;         ;; (set (make-local-variable 'python-shell-buffer-name) "IPython")
  ;;         (set (make-local-variable 'python-shell-prompt-regexp)
  ;;              "In \\[[0-9]+\\]: ")
  ;;         (set (make-local-variable 'python-shell-prompt-output-regexp)
  ;;              "Out\\[[0-9]+\\]: ")
  ;;         (set (make-local-variable 'python-shell-completion-setup-code)
  ;;              "from IPython.core.completerlib import module_completion")
  ;;         (set (make-local-variable 'python-shell-completion-module-string-code)
  ;;              "';'.join(module_completion('''%s'''))\n")
  ;;         (set (make-local-variable 'python-shell-completion-string-code)
  ;;              "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
  ;;         (set (make-local-variable 'python-shell-process-environment)
  ;;              '("PAGER=ipython-emacs-pipe" "GPB=t"))
  ;;         (set (make-local-variable 'python-shell-setup-codes)
  ;;              (append (eval (car (get 'python-shell-setup-codes
  ;;                                      'standard-value)))
  ;;                      '(gpb-py:xmode-plain)))
  ;;         (call-interactively 'run-python)))))
  ;;  (t
  ;;   (error "Not implemented")))
)


(provide 'gpb-ipython-completion)
