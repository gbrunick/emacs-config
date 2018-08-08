;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Wrapper around jedi-emacs mode.
;;
;;
;;  Function Name              Count       Elapsed Time  Average Time
;;  gpb-jedi:get-completions   400         78.810684279  0.1970267106
;;  gpb-jedi:get-completions2  400         18.060251280  0.0451506282
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'jedi)

(defvar gpb-jedi:debug-buffer-sync t)

(unless (require 'gpb-logging nil t)
  (defun (gob-log-forms &rest args) nil))

(defcustom gpb-jedi:python-executable "python"
  "The python executable used to to run the completion server.
You can provide a full path if you would like.")

(defvar gpb-jedi:mode-map (make-sparse-keymap))
(defvar gpb-jedi:server-source-filename "python_completion_server.py")
(defvar gpb-jedi:get-server-url-regex "\\(http://[-a-zA-Z0-9.:/]+\\)$")
(defvar gpb-jedi:server-buffer-name "*python completions*")
(defvar gpb-jedi:server-process nil)
(defvar gpb-jedi:server-url nil)

(defvar gpb-jedi:async-rpc-buffer-list nil)
(defvar gpb-jedi:async-call-count 0)

(defvar gpb-jedi:eldoc-function:timer nil)

(defvar gpb-jedi:registered-buffer-names nil)
(defvar gpb-jedi:registered-name nil)
(make-variable-buffer-local 'gpb-jedi:registered-name)
(defvar gpb-jedi:buffer-snapshot nil)
(make-variable-buffer-local 'gpb-jedi:buffer-snapshot)

(defvar gpb-jedi:buffer-change-count 0)
(defvar gpb-jedi:all-buffer-changes nil)
(defvar gpb-jedi:sync-buffers-timer nil)
(defvar gpb-jedi:source-buffers nil)
(defvar gpb-jedi:sync-buffer-count 0)

;; (define-key gpb-jedi:mode-map "\t" 'gpb-jedi:tab-command)
(define-key gpb-jedi:mode-map "\M-gd" 'jedi:goto-definition)
(define-key gpb-jedi:mode-map [(control c)(f1)] 'jedi:show-doc)
(define-key gpb-jedi:mode-map [(control c)(h)] 'jedi:show-doc)
;; Conficts with my rectangle command
;; (define-key gpb-jedi:mode-map "\C-cr" 'anything-jedi-related-names)


(define-minor-mode gpb-jedi-mode ()
  :keymap gpb-jedi:mode-map
  ;; :lighter " Jedi"
  (cond
   ;; Setup minor mode
   (gpb-jedi-mode
    ;; (eldoc-mode 1)
    (company-mode 1)
    (add-hook 'after-change-functions 'gpb-jedi:after-change-function nil t)
    (set (make-local-variable 'eldoc-documentation-function)
         'gpb-jedi:eldoc-function)
    (set (make-local-variable 'company-backends) '(gpb-jedi:company-python))
    (unless (gpb-jedi:server-ready-p) (gpb-jedi:start-server)))
   ;; Remove minor mode
   (t
    ;; (eldoc-mode -1)
    (company-mode -1)
    (kill-local-variable 'eldoc-documentation-function)
    (kill-local-variable 'company-backends)
    (remove-hook 'after-change-functions 'gpb-jedi:after-change-function t))))

;; (advice-remove 'rename-buffer #'gpb-jedi:rename-buffer-advice)
;; (advice-add 'rename-buffer :around #'gpb-jedi:rename-buffer-advice)
;; (defun gpb-jedi:rename-buffer-advice (next-func &rest args)
;;   "Advise `rename-buffer' to inform python server of changes."
;;   (interactive)
;;   (let ((old-buffer-name (buffer-name)))
;;     (apply next-func args)
;;     (when gpb-jedi-mode
;;       (gpb-jedi:invoke-server-method `(rename-buffer ,old-buffer-name
;;                                                      ,(buffer-name))))))


(defun gpb-jedi:after-change-function (beg end len)
  (let ((initial-end (+ beg len))
        (replacement (buffer-substring-no-properties beg end)))
    (push `(,(incf gpb-jedi:buffer-change-count) ,(buffer-name) ,(1- beg)
            ,(1- initial-end) ,replacement) gpb-jedi:all-buffer-changes)
    (when gpb-jedi:sync-buffers-timer (cancel-timer gpb-jedi:sync-buffers-timer))
    (setq gpb-jedi:sync-buffers-timer
          (run-at-time 1 nil 'gpb-jedi:sync-all-buffers))))


;; (defun gpb-jedi:assert-buffer-state-consistent (&optional buf)
;;   (let* ((buf (or (and buf (get-buffer buf)) (current-buffer)))
;;          (buf-string (with-current-buffer buf
;;                        (save-restriction
;;                          (widen)
;;                          (buffer-substring-no-properties (point-min)
;;                                                          (point-max)))))
;;          (buf-mirror (gpb-jedi:invoke-server-method
;;                       `(get-buffer-contents ,(buffer-name buf)))))
;;     (unless (string-equal buf-string buf-mirror)
;;       ;; (message "buf-mirror:\n%s" buf-mirror)
;;       (let ((buf1 (generate-new-buffer (concat (buffer-name) " (in emacs)")))
;;             (buf2 (generate-new-buffer (concat (buffer-name) " (in server)"))))
;;         (with-current-buffer buf1 (insert buf-string))
;;         (with-current-buffer buf2 (insert buf-mirror)))
;;       ;; Make all buffers as not registered and remove all pending changes
;;       (setq gpb-jedi:registered-buffer-names nil
;;             gpb-jedi:all-buffer-changes nil)
;;       (error "Buffer out of sync: %S" (buffer-name)))))


(defun gpb-jedi:company-python (command &optional arg)
  "Interface to company mode."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'gpb-jedi:company-python))
    (sorted t)
    (prefix (buffer-substring-no-properties
             (save-excursion (skip-chars-backward "a-zA-Z0-9_") (point))
             (point)))
    (candidates (gpb-jedi:get-completions))
    (post-completion (gpb-jedi:eldoc-function))))


(defun gpb-jedi:eldoc-function ()
  (let ((delay 0.25))
    (cond
     (gpb-jedi:eldoc-function:timer
      (cancel-timer gpb-jedi:eldoc-function:timer)
      (setq gpb-jedi:eldoc-function:timer
            (run-at-time delay nil 'gpb-jedi:eldoc-function-1)))
     (gpb-jedi:async-rpc-buffer-list
      (setq gpb-jedi:eldoc-function:timer
            (run-at-time delay nil 'gpb-jedi:eldoc-function-1)))
     (t
      (gpb-jedi:eldoc-function-1)))
    eldoc-last-message))


(defun gpb-jedi:eldoc-function-1 ()
  ;; (message "gpb-jedi:eldoc-function-1")
  (when gpb-jedi:eldoc-function:timer
    (cancel-timer gpb-jedi:eldoc-function:timer))
  (setq gpb-jedi:eldoc-function:timer nil)
  (let ((source (buffer-substring-no-properties (point-min) (point-max)))
        (line (line-number-at-pos))
        (column (current-column))
        (source-path (ignore-errors (expand-file-name (buffer-file-name)))))
    ;; (when gpb-jedi:debug-buffer-sync (gpb-jedi:sync-all-buffers))
    (gpb-jedi:invoke-server-method
     `(get_eldoc_message ,(buffer-name) ,line ,column ,source-path nil)
     'gpb-jedi:show-eldoc-message :with-update)
    (setq gpb-jedi:all-buffer-changes nil)))


(defun gpb-jedi:get-completions ()
  (interactive)
  (when gpb-jedi:debug-buffer-sync (gpb-jedi:sync-all-buffers))
  (let ((source (buffer-substring-no-properties (point-min) (point-max)))
        (line (line-number-at-pos))
        (column (current-column))
        (source-path (ignore-errors (expand-file-name (buffer-file-name)))))
    (prog1
        (gpb-jedi:invoke-server-method
         `(complete ,(buffer-name) ,line ,column ,source-path nil)
         nil :with-update)
      (setq gpb-jedi:all-buffer-changes nil))))


(defun gpb-jedi:get-server-url-filter (string)
  "Scan the output from the completion server process for url information.\n
This filter should be added to `comint-output-filter-functions'."
  (when (string-match gpb-jedi:get-server-url-regex string)
    (setq gpb-jedi:server-url (match-string 0 string))))


(defun gpb-jedi:invoke-server-method (form &optional callback with-update)
  (interactive)
  (assert (gpb-jedi:server-ready-p))
  (let ((save-url-show-status url-show-status))
    (setq url-show-status nil)
    (unwind-protect
        (progn
          ;; If the name of a registered buffer changes, then all bets are
          ;; off.  A name change is relatively rare and seems to be rather
          ;; tricky to get correct, so we don't expend any effort trying to
          ;; track these efficiently.
          (when (and gpb-jedi:registered-name
                     (not (eq (buffer-name) gpb-jedi:registered-name)))
            ;; (message "Reseting all buffers...")
            (setq gpb-jedi:registered-buffer-names nil
                  gpb-jedi:all-buffer-changes nil))

          ;; Ensure the the buffer is registered with the completion server
          (unless (member (buffer-name) gpb-jedi:registered-buffer-names)
            ;; Remove any pending buffer changes for the current buffer.
            (setq gpb-jedi:all-buffer-changes
                  (delete-if (lambda (x) (string-equal (second x) (buffer-name)))
                             gpb-jedi:all-buffer-changes))
            (let ((initial-contents (save-restriction
                                     (widen)
                                     (buffer-substring-no-properties
                                      (point-min) (point-max)))))
              ;; (message "Registering %s..." (buffer-name))
              (xml-rpc-method-call gpb-jedi:server-url 'register-buffer
                                   (buffer-name) initial-contents)
              (when gpb-jedi:debug-buffer-sync
                (setq gpb-jedi:buffer-snapshot initial-contents)))
            (add-to-list 'gpb-jedi:registered-buffer-names (buffer-name))
            (setq gpb-jedi:registered-name (buffer-name)))

          ;; sync all buffers
          (gpb-jedi:sync-all-buffers)

          ;; Now make the actual xml-rpc call
          (if callback
              (apply 'gpb-jedi:xml-rpc-method-call-async
                     callback gpb-jedi:server-url form)
            (apply 'xml-rpc-method-call gpb-jedi:server-url form)))
      ;; restore initial status of url-show-status
      (setq url-show-status save-url-show-status))))


;; (defun gpb-jedi:register-all-buffers ()
;;   "Register all python buffers with the current server"
;;   (dolist (buf (buffer-list))
;;     (with-current-buffer buf
;;       (when (derived-mode-p 'python-mode)
;;         (gpb-jedi:register-buffer))))
;;   (setq gpb-jedi:all-buffer-changes nil))


;; (defun gpb-jedi:register-buffer ()
;;   "Register a buffer with the python completion server."
;;   ;; Remove any pending buffer changes for the current buffer.
;;   (setq gpb-jedi:all-buffer-changes
;;         (delete-if (lambda (x) (string-equal (second x) (buffer-name)))
;;                    gpb-jedi:all-buffer-changes))
;;   (let ((inital-context (save-restriction
;;                           (widen)
;;                           (buffer-substring-no-properties (point-min)
;;                                                           (point-max)))))
;;     (gpb-jedi:invoke-server-method `(register-buffer ,(buffer-name)
;;                                                      ,inital-context))))


(defun gpb-jedi:show-eldoc-message (args)
  ;; (message "gpb-jedi:show-eldoc-message %S" args)
  (if (null args)
      (eldoc-message nil)
    (let* ((func-name (first args))
           (index (second args))
           (parameters (cddr args))
           (hl-face 'eldoc-highlight-function-argument)
           (format-parameter (lambda (p)
                               (prog1
                                   (concat (if (> i 0) ", " "")
                                           (if (= i index)
                                               (propertize p 'face hl-face)
                                             p))
                                 (incf i))))
           (i 0)
           (msg (concat
                 (propertize func-name 'face 'font-lock-function-name-face)
                 "(" (apply 'concat (mapcar format-parameter parameters)) ")")))
      (eldoc-message msg))))


(defun gpb-jedi:server-ready-p ()
  (and gpb-jedi:server-process
       (process-live-p gpb-jedi:server-process)
       gpb-jedi:server-url))


(defun gpb-jedi:start-server ()
  "Start python completion server.\n
If the python server is currently running, this process is killed
and a new process is started.."
  (interactive)
  ;; Kill any current server process and buffer
  (when (and gpb-jedi:server-process (process-live-p gpb-jedi:server-process))
    (kill-process gpb-jedi:server-process))
  (let ((old-buf (get-buffer "*python completions*")))
    (when old-buf (kill-buffer old-buf)))
  (setq gpb-jedi:server-url nil)

  ;; Create a server process and buffer
  (let ((buf (make-comint "python completions"
                          gpb-jedi:python-executable nil
                          "-u" (locate-library gpb-jedi:server-source-filename))))
    (setq gpb-jedi:server-process (get-buffer-process buf))
    (set-process-query-on-exit-flag gpb-jedi:server-process nil)
    (with-current-buffer buf
      (compilation-shell-minor-mode 1)
      (ansi-color-for-comint-mode-on)
      (add-hook 'comint-output-filter-functions
                'gpb-jedi:get-server-url-filter)
      (set (make-local-variable 'comint-buffer-maximum-size) 1000)
      (add-hook 'comint-output-filter-functions 'comint-truncate-buffer t t)))

  ;; Wait for the server process to start up.
  ;; FIXME: I don't understand why the following line is necessary.
  (accept-process-output gpb-jedi:server-process 1 nil t)
  (unless (catch 'server-started
            (dotimes (i 4)
              ;; (message "i = %s" i)
              (when (gpb-jedi:server-ready-p)
                (throw 'server-started t))
              (unless (accept-process-output gpb-jedi:server-process 1 nil t)
                (throw 'server-started nil))))
            ;;   (message "Buf: %S" (with-current-buffer "*python completions*"
            ;;                        (buffer-string))))
            ;; (throw 'server-started nil))
    (error "Python completion server did not start properly"))

  ;; Make all buffers as not registered and remove all pending changes
  (setq gpb-jedi:registered-buffer-names nil
        gpb-jedi:all-buffer-changes nil)

  (when gpb-jedi:debug-buffer-sync
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (derived-mode-p 'python-mode)
          (setq gpb-jedi:buffer-snapshot (buffer-substring-no-properties
                                          (point-min) (point-max)))))))
  ;; (gpb-jedi:register-all-buffers)
  )


(defun gpb-jedi:sync-all-buffers ()
  ;; (message "gpb-jedi:all-buffer-changes: %S" gpb-jedi:all-buffer-changes)
  (when gpb-jedi:sync-buffers-timer
    (cancel-timer gpb-jedi:sync-buffers-timer)
    (setq gpb-jedi:sync-buffers-timer nil))
  (incf gpb-jedi:sync-buffer-count)
  (when gpb-jedi:all-buffer-changes
    ;; (message "gpb-jedi:all-buffer-changes: %S" gpb-jedi:all-buffer-changes)
    (let ((modified-buffers (remove-duplicates
                              (mapcar 'second gpb-jedi:all-buffer-changes)
                              :test 'string-equal)))
      (gpb-jedi:invoke-server-method
       `(update-buffers ,gpb-jedi:all-buffer-changes))

      (when gpb-jedi:debug-buffer-sync
        (dolist (buf-name modified-buffers)
          (let* ((buf-local (with-current-buffer buf-name
                               (save-restriction
                                 (widen)
                                 (buffer-substring-no-properties (point-min)
                                                                 (point-max)))))
                 (buf-remote (gpb-jedi:invoke-server-method
                              `(get-buffer-contents ,buf-name))))
            (unless (string-equal buf-local buf-remote)
              ;; (message "buf-mirror:\n%s" buf-mirror)
              (let ((buf1 (get-buffer-create
                           (format "%s in emacs before sync %i"
                                   (buffer-name) gpb-jedi:sync-buffer-count)))
                    (buf2 (get-buffer-create
                           (format "%s in emacs after sync %i"
                                   (buffer-name) gpb-jedi:sync-buffer-count)))
                    ;; (buf3 (get-buffer-create
                    ;;        (format "%s in server before sync %i"
                    ;;                (buffer-name) gpb-jedi:sync-buffer-count)))
                    (buf4 (get-buffer-create
                           (format "%s in server after sync %i"
                                   (buffer-name) gpb-jedi:sync-buffer-count)))
                    (buf5 (get-buffer-create
                           (format "%s changes in sync %i"
                                   (buffer-name) gpb-jedi:sync-buffer-count))))
                (with-current-buffer buf1 (insert (or gpb-jedi:buffer-snapshot
                                                      "no information")))
                (with-current-buffer buf2 (insert buf-local))
                ;; (with-current-buffer buf3 (insert (aget initial-server-state
                ;;                                         buf-name)))
                (with-current-buffer buf4 (insert buf-remote))
                (with-current-buffer buf5
                  (erase-buffer)
                  (insert (format "%S" gpb-jedi:all-buffer-changes)))
              ;; Make all buffers as not registered and remove all pending
              ;; changes
              (setq gpb-jedi:registered-buffer-names nil
                    gpb-jedi:all-buffer-changes nil)
              (error "Buffer out of sync: %S" buf-name)))
            (with-current-buffer buf-name
              (setq gpb-jedi:buffer-snapshot buf-local)))))

      (setq gpb-jedi:all-buffer-changes nil))))


(defun gpb-jedi:xml-rpc-method-call-async (callback url method &rest params)
  "Call an XML-RPC method asynchronously\n
The function `xml-rpc-method-call-async' seems to be broken. This is
function is a work around."
  (let* ((callback-wrapper (copy-list (lambda (arg) nil)))
         (buf (apply 'xml-rpc-method-call-async callback-wrapper
                     url method params))
         (proc (get-buffer-process buf)))
    (setcdr callback-wrapper
            `((arg) (funcall 'gpb-jedi:xml-rpc-method-call-async:handle-callback
                             ',callback ,buf ,proc)))
    buf))


(defun gpb-jedi:xml-rpc-method-call-async:handle-callback (callback buf proc)
  (assert (and (buffer-live-p buf) (eq (process-status proc) 'closed)))
  (setq gpb-jedi:async-rpc-buffer-list (delq buf gpb-jedi:async-rpc-buffer-list))
  (funcall callback (xml-rpc-xml-to-response
                     (xml-rpc-request-process-buffer buf))))


(provide 'gpb-python-jedi)
