(add-to-list 'file-name-handler-alist '("\\`/test:.*" . test-file-handler))

(defun test-file-handler (operation &rest args)
  (let (result)
    (message "test-file-handler: %S %S" operation args)
    (setq result
          (case operation
            ;; (get-file-buffer (with-current-buffer (get-buffer-create "*test*")
            ;;                    (setq buffer-file-name (car args))))
            (substitute-in-file-name (car args))
            (file-exists-p t)
            (file-remote-p t)
            (file-directory-p nil)
            (file-truename (car args)) ;;(second (split-string (car args) ":")))
            (file-name-directory "/test:")
            (file-name-nondirectory (second (split-string (car args) ":")))
            (directory-file-name (car args))
            (file-writable-p nil)
            (make-auto-save-file-name nil)
            (file-modes (tramp-mode-string-to-int "-rw-------"))
            (insert-file-contents
             (multiple-value-bind (filename visit beg end replace) args
               (assert (and visit (null beg) (null end) (null replace)))
               (let ((text "this is a test..."))
                 (insert text)
                 (when visit
                   (setq buffer-file-name filename)
                   ;; (clear-visited-file-modtime)
                   )
                 (list (car args) (length text)))))
            (file-name-completion
             (multiple-value-bind (file directory predicate) args
               (try-completion file '("this one" "this two") predicate)))
            (file-name-all-completions
             (multiple-value-bind (file directory) args
               (all-completions file '("this one" "this two"))))
            (file-name-sans-versions (car args))
            (unhandled-file-name-directory nil)
            (file-attributes '(nil 1 1 1 (current-time) (current-time)
                                   (current-time) 100 "-rw-------" nil 1 1))
            (expand-file-name (car args))
            (otherwise
             (if (member operation '(get-file-buffer))
                 (let ((inhibit-file-name-handlers
                        (cons 'test-file-handler
                              (and (eq inhibit-file-name-operation operation)
                                   inhibit-file-name-handlers)))
                       (inhibit-file-name-operation operation))
                   (apply operation args))
               (error "Not implemented")))))
    (message "result: %S" result)
    (message nil)
    result))
