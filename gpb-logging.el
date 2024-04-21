;; Simple logging functionality.
;;
;; Use `gpb-log-forms' in a function; set `gpb-log--logging-enabled' to t;
;; and then call `gpb-log--enable-logging' on the functions for which you
;; would like to see logging output.
;;
;;
;; You need to remember to let bind deactivate-mark inside logging
;; functions so that the mark is not deactivated by the insert
;; commands.  If your don't do this then the logging functions affect
;; the state of transient mark mode which causes new problems when you
;; are trying to log output.

(require 'trace)
(require 'thingatpt)

(defcustom gpb-log--logging-enabled nil
  "All logging is disable when this value is not t"
  :type 'boolean
  :group 'gpb-log)


(defvar gpb-log--indent-level 0)
(defvar gpb-log--indent-size 4)

(defun gpb-log--function-is-begin-logged-p (function)
  (ad-find-advice function 'around 'gpb-log-function-advice))


(defun gpb-log-forms (func &rest forms)
  "Evaluate forms in list FORMS and write value to \"*Debug Log*\" buffer.

The forms are only logged if logging has been enabled for the
function using `gpb-log--enable-logging'.  This is not a macro,
so forms must be quoted to prevent premature evaluation."
  ;; (let ((caller (cadr (backtrace-frame 4))))
  ;;   (message "gpb-log-forms caller: %S" caller))
  (when (gpb-log--function-is-begin-logged-p func)
    (dolist (form forms)
      (let* ((value (condition-case exc (eval form)
                      ('error (format "Error: %s" exc))))
             (win (get-buffer-window "*Debug Log*"))
             (at-point-max (when win
                             (with-selected-window win
                               (equal (point) (point-max)))))
             deactivate-mark)
        (when (stringp value)
          (setq value (gpb-util-truncate-string value 'center 250)))
        (with-current-buffer (get-buffer-create "*Debug Log*")
          (save-excursion
            (goto-char (point-max))
            (insert (if (and (boundp 'gpb-log-label) gpb-log-label)
                        (format "%s: %S = %S\n" gpb-log-label form value)
                      (format "%s%S = %S\n" (make-string
                                             (*  gpb-log--indent-level
                                                gpb-log--indent-size)
                                             ?\ )
                              form value)))))))))

(defun gpb-log-form (func form)
  (gpb-log-forms func form))


(defun gpb-log-message (func &rest args)
  "Write message to \"*Debug Log*\" buffer.

Behaves in essentially the same way as `gpb-log-forms'"
  (when (gpb-log--function-is-begin-logged-p func)
    (with-current-buffer (get-buffer-create "*Debug Log*")
      (let (deactivate-mark)
        (save-excursion
          (goto-char (point-max))
          (insert (format "%s%s\n" (make-string (*  gpb-log--indent-level
                                                    gpb-log--indent-size) ?\ )
                          (apply 'format args))))))))


(defun gpb-log--enable-logging (function)
  "Enable logging output for FUNCTION.

When used interactively, user is prompted for the function and
defualts to the symbol at the current point."
  (interactive
   (let ((fn (symbol-at-point))
         (enable-recursive-minibuffers t)
         val)
     (setq val (completing-read (if (when (fboundp fn) fn)
                                    (format "Log function (default %s): " fn)
                                  "Log function: ")
                                obarray 'fboundp t nil nil
                                (and fn (symbol-name fn))))
     (list (if (equal val "") fn (intern val)))))

  (setq gpb-log--logging-enabled t)

  (ad-add-advice
   function
   (ad-make-advice
    'gpb-log-function-advice nil t
    `(advice lambda ()
             "Write logging messages to buffer *Debug Log*"
             (let ((trace-buffer (get-buffer-create "*Debug Log*"))
                   deactivate-mark)
               (with-current-buffer trace-buffer
                 (save-excursion
                   (goto-char (point-max))
                   (insert
                    (gpb-log--entry/exit-message
                     ',function gpb-log--indent-level ad-arg-bindings))))
               (lexical-let ((gpb-log--logging-enabled t))
                 (unwind-protect
                     (progn
                       (cl-incf gpb-log--indent-level)
                       ad-do-it)
                   (cl-decf gpb-log--indent-level)))
               (with-current-buffer trace-buffer
                 (save-excursion
                   (goto-char (point-max))
                   (insert
                    (gpb-log--entry/exit-message
                     ',function gpb-log--indent-level
                     ad-arg-bindings (list ad-return-value))))))))
   'around 'last)
  (ad-activate function nil))


(defun gpb-log--disable-logging (function)
  (interactive
   (list (ad-read-advised-function "Removing logging advice from function: "
                                   'gpb-log--function-is-begin-logged-p)))
         (ad-remove-advice function 'around 'gpb-log-function-advice)
         (ad-update function))


(defun gpb-log--entry/exit-message (function level argument-bindings
                                             &optional return-value)
  ;; Generates a string that describes that FUNCTION has been entered at
  ;; trace LEVEL with ARGUMENT-BINDINGS.
  (let ((return-str (if return-value (format "%S <- " (car return-value))
                      "-> ")))
    (format "%s%s(%s%s)\n%s"
            (make-string (* gpb-log--indent-size level) ?\ )
            return-str
            function
            (cond
             ((and argument-bindings return-value) " ...")
             (argument-bindings
              (concat " "
                      (mapconcat (function
                                  (lambda (binding)
                                    (concat
                                     (symbol-name (ad-arg-binding-field
                                                   binding 'name))
                                     "="
                                     ;; do this so we'll see strings:
                                     (prin1-to-string
                                      (ad-arg-binding-field binding 'value)))))
                                 argument-bindings
                                 " ")))
             (t ""))
            (if (and (= level 0) return-value) "\n" ""))))


(provide 'gpb-logging)
