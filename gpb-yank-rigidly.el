;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  This module provides a global minor mode to dedent strings when they
;;  are cut and then rigidly indent the string when it is yanked back into
;;  the buffer.  This is implemented by adding a function to
;;  `buffer-substring-filters' and using a yank-handler.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-minor-mode gpb-yank-rigidly-mode
  "Indent rigidly when yanking"
  :global t
  (if gpb-yank-rigidly-mode
      (add-hook 'buffer-substring-filters 'gpb-yank-rigidly--dedent-kill)
    (remove-hook 'buffer-substring-filters 'gpb-yank-rigidly--dedent-kill)))


(defun gpb-yank-rigidly--dedent-kill (str)
  "Dedent the STR to remove all common leading whitespace.
Ignores empty lines when computing the common indentation."
  (cond
   ;; Do nothing to single line strings
   ((not (string-match-p "\n" str))
    str)
   ;; Do nothing if the string does not start at the beginning of a
   ;; line.  See the documentation for `filter-buffer-substring' which
   ;; says that the point is set to the beginning of the current
   ;; region when this function is called.
   ((not (looking-back "^[ \t]*"))
    str)
   (t
    (let ((indent (current-column)))
      (with-temp-buffer
        (insert (make-string indent ?\ ))
        (insert str)
        (let ((inhibit-read-only t))
          (remove-text-properties (point-min) (point-max) '(read-only)))
        (goto-char (point-min))
        (skip-chars-forward " \t")
        (setq indent (current-indentation))
        (while (not (eobp))
          (forward-line)
          (unless (looking-at "[ \t]*$")
            (setq indent (min indent (current-indentation)))))
        (indent-rigidly (point-min) (point-max) (- indent))
        (propertize
         (buffer-substring-no-properties (point-min) (point-max))
         'yank-handler '(gpb-yank-rigidly--yank-and-indent)))))))


(defun gpb-yank-rigidly--yank-and-indent (str)
  "Indent string rigidly on yank/insert"
  (let ((beg (point)) (str (copy-sequence str)))
    (remove-text-properties 0 (length str) '(yank-handler) str)
    (insert-for-yank-1
     (replace-regexp-in-string
      "\n[ \t]+\n" "\n\n"
      (replace-regexp-in-string
       "\n[ \t]+\n" "\n\n"
       (replace-regexp-in-string
        "\n" (concat "\n" (make-string (current-column) ?\ )) str)))))
  (when (and (looking-back "^[ \t]*") (looking-at "[ \t]*$"))
    (delete-horizontal-space))
  (activate-mark)
  (setq deactivate-mark nil)
  (run-at-time 0.2 nil 'deactivate-mark))

(provide 'gpb-yank-rigidly)
