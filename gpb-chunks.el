;; Code for setting chunks of a buffer to use another major mode.

(require 'gpb-util)
(require 'gpb-logging)

(defface gpb-chunk-bad-chunk-face
  '((t (:background "salmon3")))
  "Face for text in bad chunks."
  :group 'gpb-chunk)

(defvar gpb-chunks--debug nil "A debugging flag.")

;; To enable debugging: (gpb-chunks-enable-debugging)
(defun gpb-chunks-enable-debugging ()
  (interactive)
  (setq gpb-chunks--debug t
        font-lock-verbose t))

;; To disable debugging: (gpb-chunks-disable-debugging)")
(defun gpb-chunks-disable-debugging ()
  (interactive)
  (setq gpb-chunks--debug nil
        font-lock-verbose nil))

(defun gpb-chunks-log-region (beg end)
  (let ((s (concat "***\n" (buffer-substring-no-properties beg end)
                   "\n***")))
  (gpb-log-message 'gpb-chunks-log-region "%s\n" (gpb-util-indent-string s))))

;; (defun gpb-chunks-force-redisplay ()
;;   (interactive)
;;   (jit-lock-force-redisplay (current-buffer) (point-min) (point-max)))

(defvar gpb-chunks-buffer-chunks nil
  "An alist of the form ((symbol beg-regex end-regex) ...)")
(make-variable-buffer-local 'gpb-chunks-buffer-chunks)

(defvar gpb-chunks-font-lock-defaults-alist nil
  "This alist maps chunk symbols to font-lock-defaults variables.")
(make-variable-buffer-local 'gpb-chunks-font-lock-defaults-alist)

(defvar gpb-chunks-font-lock-variables nil
  "This alist maps chunk symbols to an alist of font-lock related
  variables values.  In particular, this is an alist of alists.")
(make-variable-buffer-local 'gpb-chunks-font-lock-variables)

;; (defvar gpb-chunks-chunks-have-whole-lines t)
;; (make-variable-buffer-local 'gpb-chunks-chunks-have-whole-lines)

(defvar gpb-chunks-syntax-table-alist nil)
(make-variable-buffer-local 'gpb-chunks-syntax-table-alist)

(define-minor-mode gpb-chunks-minor-mode
  "A minor mode for dealing with buffer chunks."
  nil " Ch" nil
  (if gpb-chunks-minor-mode
      (progn
        (message "Enabling chunks in %s..." (current-buffer))
        ;; (add-hook 'jit-lock-after-change-extend-region-functions
        ;;           'gpb-chunks-extend-jit-region t t)
        ;; Buffer local variables:
        ;;   gpb-chunks-buffer-chunks
        ;;   gpb-chunks-last-chunk
        ;;   gpb-chunks-font-lock-defaults-alist
        ;;   gpb-chunks-syntax-table-alist
        ;;   gpb-chunks-minor-mode-alist
        (set (make-local-variable 'gpb-chunks-buffer-chunks) nil)
        (set (make-local-variable 'gpb-chunks-last-chunk)
             'outside-all-chunks)
        (set (make-local-variable 'gpb-chunks-syntax-table-alist) nil)
        (set (make-local-variable 'gpb-chunks-minor-mode-alist) nil)
        ;; We also need to save the base font-lock-defaults and variables
        (set (make-local-variable 'gpb-chunks-font-lock-defaults-alist)
             (list (cons 'outside-all-chunks font-lock-defaults)))
        (set (make-local-variable 'gpb-chunks-font-lock-variables)
             (list (cons 'outside-all-chunks (gpb-chunks--get-fl-variables))))
        ;; We throw some syntax tables into text properties on chunks.
        ;; The next variable must be set if we would like emacs to
        ;; look at these syntax-tables.
        (set (make-local-variable 'parse-sexp-lookup-properties) t)
        ;; Finally, we setup our custom font locking function.
        (set (make-local-variable 'font-lock-fontify-region-function)
             'gpb-chunks-fontify-region-safe)
        ;; *** Removing the following line seems to freeze emacs. ***
        ;;(font-lock-unfontify-buffer)
        ;;(font-lock-fontify-buffer)
        )
    (message "Disabling chunk mode...")
    (let ((key-value (assoc 'outside-all-chunks
                            gpb-chunks-font-lock-defaults-alist)))
      (when key-value
        (setq font-lock-defaults (cdr key-value)
              font-lock-set-defaults nil)
        (font-lock-set-defaults)))
    (kill-local-variable 'gpb-chunks-buffer-chunks)
    (kill-local-variable 'gpb-chunks-last-chunk)
    (kill-local-variable 'gpb-chunks-syntax-table-alist)
    (kill-local-variable 'gpb-chunks-minor-mode-alist)
    (remove-hook 'post-command-hook 'gpb-chunks-post-command-hook t)))

(defun gpb-chunks-extend-jit-region (start end old-len)
  (let* ((prev-data (gpb-chunks-get-current-chunk jit-lock-beg))
         (next-data (gpb-chunks-get-current-chunk jit-lock-end))
         (update (not (and (eq (nth 1 prev-data) jit-lock-beg)
                           (eq (nth 2 next-data) jit-lock-end))))
         (gpb-log-enabled gpb-chunks--debug)
         (gpb-log-label 'gpb-chunks-extend-jit-region))
    (gpb-log-forms 'gpb-chunks-extend-jit-region 'jit-lock-beg 'jit-lock-end
                   '(nth 1 prev-data) '(nth 2 next-data)
                   'update)
    (setq jit-lock-beg (nth 1 prev-data)
          jit-lock-end (nth 2 next-data))))

;; (defun gpb-chunks--font-lock-extend-region ()
;;   (let ((gpb-log-enabled gpb-chunks--debug)
;;         (gpb-log-label 'gpb-chunks--font-lock-extend-region))
;;     (gpb-log-forms 'gpb-chunks--font-lock-extend-region '(gpb-chunks-get-current-chunk font-lock-beg))
;;    nil))


(defun gpb-chunks--get-fl-variables ()
  "Create an alist of all font-lock related variables."
  (mapcar (lambda (var-name) (cons var-name (symbol-value var-name)))
          '(font-lock-mark-block-function
            font-lock-syntactic-keywords
            font-lock-fontify-region-function
            font-lock-unfontify-region-function
            font-lock-fontify-buffer-function
            font-lock-unfontify-buffer-function
            font-lock-inhibit-thing-lock)))

;; (defun gpb-chunks--get-fl-variables ()
;;   "Ensure that all font locking local variables are stored in
;;   font-lock-defaults.

;;   We change the font locking by altering the value of
;;   font-lock-defaults and then calling
;;   (font-lock-set-defaults).  If this is to work, we need to make
;;   sure that all the information that we will need for font
;;   locking is stored in font-lock-defaults.  Some modes may set
;;   font related buffer local variables directly, rather than
;;   storing the values in font-lock-defaults.  We do our best to
;;   detect and correct this.
;;   "
;;   ;; The following is probably a bad idea.  If a mode sets up font
;;   ;; lock by hand, the following call would override what it defined?
;;   ;; (font-lock-set-defaults)
;;   ;; We will modify the list, so we should copy it to prevent side
;;   ;; effects in non-chunk buffers with the major mode.
;;   (setq font-lock-defaults (copy-list font-lock-defaults))
;;   (let ((fl-vars (nthcdr 5 defaults)) key-value)
;;     (dolist (var-name '(font-lock-mark-block-function
;;                         font-lock-syntactic-keywords
;;                         font-lock-fontify-region-function
;;                         font-lock-unfontify-region-function
;;                         font-lock-fontify-buffer-function
;;                         font-lock-unfontify-buffer-function
;;                         font-lock-inhibit-thing-lock))
;;       ;; If the key is set in font lock defaults it should match
;;       ;; the buffer local variable's value.  If the key is not
;;       ;; set, we set it.
;;       (let ((key-value (assoc var-name font-lock-defaults)))
;;         (when (and key-value
;;                    (not (eq (symbol-value var-name) (cdr key-value))))
;;           (message (concat "Warning: When setting up chunks, I "
;;                            "see that %s=%s but font-lock-defaults "
;;                            "contains the value %s")
;;                    var-name (symbol-value var-name) key-value)))
;;       ;; Now stash the current value in font-lock-defaults
;;       (setq font-lock-defaults (nconc font-lock-defaults
;;                                       (list (cons var-name
;;                                                   (symbol-value var-name))))))))

(defun gpb-chunks-add-chunk (symbol beg-regex end-regex &optional
                             fontification-mode minor-mode)
  "Define a new kind of buffer sub-chunk.

  If FONTIFICATION-MODE is set, it should be a major mode, and
  gpb-chunks does its best to font-lock the code in the chunk as
  if it were in a buffer with that major mode.

  If MINOR-MODE is set, it should be a minor mode.  gpb-chunks
  activates this minor mode whenever the cursor is in the chunk."
  (assert gpb-chunks-minor-mode)
  (push (list symbol beg-regex end-regex) gpb-chunks-buffer-chunks)
  (when fontification-mode
    (let (fl-defaults syntax-table fl-variables)
      ;; Switch to major mode long enough to copy the syntax-table
      ;; and font-lock-defaults
      (with-current-buffer (generate-new-buffer "gpb-temp")
        (funcall fontification-mode)
        (setq fl-defaults font-lock-defaults
              syntax-table (copy-syntax-table (syntax-table))
              fl-variables (gpb-chunks--get-fl-variables))
        (kill-buffer))
      (push (cons symbol fl-defaults) gpb-chunks-font-lock-defaults-alist)
      (push (cons symbol syntax-table) gpb-chunks-syntax-table-alist)
      (push (cons symbol fl-variables) gpb-chunks-font-lock-variables)))
  (when minor-mode
    (push `(,symbol . ,minor-mode) gpb-chunks-minor-mode-alist)
    (add-hook 'post-command-hook 'gpb-chunks-post-command-hook nil t))
  ;; *** Removing the following line seems to freeze emacs. ***
  (font-lock-unfontify-buffer)
  (font-lock-fontify-buffer)
  )

(defun gpb-chunks-post-command-hook ()
  "This hook is responsible for enabling/disabling minor modes.
  Uses gpb-chunks-last-chunk to remember the last chunk."
  ;; (message "deact %s" deactivate-mark)
  ;; (with-current-buffer "*Debug Log*"
  ;;   (insert-before-markers "blah\n"))
  ;; (message "deact %s" deactivate-mark)
  (let ((symbol-beg-end (gpb-chunks-get-current-chunk))
        symbol-minormode
        ;; Logging setup
        (gpb-log-enabled gpb-chunks--debug)
        (gpb-log-label   'gpb-chunks-post-command-hook)
        )
    (unless (eq gpb-chunks-last-chunk (car symbol-beg-end))
      (gpb-log-message 'gpb-chunks-post-command-hook "Chunk changed from %s to %s."
                       gpb-chunks-last-chunk
                       (car symbol-beg-end))
      (setq gpb-chunks-last-chunk (car symbol-beg-end))
      (dolist (symbol-minormode gpb-chunks-minor-mode-alist)
        (funcall (cdr symbol-minormode) -1))
      (setq symbol-minormode (assoc (car symbol-beg-end)
                                    gpb-chunks-minor-mode-alist))
      (when symbol-minormode
        (gpb-log-message 'gpb-chunks-post-command-hook "Enable minor mode: %s"
                         (cdr symbol-minormode))
        (funcall (cdr symbol-minormode) 1)))))

;; (defun gpb-chunks-find-chunk-boundary (pos forward/backward beg/end)
;;   "Search forward or backward for the next start or end of a chunk.

;; Returns the symbol associated with the chunk if a boundary is
;; found.  Leaves the point before the boundary when seraching
;; forward and after the boundary when search backwards.  In
;; particular, the operation is idempotent.

;; If no boundary if found returns nil and moves point to the
;; beginning or end of the buffer.

;; FORWARD/BACKWARD should be 'forward or 'backward
;; BEG/END should be 'beg or 'end."
;;   (let* ((chunk-list gpb-chunks-buffer-chunks)
;;          (whole-lines gpb-chunks-chunks-have-whole-lines)
;;          (any-begin (concat "\\(?:"
;;                             (gpb-util-join-strings
;;                              (mapcar 'cadr chunk-list)
;;                              "\\)\\|\\(?:")
;;                             "\\)"))
;;          (any-end (concat "\\(?:"
;;                           (gpb-util-join-strings
;;                            (mapcar 'caddr chunk-list)
;;                            "\\)\\|\\(?:")
;;                           "\\)"))
;;          result-pos)
;;     (let ((result
;;            (catch 'return-chunk-type
;;              (cond
;;               ((and (eq forward/backward 'forward)
;;                     (eq beg/end 'beg))
;;                (condition-case exception
;;                    (progn
;;                      (re-search-forward any-begin)
;;                      (goto-char (match-beginning 0)))
;;                  ('error
;;                   (goto-char (point-max))
;;                   (throw 'return-chunk-type nil)))
;;                (dolist (chunk-data chunk-list)
;;                  (when (looking-at (cadr chunk-data))
;;                    (throw 'return-chunk-type (car chunk-data))))
;;                (error "This shouldn't happen."))

;;               ((and (eq forward/backward 'forward)
;;                     (eq beg/end 'end))
;;                (condition-case exception
;;                    (progn
;;                      (re-search-forward any-end)
;;                      (goto-char (match-beginning 0)))
;;                  ('error
;;                   (goto-char (point-max))
;;                   (throw 'return-chunk-type nil)))
;;                (dolist (chunk-data chunk-list)
;;                  (when (looking-at (caddr chunk-data))
;;                    (throw 'return-chunk-type (car chunk-data))))
;;                (error "This shouldn't happen."))

;;               ((and (eq forward/backward 'backward)
;;                     (eq beg/end 'beg))
;;                (condition-case exception
;;                    (progn
;;                      (re-search-backward any-begin)
;;                      (goto-char (match-end 0)))
;;                  ('error
;;                   (goto-char (point-min))
;;                   (throw 'return-chunk-type nil)))
;;                (dolist (chunk-data chunk-list)
;;                  (when (looking-back (cadr chunk-data))
;;                    (throw 'return-chunk-type (car chunk-data))))
;;                (error "This shouldn't happen."))

;;               ((and (eq forward/backward 'backward)
;;                     (eq beg/end 'end))
;;                (condition-case exception
;;                    (progn
;;                      (re-search-backward any-end)
;;                      (goto-char (match-end 0)))
;;                  ('error
;;                   (goto-char (point-min))
;;                   (throw 'return-chunk-type nil)))
;;                (dolist (chunk-data chunk-list)
;;                  (when (looking-back (caddr chunk-data))
;;                    (throw 'return-chunk-type (car chunk-data))))
;;                (error "This shouldn't happen."))

;;               (t (error "This shouldn't happen."))))))

;;       (gpb-log-message 'gpb-chunks-find-chunk-boundary "gpb-chunks-find-chunk-boundary: %s" result)
;;       result)))

(defun gpb-chunks-goto-previous-boundary ()
  (interactive)
  (let ((data (gpb-chunks-find-next-boundary nil t)))
    (message "%s" data)
    (goto-char (nth 2 data))))

(defun gpb-chunks-goto-next-boundary (&optional pos)
  (interactive)
  (let ((data (gpb-chunks-find-next-boundary)))
    (message "%s" data)
    (goto-char (nth 3 data))))

(defun gpb-chunks-find-next-boundary (&optional pos move-backward)
  "Return information about the next chunk boundary.

Returns a list of the form (chunk-symbol beg/end from to)"
  (setq pos (or pos (point)))
  (let* ((chunk-list gpb-chunks-buffer-chunks)
         ;;(whole-lines gpb-chunks-chunks-have-whole-lines)
         (any-boundary (concat ;; Open first group
                        "\\(?:"
                        ;; All begin regexs
                        (gpb-util-join-strings
                         (mapcar 'cadr chunk-list)
                         "\\)\\|\\(?:")
                        ;; Join groups
                        "\\)\\|\\(?:"
                        ;; All end regex's
                        (gpb-util-join-strings
                         (mapcar 'caddr chunk-list)
                         "\\)\\|\\(?:")
                        ;; Close last group
                        "\\)")))
    (save-excursion
      (goto-char pos)
      (save-match-data
        (catch 'return-boundary-info
          (cond
           ((not move-backward)
            (condition-case exception
                (progn
                  (re-search-forward any-boundary)
                  (goto-char (match-beginning 0)))
              ('error
               (throw 'return-boundary-info
                      `(postdocument beg ,(point-max) ,(point-max)))))
            ;; Now determine the type of boundary that we found
            ;; chunk data is a list of the form:
            ;;       (chunk-symbol beg-regex end-regex)
            (dolist (chunk-data chunk-list)
              (when (looking-at (cadr chunk-data))
                (throw 'return-boundary-info
                       (list (car chunk-data) 'beg
                             (match-beginning 0) (match-end 0))))
              (when (looking-at (caddr chunk-data))
                (throw 'return-boundary-info
                       (list (car chunk-data) 'end
                             (match-beginning 0) (match-end 0))))
              (error "This shouldn't happen.")))

           (move-backward
            (condition-case exception
                (re-search-backward any-boundary)
              ('error
               (set-match-data (list (point-min-marker)
                                     (point-min-marker)))
               (throw 'return-boundary-info
                      `(predocument end ,(point-min) ,(point-min)))))
            ;; Now determine the type of boundary that we found
            (dolist (chunk-data chunk-list)
              (when (looking-at (cadr chunk-data))
                (throw 'return-boundary-info
                       (list (car chunk-data) 'beg
                             (match-beginning 0) (match-end 0))))
              (when (looking-at (caddr chunk-data))
                (throw 'return-boundary-info
                       (list (car chunk-data) 'end
                             (match-beginning 0) (match-end 0))))
              (error "This shouldn't happen.")))

           (t (error "This shouldn't happen."))))))))

(defun gpb-chunks-get-current-chunk (&optional pos)
  "Returns a list of the form (chunk-symbol beg end) where
beg <= pos < end unless pos is at the end of the buffer.

chunk-symbol can be the symbol 'outside-all-chunks or 'bad-chunk
"
  (interactive)
  (setq pos (or pos (point)))
  (let* ((prev-data (gpb-chunks-find-next-boundary pos t))
         (prev-symbol (nth 0 prev-data))
         (prev-beg/end (nth 1 prev-data))
         (prev-beg (nth 2 prev-data))
         (prev-end (nth 3 prev-data))
         (next-data (gpb-chunks-find-next-boundary pos))
         (next-symbol (nth 0 next-data))
         (next-beg/end (nth 1 next-data))
         (next-beg (nth 2 next-data))
         (next-end (nth 3 next-data)))
    (cond
     ;; There is a special case is where the point sits in front of a
     ;; chunk closing regular expression.  In this case, we need to
     ;; respect beg <= pos < end by moving to the chunk after the
     ;; point.
     ((and (eq next-beg/end 'end) (eq pos next-beg) (not (eobp)))
      (gpb-chunks-get-current-chunk (max next-end (1+ (point)))))
     ;; Between two begin regexs
     ((and (eq prev-beg/end 'beg) (eq next-beg/end 'beg))
      (list 'bad-chunk prev-end next-end))
     ;; Between to end regexs
     ((and (eq prev-beg/end 'end) (eq next-beg/end 'end))
      (list 'bad-chunk prev-beg next-beg))
     ;; Between an end and a begin
     ((and (eq prev-beg/end 'end) (eq next-beg/end 'beg))
      (list 'outside-all-chunks prev-beg next-end))
     ;; Between a begin and an end
     ((and (eq prev-beg/end 'beg) (eq next-beg/end 'end))
      (if (eq prev-symbol next-symbol)
          (list prev-symbol prev-end next-beg)
        (list 'bad-chunk prev-end next-beg))))))


;; (defun gpb-chunks-get-current-chunk-old (&optional pos)
;;   "Returns a list of the form (chunk-symbol beg end) where
;;   beg <= pos < end.

;;   chunk-symbol can be the symbol 'outside-all-chunks or 'bad-chunk
;;   "
;;   (interactive)
;;   (setq pos (or pos (point)))
;;   (if (not gpb-chunks-buffer-chunks)
;;       ;; There are no chunks defined
;;       (list 'outside-all-chunks (point-min) (point-max))
;;     (save-excursion
;;       (let* ((chunk-list gpb-chunks-buffer-chunks)
;;              (whole-lines gpb-chunks-chunks-have-whole-lines)
;;              (any-begin (concat "\\(?:"
;;                                 (gpb-util-join-strings
;;                                  (mapcar 'cadr chunk-list)
;;                                  "\\)\\|\\(?:")
;;                                 "\\)"))
;;              (any-end (concat "\\(?:"
;;                               (gpb-util-join-strings
;;                                (mapcar 'caddr chunk-list)
;;                                "\\)\\|\\(?:")
;;                               "\\)"))
;;              previous-point chunk-symbol)
;;         ;; Move back outside of any chunk
;;         (forward-line -1)
;;         (re-search-backward any-end nil 'point-min-if-fail)
;;         (when whole-lines (forward-line 0))
;;         (catch 'return-chunk
;;           (gpb-chunks-get-current-chunk-1))))))

;; (defun gpb-chunks-get-current-chunk-1 ()
;;   "This subroutine assumes the point is
;;   1. <= pos, and
;;   2. at the end of a chunk or the beginning of the buffer."
;;   ;; Previous point is now at the end of a chunk/beginning of buffer
;;   (setq previous-point (point))
;;   ;; If the next search fails, there are no chunks after previous-point
;;   (condition-case exception
;;       (re-search-forward any-begin)
;;     ('search-failed
;;      (throw 'return-chunk (list 'outside-all-chunks
;;                                 previous-point (point-max)))))
;;   ;; If we want whole lines, we go one line past begin-regex and
;;   ;; we go to the beginning of the line containing the end-regex
;;   (when (and whole-lines (not (bolp))) (forward-line 1))
;;   ;; In the follow case pos is between chunks
;;   (when (> (point) pos)
;;     (throw 'return-chunk (list 'outside-all-chunks
;;                                previous-point (point))))
;;   ;; We now determine what kind of chunk we just started
;;   (save-excursion
;;     (goto-char (match-beginning 0))
;;     (setq chunk-symbol
;;           (catch 'found-chunk-type
;;             (dolist (symbol-beg-end chunk-list)
;;               (when (looking-at-p (cadr symbol-beg-end))
;;                 (setq chunk-beg (cadr symbol-beg-end)
;;                       chunk-end (caddr symbol-beg-end))
;;                 (throw 'found-chunk-type (car symbol-beg-end))))
;;             (error "Couldn't determine chunk symbol"))))
;;   ;; previous-point now points to the start of a chunk which
;;   ;; corresponds to chunk-symbol
;;   (setq previous-point (point))
;;   ;; If the next search fails, we are in a chunk that is not closed
;;   (condition-case exception
;;       (re-search-forward any-end)
;;     ('search-failed
;;      (throw 'return-chunk (list 'bad-chunk previous-point (point-max)))))
;;   ;; Check for incorrect closure
;;   (save-excursion
;;     (goto-char (match-beginning 0))
;;     (unless (looking-at-p chunk-end) (setq chunk-symbol 'bad-chunk)))
;;   ;; If we want whole lines, we go one line past begin-regex and
;;   ;; we go to the beginning of the line containing the end-regex
;;   (when whole-lines (forward-line 0))
;;   ;; In this case, pos is inside the given block
;;   (when (> (point) pos)
;;     (throw 'return-chunk (list chunk-symbol previous-point (point))))
;;   ;; Point is now <= pos and we are at the end of block
;;   (gpb-chunks-get-current-chunk-1))

;; (defun gpb-chunks-get-current-chunk--determine-chunk-type ()
;;   (save-excursion
;;     (goto-char (match-beginning 0))
;;     (catch 'found-chunk-type
;;       (dolist (symbol-beg-end chunk-list)
;;         (when (looking-at-p (cadr symbol-beg-end))
;;           (setq chunk-symbol (car symbol-beg-end)
;;                 chunk-beg (cadr symbol-beg-end)
;;                 chunk-end (caddr symbol-beg-end))
;;           (throw 'found-chunk-type nil)))
;;       (error "Couldn't determine chunk symbol"))))

;; (defun gpb-chunks-mark-chunk ()
;;   (interactive)
;;   (multi-find-mode-at)
;;   (set-mark (cadr multi-mode-list))
;;   (goto-char (caddr multi-mode-list))
;;   (backward-char)
;;   (activate-mark))

;; (setq chunk-list '((pyplot "\\\\begin{pyplot}" "\\\\end{pyplot}")
;;                    (python "\\\\begin{python}" "\\\\end{python}")))

(defun gpb-chunks-narrow-to-chunk (&optional pos)
  (interactive)
  (setq pos (or pos (point)))
  (let* ((symbol-beg-end (gpb-chunks-get-current-chunk pos))
         (symbol (nth 0 symbol-beg-end))
         (beg    (nth 1 symbol-beg-end))
         (end    (nth 2 symbol-beg-end))
         (gpb-log-enabled gpb-chunks--debug)
         (gpb-log-label   'gpb-chunks-narrow-to-chunk))
    (gpb-log-message 'gpb-chunks-narrow-to-chunk (format (concat "gpb-chunks-narrow-to-chunk is "
                                     "narrowing to %s region:") symbol))
    (gpb-chunks-log-region beg end)
    (narrow-to-region beg end)))

(defun gpb-chunks-goto-previous-chunk ()
  (interactive)
  (gpb-chunks-goto-chunk-start)
  (backward-char)
  (gpb-chunks-goto-chunk-start))

(defun gpb-chunks-goto-chunk-start ()
  (interactive)
  (let ((symbol-beg-end (gpb-chunks-get-current-chunk)))
    (message "symbol-beg-end: %s" symbol-beg-end)
    (goto-char (cadr symbol-beg-end))))

(defun gpb-chunks-goto-next-chunk ()
  (interactive)
  (let ((symbol-beg-end (gpb-chunks-get-current-chunk)))
    (message "symbol-beg-end: %s" symbol-beg-end)
    (goto-char (caddr symbol-beg-end))))

(defun gpb-chunks-break-region-into-chunks (beg end)
  (let (result symbol-beg-end)
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (setq symbol-beg-end (gpb-chunks-get-current-chunk))
        (push (list (car symbol-beg-end)
                    (max beg (cadr symbol-beg-end))
                    (min end (caddr symbol-beg-end)))
              result)
        (goto-char (caddr symbol-beg-end))))
    (nreverse result)))

(defun gpb-chunks-list-chunks ()
  "List the chunk boundaries in a new buffer.
   Just a debugging function."
  (interactive)
  (let (result symbol-beg-end)
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (setq symbol-beg-end (gpb-chunks-get-current-chunk))
        (push symbol-beg-end result)
        (goto-char (caddr symbol-beg-end))))
    (switch-to-buffer (get-buffer-create (concat (buffer-name) "-chunks")))
    (erase-buffer)
    (insert "chunks:\n" )

    (dolist (chunk (nreverse result))
      (insert (format "  %s\n" chunk)))))

(defun gpb-chunks-fontify-region-safe (beg end &optional loudly)
  "Calls gpb-chunks-fontify-region and ensures that lisp errors
  aren't swallowed inside of redisplay where we can't see them."
  (condition-case exception
      ;;(font-latex-fontify-region  beg end loudly)
      (gpb-chunks-fontify-region beg end loudly)
    ('error
     (setq font-lock-set-defaults nil) ;; gpb-debug-flag t)
     (font-lock-set-defaults)
     (message "Error %s in %s" exception 'gpb-chunks-fontify-region))))

;; Some variables to catch recursion.  This is an error...
(defvar gpb-chunks-fontify-region--recursion nil)
(defvar gpb-chunks-fontify-region--recursion-first t)

(defun gpb-chunks-fontify-region (beg end &optional loudly)
  "Chunk-aware fontification function."
  (let* ((modified (buffer-modified-p))
         (buffer-undo-list t)
         (inhibit-read-only t)
         (inhibit-point-motion-hooks t)
         (inhibit-modification-hooks t)
         (recursing gpb-chunks-fontify-region--recursion)
         (gpb-chunks-fontify-region--recursion t)
         (gpb-log-enabled gpb-chunks--debug)
         (gpb-log-label   'gpb-chunks-post-command-hook)
         ;; Move beg and end to include entire chunks before and after
         ;; When beg lies exactly at the beginning of a chunk, we also
         ;; include the previous chunk.  The is useful to keep
         ;; bad chunks up to date.
         (beg (let ((temp (nth 1 (gpb-chunks-get-current-chunk beg))))
                (if (eq beg temp)
                    (nth 1 (gpb-chunks-get-current-chunk
                            (max (point-min) (1- beg))))
                  temp)))
         (end (nth 2 (gpb-chunks-get-current-chunk end)))
         ;; Don't screw up transient mark mode.
         deactivate-mark
         mark-active)
    (gpb-log-message 'gpb-chunks-fontify-region "gpb-chunks-fontify-region is acting on:")
    (gpb-chunks-log-region beg end)
    (if recursing
        (progn
          (message "Error: Recursion in gpb-chunks-fontify-region")
          (when gpb-chunks-fontify-region--recursion-first
            (backtrace)
            (setq gpb-chunks-fontify-region--recursion-first nil)))
      (unwind-protect
          (save-restriction
            (widen)
            (dolist (symbol-beg-end (gpb-chunks-break-region-into-chunks
                                     beg end))
              (let* ((chunk-symbol (nth 0 symbol-beg-end))
                     (beg          (nth 1 symbol-beg-end))
                     (end          (nth 2 symbol-beg-end))
                     (font-lock-dont-widen t)
                     ;; (symbol-beg-end-2 (gpb-chunks-get-current-chunk beg))
                     ;; (beg-2 (nth 1 symbol-beg-end-2))
                     ;; (end-2 (nth 2 symbol-beg-end-2))
                     fontify-region
                     ;; You have to reset font-lock-set-defaults to make
                     ;; font-lock-set-defaults work.
                     (font-lock-set-defaults nil)
                     ;; font-lock-defaults will always be defined
                     (font-lock-defaults
                      (or (cdr (assoc chunk-symbol
                                      gpb-chunks-font-lock-defaults-alist))))
                     ;; (cdr (assoc 'outside-all-chunks
                     ;;             gpb-chunks-font-lock-defaults-alist))))
                     ;; chunk-syntax-table can be nil
                     (chunk-syntax-table
                      (cdr (assoc chunk-symbol gpb-chunks-syntax-table-alist)))
                     (chunk-fl-variables
                      (cdr (assoc chunk-symbol gpb-chunks-font-lock-variables))))
                (if (eq chunk-symbol 'bad-chunk)
                    (progn
                      (gpb-log-message 'gpb-chunks-fontify-region "Removing fontification from bad region:")
                      (gpb-chunks-log-region beg end)
                      (font-lock-unfontify-region beg end)
                      (put-text-property beg end 'face
                                         '(gpb-chunk-bad-chunk-face)))
                  (with-syntax-table (or chunk-syntax-table (syntax-table))
                    (font-lock-set-defaults)
                    (dolist (var-value chunk-fl-variables)
                      (set (car var-value) (cdr var-value)))
                    (when (eq font-lock-fontify-region-function
                              'gpb-chunks-fontify-region)
                      (error (concat "Chunk type %s does not set "
                                     "font-lock-fontify-region-function.")
                             chunk-symbol))
                    (gpb-chunks-narrow-to-chunk beg)
                    (gpb-log-message
                     "Now fontifiying this region using the function: %s"
                     font-lock-fontify-region-function)
                    ;;(font-lock-unfontify-region beg end)
                    (font-lock-fontify-region beg end loudly)
                    ;; (setq fontify-region font-lock-fontify-region-function)
                    ;; (gpb-util-debug-message "  (%S %s %s)"
                    ;;                         fontify-region beg end)
                    ;; (funcall fontify-region beg end loudly)
                    (widen))
                  ;; If we have a syntax table, we may as well put it on
                  ;; the text we just highlighted
                  (when chunk-syntax-table
                    (put-text-property beg end
                                       'syntax-table chunk-syntax-table))
                  ))))
        ;; The following forms are the body of a unwind-protect so we
        ;; don't leave the font-lock variables in a bad state on error.
        ;; *** This seems to cause an infinite loop? ***
        (setq font-lock-set-defaults nil)
        (font-lock-set-defaults)
        (setq font-lock-fontify-region-function 'gpb-chunks-fontify-region-safe)
        ))))

(provide 'gpb-chunks)
