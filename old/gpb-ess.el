;;
;;  Customization related to the ESS package
;;
;;  See: https://ess.r-project.org/
;;

(require 'cl-lib)
(require 'ffap)
(require 'tramp)
(require 'ess-r-mode)
(require 'gpb-r-mode)

;; For `string-trim'.
(require 'subr-x)

(defvar gpb:ess-start-of-output-marker
  "START: 75b30f72-85a0-483c-98ce-d24414394ff9"
  "Arbitrary string used to denote the end of R output.")

(defvar gpb:ess-end-of-output-marker
  "END: 75b30f72-85a0-483c-98ce-d24414394ff9"
  "Arbitrary string used to denote the end of R output.")

(defvar gpb:ess-primary-interpeter-buffer nil
  "The primary interpreter buffer")

(defun gpb:inferior-ess-send-or-copy-input ()
  "Send the current line or copy a previous line to the current line.

This is a replacement for the function `inferior-ess-send-input'
which immediately sends input when called on a previous line in
an ESS inferior buffer."
  (interactive)
  (cond
   ;; If the input looks like "[Evaluate lines 11-22 in scratch.R]"
   ;; evaluate the region.
   ((and (comint-after-pmark-p)
         (save-excursion
           (beginning-of-line)
           (looking-at-p "^> *\\[Evaluate lines [0-9]+-[0-9]+ in .*\\]")))
    (save-excursion
      (beginning-of-line)
      (save-match-data
        (re-search-forward
         "^> *\\[Evaluate lines \\([0-9]+\\)-\\([0-9]+\\) in \\(.*\\)\\]")
        (let* ((proc (get-buffer-process (current-buffer)))
               (pmark (process-mark proc))
               (initial-pmark (marker-position pmark))
               (line1 (string-to-number (match-string 1)))
               (line2 (string-to-number (match-string 2)))
               (buf-name (match-string 3))
               beg end)
          (delete-region initial-pmark (save-excursion (end-of-line) (point)))
          (with-current-buffer buf-name
            (goto-line line1)
            (setq beg (point))
            (goto-line line2)
            (end-of-line)
            (setq end (point))
            (gpb:ess-eval-region beg end))))))

   ((get-text-property (point) :region-beg)
    (let ((beg (get-text-property (point) :region-beg))
          (end (get-text-property (point) :region-end)))
      (with-current-buffer (marker-buffer beg)
        (gpb:ess-eval-region beg end))))

   ((comint-after-pmark-p)
    (inferior-ess-send-input))
   (t
    (let ((old-input (funcall comint-get-old-input)))
      (goto-char (point-max))
      (while (looking-back "\n")
        (backward-char))
      (insert old-input)))))


;; Additional support for text objects
(defun gpb:ess-eval-text-object (obj start end)
  "Evalute text object in the ESS process associated with the buffer."

  ;; It is unlikely that ESS is going to start the process correctly.
  ;; Better to just error out if it is going to try.  We do want to allow
  ;; it to search for existing interpreters as that logic seems pretty
  ;; involved.  See `ess-request-a-process'."
  (cond
   ((eq obj 'ess-test-func)
    (let* ((cmd (format "cat(pkgload::pkg_name('%s'), fill = TRUE)\n"
                        (file-local-name (buffer-file-name))))
           (package-name (ess-string-command cmd nil 1)))
      ;; Test files are evaluated in the appropriate package namespace with
      ;; the test file dirctory as the working directory.
      (gpb:ess-eval-region start end package-name default-directory)))
   (t
    (gpb:ess-eval-region start end))))


(defun gpb:ess-insert-browser ()
  "Insert \"browser()\" at the point as save the buffer."
  (interactive)
  (save-match-data
    (let ((pt (point)))
      (beginning-of-line)
      ;; I don't understand why save-excursion doesn't do what I want here.
      (let ((pt (point)))
        (insert "browser()\n")
        (goto-char pt))
      (indent-according-to-mode))))


(defun gpb:ess-debug-track-comint-output-filter-function (output)
  (when (and (> (length output) 0)
             (save-excursion
               (beginning-of-line)
               (looking-at-p "Browse\\[[0-9]+\\]>")))
    (save-excursion
      (goto-char comint-last-output-start)
      (when (re-search-forward "^debug at \\([^#:]+\\)[#:]\\([0-9]+\\):" nil t)
        (message "Found debug output")
        (let* ((tramp-prefix (file-remote-p default-directory))
               (filename (match-string-no-properties 1))
               (line-number (string-to-number (match-string 2)))
               (buf (or (let ((path (concat (file-name-as-directory
                                             default-directory)
                                            filename)))
                          (and (file-exists-p path) (find-file-noselect path)))
                        (ignore-errors
                          (let ((path (concat (file-name-as-directory
                                               default-directory2)
                                              filename)))
                            (and (file-exists-p path)
                                 (find-file-noselect path))))
                        (let ((path (concat tramp-prefix filename)))
                          (and (file-exists-p path) (find-file-noselect path)))
                        (get-buffer (file-name-nondirectory filename))
                        (progn (message "Can't find %S" filename) nil))))
          (when buf (gpb::ess-show-line buf line-number))))))
  output)


(defun gpb:ess-comint-input-filter (input)
  "Return non-nil if input should be recorded in the history ring."
  (> (length (string-trim input)) 4))


(defun gpb:ess-send-traceback-command ()
  (interactive)
  (ess-send-string (get-buffer-process (current-buffer))
                   "traceback(rev(.traceback()), -1)" t))

(defun gpb:ess-send-quit-command ()
  (interactive)
  (ess-send-string (ess-get-process) "Q" t))


(defun gpb:ess-get-local-filename (filename)
  "Remote any TRAMP prefix from `FILENAME'"
  (if (tramp-tramp-file-p filename)
      (tramp-file-name-localname (tramp-dissect-file-name filename))
    filename))

(defun gpb:ess-save-package ()
  "Save all files in the current package that have been edited."
  (interactive)
  (when (car (ess-r-package-project))
    (let* ((code-dir (cdr (ess-r-package-project)))
           (is-pkg-buf-p (lambda (buf)
                           (string-prefix-p code-dir (buffer-file-name buf))))
           (bufs-visiting-pkg-code (cl-remove-if-not
                                    is-pkg-buf-p (buffer-list))))
      (dolist (buf bufs-visiting-pkg-code)
        (when (buffer-modified-p buf)
          (with-current-buffer buf (save-buffer)))))))


(defun gpb:exit-all-browsers ()
  (interactive)
  (dolist (procname-props ess-process-name-list)
    (let* ((procname (car procname-props))
           (proc (get-process procname))
           (buf (process-buffer proc)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (save-excursion
            (goto-char (marker-position (process-mark proc)))
            (beginning-of-line)
            (when (looking-at-p "^Browse\\[[0-9]+\\]> *")
              (ess-send-string (ess-get-process) "Q" t))))))))


(when (fboundp 'advice-add)
  ;; Automatically save all the package files when you reloading the package.
  (advice-add 'ess-r-devtools-load-package :before 'gpb:ess-save-package)
  (advice-add 'ess-r-devtools-load-package :before 'gpb:exit-all-browsers)
  ;; Automatically reload the package files when you run the tests.
  (advice-add 'ess-r-devtools-test-package
              :before 'ess-r-devtools-load-package))


(defun gpb::ess-show-line (buf line-number &optional pop-to)
  "Show line `line' in `buf' in some other window.

If `pop-to' is non-nil, switch to the buffer in which the line is
displayed."
  (let* ((window (display-buffer buf 'other-window))
         (vertical-margin (and window (/ (window-height window) 4))))
    (with-current-buffer buf
      ;; Force the window to scroll a bit.
      (goto-line (- line-number vertical-margin))
      (set-window-point window (point))
      (redisplay)
      (goto-line (+ line-number vertical-margin))
      (set-window-point window (point))
      (redisplay)

      ;; Move to the
      (goto-line line-number)
      (skip-chars-forward " \t")
      (set-window-point window (point))
      (let ((ov (make-overlay (save-excursion
                                (beginning-of-line)
                                (point))
                              (save-excursion
                                (end-of-line)
                                (when (looking-at-p "\n")
                                  (forward-char 1))
                                (point)))))
        (overlay-put ov 'face 'region)
        (overlay-put ov 'window window)
        (run-at-time 0.5 nil `(lambda () (delete-overlay ,ov)))))
    (when pop-to (select-window window))))


(defun gpb:ess-forward-test (arg)
  "Move to the beginning or end of the current test."
  (interactive "p")
  (let ((pt (point)))
    (cl-case arg
      (1 (or
          ;; First handle the case where the point is inside a test function
          ;; definition.
          (and (re-search-backward "test_that(" nil t)
               ;; Move forward to the opening parenthesis after "that_that"
               (goto-char (1- (match-end 0)))
               ;; Move forward to matching closing parenthesis
               (progn (forward-sexp) t)
               ;; If we are not in front of where we started, we are done.
               (> (point) pt))
          ;; Otherwise, we need to move forward to the next test function.
          (progn
            ;; We should signal an error if this fails.
            (goto-char pt)
            (re-search-forward "test_that(")
            (goto-char (1- (point)))
            (progn (forward-sexp) t)
            (point))))
      (-1 (re-search-backward "test_that("))
      (t (error "Runtime error")))))


(defun gpb:ess-forward-chunk (arg)
  "Move to the beginning or end of the current R Markdown chunk."
  (let ((pt (point)))
    (cl-case arg
      (1 (re-search-forward "```\n")
         (forward-line -1))
      (-1 (re-search-backward "```{")
          (forward-line 1))
      (t (error "Runtime error")))))


;; This guard may not matter due to eager macro expansion.
;; (when (require 'gpb-text-objects nil t)
;;   (gpb-tobj--define-flat-text-object ess-test-func
;;     "A `test_that` test definition."
;;     :forward-func gpb:ess-forward-test)

;;   (gpb-tobj--define-flat-text-object ess-rmarkdown-chunk
;;     "An R Markdown chunk test definition."
;;     :forward-func gpb:ess-forward-chunk))


;; BUGFIX: workaround the fact that devtools::help always prints the help,
;; unlike utils::help which returns an object with a print method.
(defun ess-r-build-help-command--get-package-dir (object dont-ask)
  ;; Ugly hack to avoid tcl/tk dialogues
  (let ((pkgs (ess-get-words-from-vector
               (format (concat "c(as.character(utils::help('%s')),"
                               "  as.character(devtools::find_topic('%s')))\n")
                       object object))))
    (when (> (length pkgs) 1)
      (if dont-ask
          (car pkgs)
        (ess-completing-read "Choose location" pkgs nil t)))))


(defun ess-quit:confirm-quit (&rest args)
  "Confirm before killing the R process."
  (unless (y-or-n-p "Kill R process?")
    (signal 'quit nil)))


(defun gpb:inferior-ess-mode-syntax-propertize-function (beg end)
  "Overwrite the comment syntax on # when it is in a traceback"
  (save-match-data
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "#" end t)
        ;; Mark # as punctation rather than the start of a comment.
        (put-text-property (match-beginning 1) (match-end 1)
                           'syntax-table (string-to-syntax "."))))))


(defun gpb:ess-sniff-out-two-space-indentation ()
  (let ((two-space-count 0) (prev-indent 0))
    ;; Count the number of lines with two space indentation.
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (= (- (current-indentation) prev-indent) 2)
          (cl-incf two-space-count))
        (setq prev-indent (current-indentation))
        (forward-line 1)))

    ;; Now check against some arbitrary threshold.
    (when (> two-space-count 5) (setq ess-indent-offset 2))))


(defun gpb:inferior-ess-tab-command ()
  (interactive)
  (setq this-command (if (comint-after-pmark-p)
                         'ess-complete-object-name
                       'gpb:inferior-ess-next-traceback))
  (call-interactively this-command))


(defun gpb:inferior-ess-next-traceback ()
  (interactive)
  (condition-case exc
      (compilation-next-error 1 nil (point))
    (error (goto-char (point-max)))))


(defun gpb:inferior-ess-previous-traceback ()
  (interactive)
  ;; (compilation-next-error -1 nil (point)) seems to be very slow.
  (let ((new-pt nil))
    (save-excursion
      (when (get-text-property (point) 'compilation-message)
        ;; We are in a link, get out back.
        (goto-char (previous-single-property-change (point) 'compilation-message)))
      ;; Move back to end of previous link.
      (goto-char (previous-single-property-change (point) 'compilation-message))
      ;; Move to start of previous link.
      (unless (get-text-property (point) 'compilation-message)
        (goto-char (previous-single-property-change (point) 'compilation-message)))
      (when (> (point) (point-min))
        (setq new-pt (point))))
    (when new-pt (goto-char new-pt))))


(advice-add 'ess-quit :before 'ess-quit:confirm-quit)


;; (defun ess-r-package-info:remove-tramp-prefix (f &optional dir)
;;   (let* ((pkg-info (funcall f dir))
;;          (root (plist-get pkg-info :root))
;;          (local-path (and root (file-remote-p root 'localname))))
;;     (when local-path
;;       (plist-put pkg-info :root local-path))
;;     pkg-info))

;; (advice-add 'ess-r-package-info :around
;;             'ess-r-package-info:remove-tramp-prefix)

;; (defun ess-r-package-eval-linewise:remove-tramp-prefix
;;     (f command &optional msg p actions pkg-path)
;;   "Remove the TRAMP prefix from the package path."
;;   (let* ((pkg-info (or (ess-r-package-project)
;;                        (ess-r-package-set-package)))
;;          (pkg-path (cdr pkg-info)))
;;     (when (and pkg-path (file-remote-p pkg-path))
;;       (setq pkg-path (file-remote-p pkg-path 'localname)))
;;     (funcall f command msg p actions
;;              (concat "'" (abbreviate-file-name pkg-path) "'"))))

;; (advice-add 'ess-r-package-eval-linewise :around
;;             'ess-r-package-eval-linewise:remove-tramp-prefix)

;; (advice-remove 'ess-r-package-eval-linewise
;;                'ess-r-package-eval-linewise:remove-tramp-prefix)


(defun gpb:ess-indent-one-space-advice (f)
  (let ((ess-indent-offset 1)) (funcall f)))

(advice-add 'ess-roxy-maybe-indent-line :around
            'gpb:ess-indent-one-space-advice)

(advice-add 'ess-roxy-adaptive-fill-function :around
            'gpb:ess-indent-one-space-advice)


(defvar gpb-ess:define-traceback-function
  "local({
       assign('.essrTraceback',
              function() {
                  if (!exists('.Traceback', envir = baseenv())) {
                      cat(gettext('No traceback available'), '\\n')
                      return(invisible(NULL))
                  }
                  x <- get('.Traceback', envir = baseenv())
                  n <- length(x)
                  if (n == 0L) cat(gettext('No traceback available'), '\n')
                  if (n > 0L) {
                      for (i in n:1L) {
                          nLines <- length(x[[i]])
                          code <- paste0(trimws(x[[i]]), collapse = ' ')
                          if (nchar(code) > 80) {
                              code <- substring(code, 1, 77)
                              if (nchar(gsub('[^\"]', '', code)) %% 2)
                                  code <- paste0(code, '\"')
                              code <- paste0(code, ' ...')
                          }
                          cat(paste0(n - i + 1L, ': ', code, '\n'))
                          if (!is.null(srcref <- attr(x[[i]], 'srcref'))) {
                              srcfile <- attr(srcref, 'srcfile')
                              cat(paste0('   at ', srcfile$filename, '#', srcref[1L], '\n'))
                          }
                      }
                  }
              },
              'ESSR')
   })\n"
  "The code for a modified traceback function.")


(defun gpb-ess:symbol-at-point ()
  (let ((symbol-name (or (thing-at-point 'symbol) "")))
    (when symbol-name
      (set-text-properties 0 (length symbol-name) nil symbol-name)
      symbol-name)))


(defun gpb-ess:get-interpreter-buffer (&optional buf)
  "Find the interpreter buffer associated with BUF.

BUF defaults to the current buffer."
  (let ((buf (or buf (current-buffer))) proc)
    (with-current-buffer buf
      (setq proc (or (get-buffer-process buf)
                     (get-process ess-local-process-name)))
      (if proc
          (process-buffer proc)
        gpb:ess-primary-interpeter-buffer))))


(defun gpb:ess-behind-block-paren-p ()
  "Are we looking at a parenthesis that starts a block?

Unlike the default implementation, we only consider a parenthesis
to be the start of a block if the parenthesis is the last thing
on the line.  This gets us indentation that looks like:

x <- (f()
      %>% g
      %>% h)
and

rather than

x <- (f()
  %>% g
  %>% h)

but doesn't not change

x <- (
  f()
  %>% g
  %>% h)

"
  (and (looking-at "( *$")
       (not (ess-ahead-attached-name-p))))

(advice-add 'ess-behind-block-paren-p :override 'gpb:ess-behind-block-paren-p)

(defun gpb:ess-indent-line ()
  (prog1
      (ess-r-indent-line)
    (let ((pt (point))
          (continue t)
          init-depth col)
      (setq init-depth (car (syntax-ppss)))
      (catch 'done
        (setq start (save-excursion
                      (ess-backward-up-list)
                      (unless (looking-at-p "(\\|\\[") (throw 'done t))
                      (point)))

        ;; Look for an outer comma.  If we find one, we are in an argument
        ;; list and should only look back to the start of this argument to
        ;; find the "~" or ":=".
        (save-excursion
          (while (and continue (re-search-backward "," start t))
            (when (<= (car (syntax-ppss)) init-depth)
              (setq continue nil
                    start (point)))))

        (save-excursion
          (setq continue t)
          (beginning-of-line)
          (while (and continue (>= (point) start))
            (if (re-search-backward "~\\|:=" start t)
                (cond
                 ;; If the "~" or ":=" we found is not inside a string or
                 ;; some other nested expression.
                 ((and (null (nth 3 (syntax-ppss)))
                       (<= (car (syntax-ppss)) init-depth))
                  (goto-char (match-end 0))
                  (skip-chars-forward " ")
                  (setq col (current-column)
                        continue nil))
                 ;; Otherwise, keep looking.
                 (t (backward-char)))
              ;; We didn't find a string match
              (throw 'done t))))

        (unless (null col)
          (beginning-of-line)
          (when (looking-at " +")
            (delete-region (match-beginning 0) (match-end 0)))
          (indent-to-column col))))))


(defvar-local gpb-ess:initial-space-output-filter-flag nil
  "Set this flag to delete the next initial space from process output.
The filter function `gpb-ess:initial-space-output-filter' checks this flag.")


(defun gpb:make-functionish-regex (keyword)
  (format "%s%s%s" "^ *\\([^ \t\n]+\\)[ \t\n]*\\(?:<-\\|=\\)[ \t\n]*"
          keyword "[ \t]*("))

(defun gpb-ess:adjust-imenu-definitions ()
  ;; The Data section of the imenu entries is pretty useless.
  (adelete 'ess-imenu-S-generic-expression "Data")

  ;; Only match top-level function definitions.
  (setcdr (assoc "Functions" ess-imenu-S-generic-expression)
          '("^\\([^ \t\n]+\\)[ \t\n]*\\(?:<-\\|=\\)[ \t\n]function[ ]*(" 1))

  (nconc ess-imenu-S-generic-expression
         '(("Tests" "^test_that([\"']\\(.*\\)[\"'], *{" 1)))

  (nconc ess-imenu-S-generic-expression
         `(("Reactives" ,(gpb:make-functionish-regex "reactive") 1)))

  (nconc ess-imenu-S-generic-expression
         `(("renderUIs" ,(gpb:make-functionish-regex "renderUI") 1)))

  (nconc imenu-generic-expression
         `(("observeEvents" ,(gpb:make-functionish-regex "observeEvent") 1))))

(defun gpb:inferior-ess-input-sender (proc string)
  "Replacement for ESS version that doesn't add extra echoing."
  (inferior-ess--interrupt-subjob-maybe proc)
  (let ((comint-input-filter-functions nil)
        (buf (process-buffer proc))
        ;; For whatever reason, the markers in compilation-shell-minor-mode
        ;; slowly get stale and point to the wrong place.  Periodically
        ;; clearing `compilation-locs' fixes this.
        (clrhash-func  (lambda ()
                         (clrhash compilation-locs)
                         (message "Cleared compilation-locs"))))
    (when (and (boundp 'gpb-clrhash-timer)
               (timerp gpb-clrhash-timer))
      (cancel-timer gpb-clrhash-timer))
    ;; (setq-local gpb-clrhash-timer (run-with-timer 1 nil clrhash-func))
    (ess-send-string proc string)))


(defun gpb:ess-set-compilation-search-path ()
  (directory-files-recursively default-directory "R" t))


;; (setq next-error-function #'gpb:ess-next-error)
(defun gpb:ess-next-error (n &optional reset)
  (let* ((msg (compilation-next-error (or n 1) nil
                                      (or compilation-current-error
                                          compilation-messages-start
                                          (point-min))))
         (loc (compilation--message->loc msg))
         (line (compilation--loc->line loc)))
    (compilation-next-error-function n reset)
    (goto-line line)))

(provide 'gpb-ess)
