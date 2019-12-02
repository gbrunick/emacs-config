;;
;;  Customization related to the ESS package
;;
;;  See: https://ess.r-project.org/
;;

(require 'cl-lib)
(require 'ffap)
(require 'tramp)

;; For `string-trim'.
(require 'subr-x)


(defvar gpb:ess-region-file-cache nil
  "An alist mapping from TRAMP remotes to region files.

Each entry has a key corresponding to the TRAMP prefix string as
returned by `file-remote-p' and a value that is a list containing
two filenames for temporary files that are used by
`gpb:ess-eval-region' to allow for evaluation of regions of
code.")

(add-hook 'R-mode-hook 'gpb:R-mode-hook)
(add-hook 'inferior-ess-mode-hook 'gpb:inferior-ess-mode-hook)
(add-hook 'ess-r-post-run-hook 'gpb:ess-post-run-hook)


(defvar gpb:ess-last-eval-region nil
  "The last region that was sent to the interpreter.
Contains a cons of two markers.")

(setq ess-use-auto-complete nil)

(defun gpb:R-mode-hook ()
  ;; Get rid of the annoying "smart underscore" behaviour.
  (local-set-key "_" 'self-insert-command)

  (local-set-key "\C-cb" 'gpb:ess-insert-browser)
  (local-set-key "\C-cq" 'gpb:ess-send-quit-command)
  (local-set-key "\C-c\C-c" 'gpb-ess:save-and-load-command)
  (local-set-key "\C-co" 'gpb:ess-view-data-frame)
  (local-set-key "\C-ct" 'gpb:ess-test-package)

  ;; Override the help function
  (local-set-key "\C-c\C-v" 'gpb-ess:show-help)

  (setq-local ess-indent-with-fancy-comments nil)
  ;; The Data section of the imenu entries is pretty useless.
  (adelete 'ess-imenu-S-generic-expression "Data")
  ;; Only match top-level function definitions.
  (setcdr (assoc "Functions" ess-imenu-S-generic-expression)
          '("^\\([^ \t\n]+\\)[ \t\n]*\\(?:<-\\|=\\)[ \t\n]*function[ ]*(" 1))
  (adelete 'ess-imenu-S-generic-expression "Tests")
  (nconc ess-imenu-S-generic-expression
        '(("Tests" "^test_that([\"']\\(.*\\)[\"'], *{" 1)))

  ;;(gpb:ess-sniff-out-two-space-indentation)
  (setq ess-indent-offset 2)
  (setcdr (assoc 'ess-indent-offset (assoc 'RRR ess-style-alist)) 2)

  ;; Allow movement within camel-case identifiers.
  (subword-mode 1)

  ;; Use etags rather than ESS's custom xref implementation.
  (xref-etags-mode 1)

  ;; Enable tab completion of R object names.
  ;; (setq ess-tab-complete-in-script t)

  (let ((package-item (or (assoc "Package" ess-imenu-S-generic-expression)
                          (assoc "Packages" ess-imenu-S-generic-expression))))
    (setcar package-item "Packages")
    (setcdr package-item '("^.*\\(library\\|require\\)(\\([^,)]*\\)" 2)))

  (when (require 'yasnippet nil t)
    (yas-minor-mode 1))
  (when (require 'gpb-text-objects nil t)
    (gpb-modal--define-command-key "q" 'fill-paragraph t)
    (setq-local execute-text-object-function 'gpb:ess-eval-text-object)
    (gpb-tobj--define-key 'root "a" 'ess-last-eval-region :local t)
    (gpb-tobj--define-key 'root "t" 'ess-test-func :local t)
    (gpb-tobj--define-key 'root "T" 'ess-test-func :local t :backwards t)))


(defun gpb:inferior-ess-mode-hook ()
  (local-set-key "\r" 'gpb:inferior-ess-send-or-copy-input)
  (local-set-key "\C-n" 'comint-next-input)
  (local-set-key "\C-p" 'gpb-comint:previous-input)
  (local-set-key "\C-ct" 'gpb:ess-send-traceback-command)
  (local-set-key "\C-cq" 'gpb:ess-send-quit-command)
  (local-set-key [?\t] 'gpb:inferior-ess-tab-command)
  (local-set-key [(backtab)] 'gpb:inferior-ess-previous-traceback)
  (local-set-key "\C-co" 'gpb:ess-view-data-frame)

  ;; Override the help function
  (local-set-key "\C-c\C-v" 'gpb-ess:show-help)

  ;; Get rid of the annoying "smart underscore" behaviour.
  (local-set-key "_" 'self-insert-command)

  ;; Allow movement within camel-case identifiers.
  (subword-mode 1)

  (setq-local comint-input-filter 'gpb:ess-comint-input-filter)

  ;; Correct some syntax assignments.
  (setq-local syntax-propertize-function
              'gpb:inferior-ess-mode-syntax-propertize-function)

  ;; Customize some faces
  (dolist (face '(underline compilation-error compilation-line-number))
    (face-remap-add-relative face :foreground "royal blue" :weight 'normal))

  ;; Don't underline the " at " part of the traceback.
  (aput 'compilation-error-regexp-alist-alist
        'R '("\\(?: at \\|(@\\)\\(\\([^#()\n]+\\)[#:]\\([0-9]+\\)\\)"
             2 3 nil 2 1))

  ;; This matches Shiny tracebacks like:
  ;;     <reactive> [/nfs/home/trpgh47/src/CrossAssetBeta/R/module_tradeTable.R#136]
  (aput 'compilation-error-regexp-alist-alist
        'R2 '("\\(?:\\w\\|\\s.\\)+ \\[?\\(\\([^[())\n]+\\)#\\([0-9]+\\)\\)" 2 3 nil 2 1))

  ;; Implementation detail of "?" help.
  (add-hook 'comint-redirect-hook 'gpb:show-definition-buffer nil t)

  ;; Track the current line when debugging like pdbtrack in Python.
  (add-hook 'comint-output-filter-functions
            'gpb:ess-debug-track-comint-output-filter-function nil t)

  (when (require 'gpb-text-objects nil t)
    (gpb-modal--define-command-key "g" 'gpb:ess-goto-line t)))


(defun gpb:ess-post-run-hook ()
  ;; Do some custom R process configuration
  ;; Disable magical handling of help.
  (setq comint-input-sender 'inferior-ess-input-sender)
  (ess-string-command gpb-ess:define-traceback-function nil 1)

  ;; Use etags rather than ESS's custom xref implementation.
  (xref-etags-mode 1)

  ;; Not sure what is wrong here, but currently `comint-prompt-regexp'
  ;; doesn't handle the browse prompt correctly.
  (setq-local comint-prompt-regexp inferior-ess-prompt))


(defun gpb:ess-goto-line (arg)
  "Attempt to jump to an R source line.

With a prefix argument, show the line but don't jump."
  (interactive "P")
  (if (and (eq major-mode 'inferior-ess-mode)
           (comint-after-pmark-p)
           comint-last-output-start)

      ;; Move the point before the line that tells us the current statement.
      (save-excursion
        (goto-char comint-last-output-start)
        (and (re-search-forward "^debug at " nil t)
             (gpb:ess-goto-line arg)))

    ;; Otherwise, look for a line number at the point.
    (let ((text (substring-no-properties
                 (buffer-substring (save-excursion
                                     (re-search-backward "[ \t]\\|^")
                                     (skip-chars-forward " \t")
                                     ;; Unit test failures are wrapped in
                                     ;; Failure(@ ... ):
                                     (when (looking-at "Failure(@")
                                       (goto-char (match-end 0)))
                                     (point))
                                   (save-excursion
                                     (re-search-forward "[ \t]\\|$")
                                     (skip-chars-backward " \t):")
                                     (point))))))
      (when (string-match "\\([^#:]+\\)[#:]\\([0-9]+\\):?" text)
        (let* ((tramp-prefix (file-remote-p default-directory))
               (filename (match-string 1 text))
               (line-number (string-to-number (match-string 2 text)))
               (buf (or (let ((path (concat default-directory filename)))
                          (and (file-exists-p path) (find-file-noselect path)))
                        (let ((path (concat tramp-prefix filename)))
                          (and (file-exists-p path) (find-file-noselect path)))
                        (get-buffer (file-name-nondirectory filename))
                        (error "Can't find file %S" filename)))
               (window (display-buffer buf 'other-window))
               (vertical-margin (and window (/ (window-height window) 4))))
          (with-current-buffer buf
            ;; Force the window to scroll a bit.
            (goto-line (- line-number vertical-margin))
            (set-window-point window (point))
            (redisplay)
            (goto-line (+ line-number vertical-margin))
            (set-window-point window (point))
            (redisplay)

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
          (unless arg
            (select-window window)))))))


(defun gpb:show-definition-buffer ()
  (let ((buffer gpb:output-buffer))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (setq-local buffer-read-only t)
      (set-buffer-modified-p nil)
      (goto-char 0)
      ;; Some packages (like `gpb-modal'), get confused if we change
      ;; buffers asyncronously in a callback function.  Running the
      ;; `post-command-hook' helps them get sorted.
      (run-hooks 'post-command-hook))))


(defun gpb:inferior-ess-send-or-copy-input ()
  "Send the current line or copy a previous line to the current line.

This is a replacement for the function `inferior-ess-send-input'
which immediately sends input when called on a previous line in
an ESS inferior buffer."
  (interactive)
  (cond
   ;; If we end the line with a ?, we put the output in another window for
   ;; easier browsing.  This is a workaround for reading function
   ;; definitions.  Ideally, we would prefer to jump to the definition but
   ;; I haven't figures that out yet.
   ((and (comint-after-pmark-p) (looking-back "? *"))
    (skip-chars-backward " ")
    (skip-chars-backward "?")
    (let* ((proc (get-buffer-process (current-buffer)))
           (pmark (process-mark proc))
           (initial-pmark (marker-position pmark))
           (r-object (buffer-substring pmark (point)))
           (buffer (get-buffer-create (concat "*" r-object "*")))
           command)
      (delete-region initial-pmark (save-excursion (end-of-line) (point)))
      ;; Send a blank line and wait for a responds to trigger all the
      ;; proper comint prompt accounting.
      (inferior-ess-send-input)
      (while (or (= (marker-position pmark) initial-pmark)
                 (not (looking-back "> *")))
        (accept-process-output proc 3 nil t))
      ;; Now insert the fake input above.
      (save-excursion
        (goto-char initial-pmark)
        (insert (concat r-object "?"))
        (comint-add-to-input-history (concat r-object "?"))
        (setq comint-input-ring-index nil))

      (let ((obj-class (ess-string-command (format "class(%s)\n" r-object)))
            (dt-command (format "print(%%s, %s)"
                                (mapconcat 'identity
                                           '("nrows = 10000"
                                             "topn = 2000"
                                             "row.names = FALSE")
                                           ", "))))
        (cond
         ((string-match "^Error:" obj-class)
          (setq command r-object))
         ((string-match "data.table" obj-class)
          (setq command (read-string "Print command: "
                                     (format dt-command r-object))))
         ((string-match "data.frame" obj-class)
          (setq command (format "print(%s, max = 10000)" r-object)))
         (t
          (setq command r-object))))


      ;; Now actually send the command and pipe the results to a new buffer.
      (with-current-buffer buffer
        (setq-local buffer-read-only nil)
        (erase-buffer)
        (insert (format "#\n#  %s\n#\n\n" command))
        (ess-r-mode 1))
      (setq-local gpb:output-buffer buffer)
      (comint-redirect-send-command command buffer nil)))


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

  ;; edebug doesn't like this.  Are we using it correctly?
  ;; (cl-letf (((symbol-function 'ess-start-process-specific)
  ;;            (lambda (&rest) (error (concat "No interpreter process is "
  ;;                                           "associated with this buffer.")))))
  ;;   (ess-force-buffer-current "Process: " nil t nil))

  (cond
   ((eq obj 'ess-last-eval-region)
    (when (null gpb:ess-last-eval-region)
      (error "No region has been evaluated yet."))
    (let* ((beg (car gpb:ess-last-eval-region))
           (buf (marker-buffer beg))
           (end (cdr gpb:ess-last-eval-region)))
      (with-current-buffer buf (gpb:ess-eval-region beg end))))
   ((eq obj 'ess-test-func)
    (gpb:ess-eval-region start end "RLSBacktest" default-directory))
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
        (let* ((tramp-prefix (file-remote-p default-directory))
               (filename (match-string 1))
               (line-number (string-to-number (match-string 2)))
               (buf (or (let ((path (concat (file-name-as-directory default-directory) filename)))
                          (and (file-exists-p path) (find-file-noselect path)))
                        (let ((path (concat tramp-prefix filename)))
                          (and (file-exists-p path) (find-file-noselect path)))
                        (get-buffer (file-name-nondirectory filename))
                        (error "Can't find %S" filename))))
          (gpb::ess-show-line buf line-number)))))
  output)


(defun gpb:ess-comint-input-filter (input)
  "Return non-nil if input should be recorded in the history ring."
  (> (length (string-trim input)) 4))


(defun gpb:ess-send-traceback-command ()
  (interactive)
  (ess-send-string (get-buffer-process (current-buffer)) ".essrTraceback()" t))


(defun gpb:ess-send-quit-command ()
  (interactive)
  (ess-send-string (ess-get-process) "Q" t))


(defun gpb:ess-get-region-file-names (&optional where)
  "Get the names of the file used for execution of the region."
  (let* ((default-directory (or where default-directory))
         (remote (file-remote-p default-directory))
         (cache-val (cdr (assoc remote gpb:ess-region-file-cache))))
    (if cache-val
        cache-val
      (let* ((make-file (or (and (fboundp 'make-nearby-temp-file)
                                 'make-nearby-temp-file)
                            'make-temp-file))
             (region-file (funcall make-file "emacs-region-" nil ".R"))
             (wrapper-file (funcall make-file "emacs-region-wrapper-" nil ".R"))
             (new-val (list remote region-file wrapper-file)))
        (push new-val gpb:ess-region-file-cache)
        (cdr new-val)))))


(defun gpb:ess-get-local-filename (filename)
  "Remote any TRAMP prefix from `FILENAME'"
  (if (tramp-tramp-file-p filename)
      (tramp-file-name-localname (tramp-dissect-file-name filename))
    filename))


(defun gpb:ess-make-region-file (beg end &optional where package working-dir)
  "Create an R source file containing a region of code.

We jump through some hoops to ensure that the R source code
references refer to the current buffer, rather than to the
temporary file that is produced.

If PACKAGE is provided, the code is evaluated with the package
namespace.  Otherwise, it is evaluated in the global environment.
If WORKING-DIR, the code is evaluate with the given directory set
to be the working directory."
  (interactive "r")
  (let* ((line-number (line-number-at-pos beg))
         (text (buffer-substring-no-properties
                (save-excursion
                  (goto-char beg)
                  (beginning-of-line)
                  (point))
                (save-excursion
                  (goto-char end)
                  (end-of-line)
                  (point))))
         (region-wrapper-filenames (gpb:ess-get-region-file-names where))
         (region-filename (first region-wrapper-filenames))
         (wrapper-filename (second region-wrapper-filenames))
         (source-filename (or (and (buffer-file-name)
                                   (gpb:ess-get-local-filename
                                    (buffer-file-name)))
                              (buffer-name))))
    (with-temp-file region-filename
      (insert (make-string (1- line-number) ?\n))
      (insert text)
      (insert "\n\n"))
    (message "Wrote %s" region-filename)
    (with-temp-file wrapper-filename
      ;; Wrap everything in local() so our temporary variables don't clash
      ;; with anthing.
      (insert (format "local({\n"))
      (insert (format "    srcFile <- %s\n" (prin1-to-string source-filename)))
      (insert (format "    regionFile <- %s\n"
                      (prin1-to-string
                       (gpb:ess-get-local-filename region-filename))))
      (insert "    src <- srcfilecopy(srcFile, readLines(regionFile),\n")
      (insert "                       timestamp = Sys.time(), isFile = TRUE)\n")
      (insert "    expr <- parse(text = readLines(regionFile), srcfile = src)\n")
      (when working-dir
        (insert (format "wd <- setwd('%s')\n" (file-local-name working-dir)))
        (insert "on.exit(setwd(wd))\n"))
      ;; Eval outside of the local() wrapper, so the code being evaluated
      ;; can change the global environment.
      (if package
          (insert (format "    eval(expr, asNamespace('%s'))\n" package))
        (insert "    eval.parent(expr, 2)\n"))
      (insert (format "})\n")))
    (message "Wrote %s" wrapper-filename)
    wrapper-filename))


(defun gpb:ess-eval-region (beg end &optional package working-dir)
  (interactive "r")
  (let* ((whole-buffer-p (save-restriction (widen) (and (= beg (point-min))
                                                        (= end (point-max)))))
         (end (save-excursion
                (goto-char end) (skip-chars-backward " \n\t") (point)))
         (line1 (line-number-at-pos beg))
         (line2 (line-number-at-pos end))
         (ess-proc (ess-get-process))
         (proc-default-directory (with-current-buffer (process-buffer ess-proc)
                                   default-directory)))

    (setq gpb:ess-last-eval-region (or gpb:ess-last-eval-region
                                       `(,(make-marker) . ,(make-marker))))
    (set-marker (car gpb:ess-last-eval-region) beg)
    (set-marker (cdr gpb:ess-last-eval-region) end)

    (cond
     ;; If the regions is a single line, just send it directly to the
     ;; inferior process.
     ((and (= line1 line2) (null package) (null working-dir))
      (ess-send-string ess-proc (buffer-substring-no-properties beg end) t))
     ;; If the file is up to date and the region is whole file, just source
     ;; the file.
     ((and whole-buffer-p (not (buffer-modified-p)) (null package) (null working-dir))
      (let* ((filename (buffer-file-name))
             (working-dir (ess-string-command "cat(sprintf(\"%s\\n\", getwd()))\n"))
             (localname (or (file-remote-p filename 'localname) filename))
             (relative-name (file-relative-name localname working-dir))
             (cmd (format "source(%s)" (prin1-to-string relative-name))))
        (ess-send-string ess-proc cmd t)))
     ;; Otherwise, write temp files (we actually use two files for this see
     ;; `gpb:ess-make-region-file') and source the appropriate temp file.
     (t
      (let* ((filename (gpb:ess-make-region-file beg end proc-default-directory
                                                 package working-dir))
             (local-filename (or (file-remote-p filename 'localname) filename))
             (cmd (format "source(%s)" (prin1-to-string local-filename)))
             (text (format "[Evaluate lines %s-%s in %s]" line1 line2 (buffer-name))))
        (gpb:exit-all-browsers)
        (ess-send-string ess-proc cmd text)
        (with-current-buffer (process-buffer ess-proc)
          (comint-add-to-input-history text)
          (setq comint-input-ring-index nil)))))

    (display-buffer (process-buffer ess-proc))
    (with-current-buffer (process-buffer ess-proc)
      (goto-char (point-max)))))


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


(defvar gpb:ess-helper-funcs "
   getDebugEnv <- function (reset = FALSE) {
       name <- '* breakpointInfo4 *'
       if (!exists(name, globalenv(), inherit = FALSE)) {
           env <- new.env()
           env$nextFrame <- NULL
           env$stepping <- FALSE
           env$breakpoints <- NULL
           assign(name, env, globalenv(), inherit = FALSE)
       } else {
           env <- get(name, globalenv(), inherit = FALSE)
       }
       env
   }

   getBreakpointString <- function(filename = NULL, line = NULL,
                                   funcName = NULL)
   {
       if (!is.null(filename)) sprintf('%s#%s', filename, line)
       else if (!is.null(funcName)) sprintf('func:%s', funcName)
   }

   addBreakpoint <- function(filename = NULL, line = NULL, funcName = NULL) {
       env <- getDebugEnv()
       string <- getBreakpointString(filename = filename,
                                     line = line,
                                     funcName = funcName)
       env$breakpoints <- c(env$breakpoints, string)
   }

   removeBreakpoint <- function(filename = NULL, line = NULL, funcName = NULL) {
       env <- getDebugEnv()
       string <- getBreakpointString(filename = filename,
                                     line = line,
                                     funcName = funcName)
       env$breakpoints <- Filter(function(bp) bp != string, env$breakpoints)
   }

   listBreakpoints <- function(filename, line) {
       env <- getDebugEnv()
       if (length(env$breakpoints) == 0) {
           cat('No breakpoints set\n')
       } else {
           cat('Breakpoints:\n')
           for (i in seq_along(env$breakpoints)) {
               cat(sprintf('%4i %s\n', i, env$breakpoints[[i]]))
           }
       }
   }

   instrument <- function(x) {
       name <- deparse(substitute(x))
       if (is.function(x)) {
           body(x) <- instrument(body(x))
           # Append the function name to the first srcline call.
           firstSrclineCall <- c(as.list(body(x)[[2]]), name)
           body(x)[[2]] <- as.call(firstSrclineCall)
       } else if(is.call(x) && x[[1]] == as.name('{')) {
           # An compound expression wrapped in curly braces.
           srcrefs <- attr(x, 'srcref')
           subexprs <- vector('list', 2 * length(x) - 1)
           subexprs[[1]] <- as.name('{')
           for (i in 2:length(x)) {
               srcref <- srcrefs[[i]]
               line <- srcref[[1]]
               filename <- attr(srcref, 'srcfile')$filename
               expr <- x[[i]]

               subexprs[[(i - 1) * 2]] <- call('srcline', filename, line,
                                               deparse(expr))
               if (is.call(expr) && expr[[1]] == as.name('for')) {
                   expr[[4]] <- instrument(expr[[4]])
               }
               subexprs[[1 + (i - 1) * 2]] <- expr
           }
           stopifnot(length(subexprs) == 2 * length(x) - 1)
           x <- as.call(subexprs)
       }
       x
   }


   srcline <- function(filename, line, text, funcName = NULL) {
       env <- getDebugEnv()

       enterDebugger <- (
           # Break if we hit a breakpoint
           (sprintf('%s#%s', filename, line) %in% env$breakpoints)
           # Break if we hit the next line in giveen function frame
           || identical(env$nextFrame, frame)
           # Break if stepping
           || env$stepping
           # Break on the first line of a function
           || (!is.null(funcName)
               && sprintf('func:%s', funcName) %in% env$breakpoints))

       # Reset the variables used for 'n' and 's'
       env$nextFrame <- NULL
       env$stepping <- FALSE

       if (enterDebugger) {
           cat(sprintf('debug at %s#%s: %s\n', filename, line, text))
           repeat {
               cat('rdb> ')
               cmd <- readLines(n = 1)
               if (cmd == 's') {
                   env$stepping <- TRUE
                   break
               }
               else if(cmd == 'n') {
                   env$nextFrame <- frame
                   break
               }
               else if(cmd == 'c') {
                   break
               } else if(cmd == 'Q') {
                   stop('Quit')
               }
               else cat('Bad command\n')
           }
       }
   }

   enterRepl <- function() {
       # How may calls above this function are we?

       calls <- sapply(sys.calls(), deparse)

       # These don't seem that useful.
       srcRefs <- sapply(sys.calls(), function (x) attr(x, 'srcref'))
       srcLines <- sapply(srcRefs, function (srcref) srcref[[1]])
       srcFiles <- sapply(srcRefs, function (srcref) attr(srcref, 'srcfile')$filename)

       frames <- sys.frames()
       stopifnot(length(calls) == length(frames))

       where <- length(calls) - 1

       repeat {
           if (where > 0) {
               prompt <- paste0(calls[[where]], '> ')
               evalEnv <- frames[[where]]
           }
           else {
               prompt <- 'globalenv> '
               evalEnv <- globalenv()
           }
           # readline doesn't work in Rscript
           cat('rdb> ')
           cmd <- readLines('stdin', n=1)
           if (cmd  == 'c') break
           else if (cmd  == 'Q') stop('quit')
           else if (cmd %in% c('u', 'd', 'w', 'where')) {
               if (cmd  == 'u') {
                   if(where == 0) cat('Already at the top of the stack.\n')
                   else where <- where - 1
               }
               else if (cmd == 'd') {
                   if(where == length(calls)) cat('Already at the bottom of the stack.\n')
                   else where <- where + 1
               }

               # Print the call stack with an indication of the current level.
               if (where == 0) marker <- '->'
               else marker <- '  '
               cat(sprintf('%s 0: globalenv()\n', marker))

               for (i in seq_along(calls)) {
                   if (i == where) marker <- '->'
                   else marker <- '  '
                   cat(sprintf('%s %i: %s\n', marker, i, calls[[i]]))
               }
           } else {
               try({
                   val <- eval(parse(text = cmd), envir = evalEnv)
                   print(val)
               })
           }
       }
   }

  "
  "Various helper functions.  Mainly for debugging.")


(defun gpb:ess-forward-test (arg)
  "Move to the beginning or end of the current test."
  (interactive "p")
  (let ((pt (point)))
    (case arg
      (1 (or
          ;; First handle the case where the point is inside a test function
          ;; definition.
          (and (re-search-backward "test_that(" nil t)
               ;; Move forward to the opening parenthesis after "that_that"
               (goto-char (1- (match-end 0)))
               ;; Move forward to mathcing closing parenthesis
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



;; This guard may not matter due to eager macro expansion.
(when (require 'gpb-text-objects nil t)
  (gpb-tobj--define-text-object ess-last-eval-region (pos &rest modifiers)
    "the last evaluated region"
    ;; This is a just a placeholder object, so the position doesn't matter.
    (list 1 1))

  (gpb-tobj--define-flat-text-object ess-test-func
    "A `test_that` test definition."
    :forward-func gpb:ess-forward-test))



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
      (while (re-search-forward "\\(?:at \\|from \\|\\[\\).*\\.[rR]\\(#\\)[0-9]+" end t)
        ;; Mark the # that lies between a filename and a line number as
        ;; punctation rather than the start of a comment.
        (put-text-property (match-beginning 1) (match-end 1)
                           'syntax-table (string-to-syntax "."))))))


(defun gpb:ess-sniff-out-two-space-indentation ()
  (let ((two-space-count 0) (prev-indent 0))
    ;; Count the number of lines with two space indentation.
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (= (- (current-indentation) prev-indent) 2)
          (incf two-space-count))
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



(defun gpb:ess-download-package-source (url dir &optional tag-dirs-file)
  "Download the package from URL to DIR/<pkgname>_<ver>/<pkgname>.
If TAG-DIRS-FILE is non-nil, we add the downloaded package's R
directory to the list of directories in that file and call
`gpb:ess-update-tags' to refresh the TAGS file.

When called interactively, we prompt the user for the name of an
installed package, and attempt to download the version that is
currently installed."
  (interactive
   (list
    ;; url
    (let ((pkg-ver (gpb:ess-choose-installed-package)))
      (gpb:ess-find-package-source (car pkg-ver) (cdr pkg-ver)))
    ;; dir
    (read-directory-name "Download source to: ")
    ;; tag-dirs-file
    (and (y-or-n-p "Add to TAG_DIRS? ")
         (let ((file (or (gpb:ess-find-tag-dirs-file)
                         tag-dirs-file)))
           (read-file-name
            (if file "Tags file (default TAG_DIRS): " "Tags file: ")
            (file-name-directory file)
            file)))))

  (assert (string-match ".tar.gz$" url))
  (let* ((package-file (file-name-nondirectory url))
         (package-name (car (split-string package-file "_")))
         (package-name-as-dir (file-name-as-directory package-name))
         ;; package-dir is <dir>/<pkgname>_<ver>/
         (package-dir (file-name-as-directory
                       (concat dir (replace-regexp-in-string
                                    ".tar.gz$" "" package-file))))
         (process-buf (with-current-buffer
                          (get-buffer-create "*R package download output*")
                        (erase-buffer)
                        (current-buffer))))

    (when (not (file-directory-p package-dir)) (make-directory package-dir))

    ;; Download the archive file into `package-dir'
    (unless
        (or (zerop (let ((default-directory package-dir))
                     (process-file "wget" nil process-buf nil
                                   "--no-check-certificate"
                                   "--timestamping" url)))
            (and (display-buffer process-buf)
                 (y-or-n-p (concat "Couldn't download package.  "
                                   "Would you like to try the CRAN archive? "))
                 (let ((url (format (concat "https://cran.r-project.org"
                                            "/src/contrib/Archive/%s/%s")
                                    package-name package-file)))
                   (zerop (let ((default-directory package-dir))
                            (process-file "wget" nil process-buf nil
                                          "--no-check-certificate"
                                          "--timestamping" url))))))
      (pop-to-buffer process-buf)
      (error "Download failed"))
    (with-current-buffer process-buf (erase-buffer))

    ;; Now extract the contents of the archive file.  This will create a
    ;; further subdirectory in `package-dir' giving the directory structure
    ;; <dir>/<pkgname>_<ver>/<pkgname>/.
    (unless (zerop (let ((default-directory package-dir))
                     (process-file "tar" nil process-buf nil
                                   "-zxvf" package-file)))
      (pop-to-buffer process-buf)
      (error "Package extraction failed"))
    (kill-buffer process-buf)
    (message "Wrote %s" package-dir)

    (when tag-dirs-file
      (let* ((subdir (file-name-as-directory package-name))
             (source-dir (concat package-dir subdir "R"))
             (base-dir (file-name-directory tag-dirs-file))
             ;;(local-dir (or (file-remote-p source-dir 'localname) source-dir))
             ;; The path relative to the TAG_DIRS file.
             (relative-dir (file-relative-name source-dir base-dir)))
        (with-temp-buffer
          (insert-file-contents tag-dirs-file)
          (goto-char (point-max))
          (skip-chars-backward " \n\t")
          (insert (concat "\n" relative-dir "\n"))
          (write-region (point-min) (point) tag-dirs-file))))
    package-dir))


(defun gpb:ess-choose-installed-package (&optional buf)
  "Prompt the user the name of an installed package.
Returns (package-name . version) cons cell."
  (with-current-buffer (or buf (current-buffer))
    (message "Loading package info...")
    (let* ((cmd (concat
                 ;; Prints Lisp alist from package name to version.
                 "local({"
                 "    pkgs <- installed.packages();"
                 "    cat(paste0('(', "
                 "           paste0('(\"', pkgs[, 'Package'], '\" . \"', "
                 "                  pkgs[, 'Version'], '\")', collapse = ' '), "
                 "           ')'), '\\n');"
                 "})\n"))
           (result (ess-string-command cmd))
           (installed-pkgs (car (read-from-string result)))
           (package (completing-read "Package: " (mapcar 'car installed-pkgs)
                                     nil t)))
      (assoc package installed-pkgs))))


(defun gpb:ess-find-package-source (package version &optional refresh buf)
  "Find URL with the source code for the given version of PACKAGE."
  (with-current-buffer (or buf (current-buffer))
    (let* ((cmd2 (concat
                  ;; Prints Lisp alist from package name to base repository
                  ;; url.  It appears that available.packages() only
                  ;; advertises the most recent version, so we ignore the
                  ;; version information that is returned and munge the URL
                  ;; ourselves to find the source for the version that is
                  ;; currently installed.
                  "local({"
                  "    pkgs <- available.packages();"
                  "    cat(paste0('(', "
                  "           paste0('(\"', pkgs[, 'Package'], '\" . \"', "
                  "                  pkgs[, 'Repository'], '\")', "
                  "                  collapse = ' '), "
                  "           ')'), '\\n');"
                  "})\n"))
           (base-url-alist (or (and (not refresh)
                                    (boundp 'gpb-ess:find-package-source:cache)
                                    gpb-ess:find-package-source:cache)
                               (progn
                                 (message "Loading repo info...")
                                 (car (read-from-string
                                       (ess-string-command cmd2))))))
           (base-url (cdr (assoc package base-url-alist))))
      (setq-local gpb-ess:find-package-source:cache base-url-alist)
      (unless base-url (error "Could not find package: %s" package))
      (format "%s/%s_%s.tar.gz" base-url package version))))


(defmacro gpb:ess-with-working-dir (dir &rest body)
  "Evaluate BODY with the inferior process working directory set to DIR."
  (declare (indent 1) (debug t))
  (let ((saved-dir-symbol (gensym "saved-dir-"))
        (cmd-symbol (gensym "cmd-")))
    `(let ((,saved-dir-symbol (ess-string-command
                               "cat(paste0(getwd(), '\\n'))\n"))
           (,cmd-symbol (format "setwd('%s')\n"
                                (directory-file-name
                                 (or (file-remote-p ,dir 'localname) ,dir)))))
       (ess-command ,cmd-symbol)
       (unwind-protect
           ,@body
         (ess-command (format "setwd('%s')\n" ,saved-dir-symbol))))))


(defun gpb:ess-find-tag-dirs-file (&optional dir)
  "Find TAG_DIRS file in DIR or one of its parents.
DIR defaults to `default-directory'."
  (let* ((init-dir (file-name-as-directory (or dir default-directory)))
         (basename "TAG_DIRS")
         (file (concat init-dir basename)))
    (catch 'no-file
      (while (not (file-exists-p file))
        ;; Apparently moving up to the parent directory requires three steps:
        ;; 1. Get the directory of file
        ;; 2. Remove trailing slash of of directory in 1.
        ;; 3. Get the directory of 2.
        (setq file (let ((next (concat (file-name-directory
                                        (directory-file-name
                                         (file-name-directory file)))
                                       basename)))
                     (when (string= file next)
                       (throw 'no-file nil))
                     next)))
      file)))


(defun gpb:ess-update-tags (&optional tags-dirs-file)
  "Update TAGS file using directories in `tags-dirs-file'.
`tags-dirs-file' is usually a file named TAG_DIRS that sits next to TAGS."
  (interactive
   (list (or (let* ((file-name (buffer-file-name))
                    (base-name (ignore-errors
                                 (file-name-nondirectory file-name))))
               (when (string= base-name "TAG_DIRS") file-name))
             (gpb:ess-find-tag-dirs-file)
             (read-file-name "Tags dirs file: "))))
  (let* ((working-dir (file-name-directory tags-dirs-file))
         (ess-buf (current-buffer))
         (tags-file (concat working-dir "TAGS"))
         (cmd-template (concat "rtags('%s', recursive = TRUE, "
                               "      pattern = '\\\\.[Rr]$', verbose = FALSE, "
                               "      ofile = 'TAGS', append = TRUE)\n"))
         source-dir cmd)
    (with-current-buffer (find-file-noselect tags-dirs-file)
      ;; Delete the current content of the TAGS file.
      (delete-file tags-file)
      (save-excursion
        ;; Now iterate through the lines in TAG_DIRS
        (goto-char (point-min))
        (while (< (point) (point-max))
          (setq source-dir (buffer-substring-no-properties
                            (point)
                            ;; Don't include the newline.
                            (progn (forward-line 1) (1- (point)))))
          (message "Reading %s ..." source-dir)
          ;; Ignore blank lines and lines starting with '#'.
          (when (not (string-match "^\\(#\\| *$\\)" source-dir))
            (setq cmd (format cmd-template source-dir))
            (with-current-buffer ess-buf
              ;; `expand-file-name' behaves in way that breaks
              ;; `etags-file-of-tag' on Windows with a remote
              ;; `default-directory' and an absolute path so we need to use
              ;; relative paths in our TAGS file.  We set the R working
              ;; directory before we call rtags to get relative paths.
              ;;
              ;; (expand-file-name  "/home/user/src/DT_0.5/DT/R/shiny.R"
              ;;                    "/plinkx:SESSION:/home/user/")
              ;; => "c:/home/user/src/DT_0.5/DT/R/shiny.R"
              ;;
              (gpb:ess-with-working-dir working-dir
                (ess-command cmd)))))))

    ;; Now fix up the TAGS file.  The rtags command only keeps the first
    ;; letter of each identifier in the first field of the tag.  This makes
    ;; some Emacs interactions unpleasant, so we copy the value from the
    ;; second field into the first field.  The special characters  and 
    ;; are the field delimiters in the tag file.
    (with-temp-buffer
      (insert-file-contents tags-file)
      (goto-char (point-min))
      (while (re-search-forward "^\\([^\n]*\\)\\([^\n]*\\)" nil t)
        (replace-match (match-string 2) t nil nil 1))
      (write-region (point-min) (point-max) tags-file))))


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


(defun gpb-ess:show-help (object-name)
  "Show help on OBJECT-NAME.

This function doesn't not attempt to provided symbol completion,
so it can be faster then calling `ess-display-help-on-object'
interactively."
  (interactive (list (read-string "R Object: " (gpb-ess:symbol-at-point))))
  (ess-force-buffer-current "Process: " nil t nil)

  (let* ((command (format "help(\"%s\", try.all.packages = FALSE)"
                          object-name))
         (help-buf (get-buffer-create "*R Help*"))
         (proc (get-process ess-local-process-name))
         (proc-buf (process-buffer proc))
         (inhibit-read-only t))
    (setq comint-redirect-filter-functions nil)
    (with-current-buffer help-buf
      (erase-buffer)
      (help-mode)
      (read-only-mode 1)
      (setq comint-redirect-subvert-readonly t))
    (with-current-buffer proc-buf
      (setq-local comint-redirect-hook '(gpb-ess:show-help--comint-redirect-hook))
      (comint-redirect-send-command command help-buf nil t)
      (setq comint-redirect-finished-regexp "^\\(>\\|Selection:\\)"
            comint-redirect-insert-matching-regexp t))))


(defun gpb-ess:show-help--comint-redirect-hook ()
  (let* ((proc-buf (current-buffer))
         (help-buf (get-buffer "*R Help*"))
         (prompt-p (with-current-buffer help-buf
                     (looking-back "^Selection: *")))
         (inhibit-read-only t))
    (message "proc-buf: %S" proc-buf)
    (pop-to-buffer help-buf)
    (cond
     ;; There are multiple matches, so we prompt the user to choose one.
     (prompt-p
      (let ((selection (or (with-local-quit
                             (prin1-to-string (read-number "Selection: " 1)))
                           "1")))
        (with-current-buffer help-buf (erase-buffer))
        (with-current-buffer proc-buf
          (comint-redirect-send-command selection help-buf nil))))

     ;; The help buffer now contains the help text.
     (t
      (with-current-buffer help-buf
        (goto-char (point-max))
        (when (re-search-backward "^> *" nil t) (replace-match ""))
        (skip-chars-backward " \n")
        (forward-line 1)
        (delete-region (point) (point-max))
        (goto-char (point-min))
        (when (looking-at-p "^Rendering development")
          (forward-line 1)
          (delete-region (point-min) (point)))
        (ess-help-underline))))))


(defun gpb-ess:save-and-load-command (arg)
  "Save the current buffer and then source it or reload the package."
  (interactive "P")
  (let* ((filename (buffer-file-name))
         (localname (ignore-errors (file-local-name filename)))
         (ess-proc (ess-get-process))
         dir cmd)
    ;; Get the name of the directory that contains filename.
    (setq dir (ignore-errors (directory-file-name (file-name-directory localname))))
    (when filename (save-buffer))
    (cond
     ((string-suffix-p ".Rmd" localname t)
      (let ((build-cmd
             (or (and (null arg) (boundp 'gpb-ess:build-cmd) gpb-ess:build-cmd)
                 (read-string "Build command: "
                              (format "rmarkdown::render('%s')" localname)))))
        (setq-local gpb-ess:build-cmd build-cmd)
        (ess-send-string ess-proc build-cmd t)))

     ;; If we are in a package, reload the package.
     ((string-equal (ignore-errors (file-name-base dir)) "R")
      (gpb:ess-save-package)
      (setq cmd (format "devtools::load_all('%s', export_all = FALSE)"
                        (directory-file-name (file-name-directory dir))))
      (ess-send-string ess-proc cmd t))

     ;; If we are in a test file, source the file but evaluate in the
     ;; current package namespace with the current directory as the working
     ;; directory.
     ((string-equal (ignore-errors (file-name-base dir)) "testthat")
      (let* ((cmd (format "cat(pkgload::pkg_name('%s'), fill = TRUE)\n"
                          (file-local-name (buffer-file-name))))
             (package-name (ess-string-command cmd nil 1)))
        (gpb:ess-eval-region (point-min) (point-max) package-name dir)))

     ;; Otherwise, just evaluate the buffer contents in the global
     ;; environment.
     (t
      (save-restriction
        (widen)
        (gpb:ess-eval-region (point-min) (point-max)))))))


(defun gpb:ess-view-data-frame ()
  (interactive)
  (let* ((r-expr (funcall comint-get-old-input))
         (tramp-prefix (or (file-remote-p default-directory) ""))
         (cmd (format (concat "filename <- tempfile(fileext = \".xlsx\"); "
                              "openxlsx::write.xlsx(%s, filename); "
                              "cat(sprintf(\"Excel File: %%s\n\", filename))")
                      r-expr))
         (proc (get-buffer-process (current-buffer)))
         file)
    (message "cmd: %S" cmd)
    (ess-send-string proc cmd)
    (accept-process-output proc)
    (save-excursion
      (goto-char (point-max))
      (re-search-backward "Excel File: \\(.*\\)$")
      (setq file (concat tramp-prefix (match-string 1)))
      (setq file (or (file-local-copy file) file))
      (delete-region (match-beginning 0) (match-end 0))
      (when (looking-back "\n> \\+ > $")
        (delete-region (match-beginning 0) (match-end 0)))
      (let ((default-directory ""))
        (shell-command (format "cmd /C \"start %s\"" file))))))


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

;; This matches R Markdown build failures like:
;;     Quitting from lines 257-329 (report.Rmd)
(aput 'compilation-error-regexp-alist-alist
      'R-markdown '("Quitting from lines \\(\\([0-9]+\\)-[0-9]+ (\\(.*\\))\\)" 3 2 nil 1))

(add-to-list 'ess-r-error-regexp-alist 'R-markdown)


(defun gpb:ess-test-package ()
  (interactive)
  (let* ((localname (file-local-name (buffer-file-name)))
         (cmd1 (format "cat(pkgload::pkg_name('%s'), fill = TRUE)\n" localname))
         (package-name (ess-string-command cmd1 nil 0.2))
         (cmd2 (format "pkgload::load_all('%s', export_all = FALSE)" package-name))
         (cmd3 (format "testthat::test_package('%s', reporter = default_reporter())"
                       package-name)))

    (ess-switch-to-end-of-ESS)
    (ess-send-string (ess-get-process) cmd2 t)
    (ess-wait-for-process)
    (ess-send-string (ess-get-process) cmd3 t)))
