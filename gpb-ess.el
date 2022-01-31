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

(defcustom gpb:ess-tmpdir-map nil
  "Override the default temporary directory location.

The value is a alist where the keys are TRAMP prefix strings as
returned by `file-remote-p' and the values are paths to a
temporary directory.  If set, this location is used when writing
region code to a temporary file for execution."
  :type '(alist :key-type (string :tag "TRAMP Prefix")
                :value-type (string :tag "Temp Dir"))
  :group 'gpb-ess)


(defvar gpb:ess-region-file-cache nil
  "An alist mapping from TRAMP remotes to region files.

Each entry has a key corresponding to the TRAMP prefix string as
returned by `file-remote-p' and a value that is a list containing
two filenames for temporary files that are used by
`gpb:ess-eval-region' to allow for evaluation of regions of
code.")

(defvar gpb:ess-primary-interpeter-buffer nil
  "The primary interpreter buffer")

(add-hook 'ess-r-mode-hook 'gpb:ess-r-mode-hook)
(add-hook 'inferior-ess-mode-hook 'gpb:inferior-ess-mode-hook)
(add-hook 'ess-r-post-run-hook 'gpb:ess-post-run-hook)

(eval-after-load 'ess-r-package
  (remove-hook 'shell-mode-hook 'ess-r-package-activate-directory-tracker))

(setq ess-use-auto-complete nil
      ess-use-tracebug t)

(defun gpb:ess-r-mode-hook ()
  (gpb-r-code-mode 1)

  ;; Get rid of the annoying "smart underscore" behaviour.
  (local-set-key "_" 'self-insert-command)

  ;; Clear out some annoying ESS bindings.
  (local-set-key "\C-c\C-p" nil)
  (local-set-key "\C-c\C-n" nil)
  (local-set-key "\C-c\C-b" nil)

  (local-set-key "\C-cb" 'gpb:ess-insert-browser)
  (local-set-key "\C-cq" 'gpb:ess-send-quit-command)
  (local-set-key "\C-co" 'gpb:ess-view-data-frame)
  (local-set-key "\C-ct" 'gpb:ess-test-package)
  (local-set-key "\C-c\C-s" 'gpb:ess-choose-interpreter)
  ;; Override the help function
  (local-set-key "\C-c\C-v" 'gpb-ess:show-help)

  (setq-local ess-indent-with-fancy-comments nil)

  ;; (nconc ess-imenu-S-generic-expression
  ;;        '(("Chunks" "^```{r \\(.*\\)}" 1)))

  ;;(gpb:ess-sniff-out-two-space-indentation)
  (setq ess-indent-offset 2)
  (setcdr (assoc 'ess-indent-offset (assoc 'RRR ess-style-alist)) 2)

  ;; Allow movement within camel-case identifiers.
  (subword-mode 1)

  ;; Use etags rather than ESS's custom xref implementation.
  (xref-etags-mode 1)

  ;; Enable tab completion of R object names.
  ;; (setq ess-tab-complete-in-script t)

  ;; (let ((package-item (or (assoc "Package" ess-imenu-S-generic-expression)
  ;;                         (assoc "Packages" ess-imenu-S-generic-expression))))
  ;;   (setcar package-item "Packages")
  ;;   (setcdr package-item '("^.*\\(library\\|require\\)(\\([^,)]*\\)" 2)))

  (setq-local indent-line-function #'gpb:ess-indent-line)

  (when (require 'yasnippet nil t)
    (yas-minor-mode 1))
  (when (require 'gpb-text-objects nil t)
    (setq-local execute-text-object-function 'gpb:ess-eval-text-object)
    (gpb-tobj--define-key 'root "t" 'ess-test-func :local t)
    (gpb-tobj--define-key 'root "T" 'ess-test-func :local t :backwards t)
    (gpb-tobj--define-key 'root "c" 'ess-rmarkdown-chunk :local t)
    (gpb-tobj--define-key 'root "C" 'ess-rmarkdown-chunk :local t :backwards t)))


(defun gpb:inferior-ess-mode-hook ()
  (local-set-key "\r" 'gpb:inferior-ess-send-or-copy-input)
  (local-set-key "\C-n" 'comint-next-input)
  (local-set-key "\C-p" 'gpb-comint:previous-input)
  (local-set-key "\C-ct" 'gpb:ess-send-traceback-command)
  (local-set-key "\C-cq" 'gpb:ess-send-quit-command)
  (local-set-key [?\t] 'gpb:inferior-ess-tab-command)
  (local-set-key [(backtab)] 'gpb:inferior-ess-previous-traceback)
  (local-set-key "\C-co" 'gpb:ess-view-data-frame)
  ;; (when (require 'gpb-text-objects nil t)
  ;;   (local-set-key "g" 'gpb:ess-goto-line))

  ;; Override the help function
  (local-set-key "\C-c\C-v" 'gpb-ess:show-help)

  ;; Get rid of the annoying "smart underscore" behaviour.
  (local-set-key "_" 'self-insert-command)

  ;; Allow movement within camel-case identifiers.
  (subword-mode 1)

  (setq-local comint-input-filter 'gpb:ess-comint-input-filter)
  (setq-local truncate-lines t)

  ;; Correct some syntax assignments.
  (setq-local syntax-propertize-function
              'gpb:inferior-ess-mode-syntax-propertize-function)

  ;; Customize some faces
  (dolist (face '(underline compilation-error compilation-line-number))
    (face-remap-add-relative face :foreground "royal blue" :weight 'normal))

  ;; ;; Implementation detail of "?" help.
  ;; (add-hook 'comint-redirect-hook 'gpb:show-definition-buffer nil t)

  ;; Track the current line when debugging like pdbtrack in Python.
  (add-hook 'comint-output-filter-functions
            #'gpb:ess-debug-track-comint-output-filter-function nil t)

  (add-hook 'comint-preoutput-filter-functions
            #'gpb-ess:initial-space-output-filter nil t))


(defun gpb:ess-post-run-hook ()
  ;; Do some custom R process configuration
  ;; Disable magical handling of help.

  ;; Setting comint-process-echoes to t can cause things to block with a
  ;; long runnging commmend.
  (setq comint-process-echoes nil
        comint-input-sender 'gpb:inferior-ess-input-sender)
  (ess-string-command gpb-ess:define-traceback-function nil 1)
  (ess-string-command "options(menu.graphics = FALSE)\n" nil 1)

  ;; Use etags rather than ESS's custom xref implementation.
  (xref-etags-mode 1)

  ;; Not sure what is wrong here, but currently `comint-prompt-regexp'
  ;; doesn't handle the browse prompt correctly.
  (setq-local comint-prompt-regexp inferior-ess-prompt)

  ;; ESS attempts to track the current directory interactively, but
  ;; this seems to screw up the compiliation mode links.
  (setq-local ess-getwd-command nil)

  ;; Use `compilation-shell-minor-mode' for parsing R source info output.
  (setq-local compilation-error-regexp-alist
              gpb:ess-inferior-compilation-error-regexp-alist)

  ;; We want to void adding the extra keywords in
  ;; `compilation-mode-font-lock-keywords'.
  (let ((compilation-mode-font-lock-keywords nil))
    (compilation-shell-minor-mode 1))

  (setq-local next-error-function #'gpb:ess-next-error))


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


;; (defun gpb:show-definition-buffer ()
;;   (let ((buffer gpb:output-buffer))
;;     (pop-to-buffer buffer)
;;     (with-current-buffer buffer
;;       (setq-local buffer-read-only t)
;;       (set-buffer-modified-p nil)
;;       (goto-char 0)
;;       ;; Some packages (like `gpb-modal'), get confused if we change
;;       ;; buffers asyncronously in a callback function.  Running the
;;       ;; `post-command-hook' helps them get sorted.
;;       (run-hooks 'post-command-hook))))


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


(defun gpb:ess-get-region-file-names (&optional where)
  "Get the names of the file used for execution of the region."
  (let* ((default-directory (or where default-directory))
         (remote (file-remote-p default-directory))
         (cache-val (cdr (assoc remote gpb:ess-region-file-cache))))
    (if cache-val
        cache-val
      (let* ((region-file (gpb:ess-get-tmp-file "r-data-" nil ".R"))
             (wrapper-file (gpb:ess-get-tmp-file "emacs-region-wrapper-"
                                                 nil ".R"))
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
      (insert text)

      ;; If all the lines are in a roxygen comment, remove the comment
      ;; prefix.
      (goto-char (point-min))
      (while (looking-at "^#'") (forward-line))
      (when (eobp)
        (goto-char (point-min))
        (while (looking-at "^#'")
          (replace-match "  ")
          (forward-line)))

      (goto-char (point-min))
      (insert (make-string (1- line-number) ?\n))
      (goto-char (point-max))
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

    (cond
     ;; If the regions is a single line, just send it directly to the
     ;; inferior process.
     ((and (= line1 line2) (null package) (null working-dir))
      (ess-send-string ess-proc (buffer-substring-no-properties beg end) t))

     ;; If the file is up to date, the region is whole file, and the file
     ;; and inferior process are on the same machine, source the file.
     ((and whole-buffer-p
           (not (buffer-modified-p))
           (string-equal (file-remote-p default-directory)
                         (file-remote-p proc-default-directory)))
      (let* ((filename (buffer-file-name))
             (working-dir (ess-string-command
                           "cat(sprintf(\"%s\\n\", getwd()))\n"))
             (localname (or (file-remote-p filename 'localname) filename))
             (relative-name (file-relative-name localname working-dir))
             (dir (ignore-errors (directory-file-name
                                  (file-name-directory localname))))
             (in-test-file (string-equal (ignore-errors (file-name-base dir))
                                         "testthat"))
             (cmd (format (cond
                           ((and in-test-file (not (null package)))
                            (format "testthat::test_file(%%s, env = new.env(parent = loadNamespace(\"%s\")))"
                                    package))
                           (in-test-file "testthat::test_file(%s)")
                           (t "source(%s)"))
                          (prin1-to-string relative-name))))
        (ess-send-string ess-proc cmd t)
        (display-buffer (process-buffer ess-proc))
        (with-current-buffer (process-buffer ess-proc)
          (goto-char (point-max)))))
     ;; Otherwise, write temp files (we actually use two files for this see
     ;; `gpb:ess-make-region-file') and source the appropriate temp file.
     (t
      (let* ((filename (gpb:ess-make-region-file beg end proc-default-directory
                                                 package working-dir))
             (local-filename (or (file-remote-p filename 'localname) filename))
             (cmd (format "source(%s, print.eval = TRUE)"
                          (prin1-to-string local-filename)))
             (text (format "[Evaluate lines %s-%s in %s]"
                           line1 line2 (buffer-name))))
        ;; Sometimes we want to execute a snippet while debugging, so maybe
        ;; we don't want to exit all browsers?
        ;; (gpb:exit-all-browsers)
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
    (case arg
      (1 (re-search-forward "```\n")
         (forward-line -1))
      (-1 (re-search-backward "```{")
          (forward-line 1))
      (t (error "Runtime error")))))


;; This guard may not matter due to eager macro expansion.
(when (require 'gpb-text-objects nil t)
  (gpb-tobj--define-flat-text-object ess-test-func
    "A `test_that` test definition."
    :forward-func gpb:ess-forward-test)

  (gpb-tobj--define-flat-text-object ess-rmarkdown-chunk
    "An R Markdown chunk test definition."
    :forward-func gpb:ess-forward-chunk))


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
          (unless (string-prefix-p "#" source-dir)
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
                  (ess-command cmd))))))))

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


(defun gpb-ess:show-help (object-name)
  "Show help on OBJECT-NAME.

This function doesn't not attempt to provided symbol completion,
so it can be faster then calling `ess-display-help-on-object'
interactively."
  (interactive (list (read-string "R Object: " (gpb-ess:symbol-at-point))))

  (let* ((proc-buf (gpb-ess:get-interpreter-buffer))
         (cmd (format "print(help(\"%s\", try.all.packages = FALSE))"
                      object-name)))
    (gpb:ess-send-command cmd 'gpb-ess:show-help--callback)))


(defun gpb-ess:show-help--callback (complete)
  (let* ((server-buf (current-buffer))
         (proc (get-buffer-process server-buf))
         (help-buf (get-buffer-create "*R Help*"))
         ;; start is sometimes nil
         (start (save-excursion (goto-char (point-max))
                                (ignore-errors
                                  (re-search-backward
                                   (concat "^" gpb:ess-start-of-output-marker))
                                  (forward-line 1)
                                  (point))))
         ;; end is never nil
         (end (save-excursion (goto-char (point-max))
                              (or (re-search-backward
                                   (concat "^" gpb:ess-end-of-output-marker)
                                   nil t)
                                  (point-max))))
         (inhibit-read-only t)
         selection)

    (if complete
        (with-current-buffer help-buf
          (erase-buffer)
          (insert-buffer-substring server-buf start end)
          (goto-char (point-min))
          ;; This is some kind of terminal code for underlining text.
          (while (search-forward "_" nil t)
            (backward-delete-char 2)
            (put-text-property (point) (1+ (point)) 'face 'underline))
          (goto-char (point-min))
          (help-mode)
          (read-only-mode 1)
          (pop-to-buffer help-buf))

      ;; If the output is not complete, we look for a selection prompt.  We
      ;; see this prompt when there are multiple matches.
      (save-excursion
        (goto-char (process-mark proc))
        (when (and start (looking-back "^Selection: *" nil t))
          (with-current-buffer help-buf
            (erase-buffer)
            (insert-buffer-substring server-buf start end)
            (pop-to-buffer help-buf)
            (setq selection (or (with-local-quit
                                  (prin1-to-string (read-number
                                                    "Selection: " 1)))
                                "0")))
          (insert "\n")
          (insert gpb:ess-start-of-output-marker)
          (insert "\n")
          (set-marker (process-mark proc) (point))
          (process-send-string proc (format "%s\n" selection)))))))


(defun gpb-ess:save-and-load-command (arg)
  "Save the current buffer and then source it or reload the package.

With a prefix argument, we source with chdir = TRUE or update the
Rmarkdown render expression."
  (interactive "P")
  (let* ((filename (buffer-file-name))
         (localname (ignore-errors (file-local-name filename)))
         (ess-proc (ess-get-process))
         (ess-proc-buf (process-buffer ess-proc))
         (ess-proc-dir (with-current-buffer ess-proc-buf default-directory))
         ;; Get the name of the directory that contains filename.
         (dir (ignore-errors (directory-file-name
                              (file-name-directory localname))))
         ;; Get the path relative to the interpreter's working directory
         (relpath (with-current-buffer ess-proc-buf
                    (file-relative-name filename)))
         cmd)
    (when filename (save-buffer))
    (cond
     ((string-suffix-p ".Rmd" localname t)
      (let ((build-cmd
             (or (and (null arg) (boundp 'gpb-ess:build-cmd) gpb-ess:build-cmd)
                 (read-string "Build command: "
                              (format "rmarkdown::render('%s')" relpath)))))
        (setq-local gpb-ess:build-cmd build-cmd)
        (with-current-buffer ess-proc-buf
          (comint-add-to-input-history build-cmd)
          (goto-char (point-max)))
        (display-buffer ess-proc-buf)
        (ess-send-string ess-proc build-cmd t)))

     ;; If we are in a package, reload the package.
     ((string-equal (ignore-errors (file-name-base dir)) "R")
      (gpb:ess-save-package)
      (setq cmd (format "pkgload::load_all('%s', export_all = FALSE)"
                        (directory-file-name (file-name-directory dir))))
      (display-buffer (process-buffer ess-proc))
      (with-current-buffer (process-buffer ess-proc)
        (comint-add-to-input-history cmd)
        (goto-char (point-max)))
      (ess-send-string ess-proc cmd t))

     ;; If we are in a test file, source the file but evaluate in the
     ;; current package namespace with the current directory as the working
     ;; directory.
     ((string-equal (ignore-errors (file-name-base dir)) "testthat")
      (let* ((cmd (format "cat(pkgload::pkg_name('%s'), fill = TRUE)\n"
                          (file-local-name (buffer-file-name))))
             (package-name (ess-string-command cmd nil 1)))
        (gpb:ess-eval-region (point-min) (point-max) package-name dir)))

     ;; If we have a prefix argument, source with chdir = TRUE.  This
     ;; requires us to save the file.
     ((not (null arg))
      (save-buffer)
      (let* ((filename (buffer-file-name))
             (proc-buf (gpb-ess:get-interpreter-buffer))
             (proc (get-buffer-process proc-buf))
             (working-dir (with-current-buffer proc-buf default-directory))
             (relative-name (file-relative-name filename working-dir))
             (cmd (format "source('%s', chdir = TRUE)" relative-name)))
        (message "working-dir: %s" working-dir)
        ;; See `gpb:ess-debug-track-comint-output-filter-function'
        (with-current-buffer proc-buf
          (setq-local default-directory2 (file-name-directory filename)))
        (ess-send-string proc cmd t)
        (display-buffer (process-buffer ess-proc))
        (with-current-buffer (process-buffer ess-proc)
          (comint-add-to-input-history cmd)
          (goto-char (point-max)))))

     ;; Otherwise, just evaluate the buffer contents in the global
     ;; environment.
     (t
      (when filename (save-buffer))
      (save-restriction
        (widen)
        (gpb:ess-eval-region (point-min) (point-max)))))))


(defun gpb:ess-view-data-frame ()
  (interactive)
  (let* ((r-expr (funcall comint-get-old-input))
         (tmp-file (gpb:ess-get-tmp-file "r-data-" nil ".xlsx"))
         (local-tmp-file (file-remote-p tmp-file 'localname))
         (cmd (format (concat "openxlsx::write.xlsx(%s, \"%s\"); "
                              "cat(sprintf(\"\nExcel File: %s\n\"))")
                      r-expr local-tmp-file tmp-file))
         (result-buf (gpb:ess-send-command cmd))
         file)
    (with-current-buffer result-buf
      (goto-char (point-min))
      (when (re-search-forward "Error in loadNamespace" nil t)
        (switch-to-buffer result-buf)
        (error "Error in R command"))
      (re-search-forward "Excel File: \\(.*\\)$")
      (setq file (or (file-local-copy tmp-file) file))
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

(defun gpb:ess-test-package ()
  (interactive)
  (let* ((procbuf (process-buffer (ess-get-process)))
         (localname (file-local-name (buffer-file-name)))
         (cmd1 (format "cat(pkgload::pkg_name('%s'), fill = TRUE)\n" localname))
         (package-name (ess-string-command cmd1 nil 0.2))
         (cmd2 (format "pkgload::load_all('%s', export_all = FALSE)" package-name))
         (cmd3 (format "testthat::test_package('%s', reporter = default_reporter())"
                       package-name)))

    (ess-switch-to-end-of-ESS)
    (with-current-buffer procbuf
      (setq-local gpb-ess:initial-space-output-filter-flag t))
    (ess-send-string (ess-get-process) cmd2 t)
    (ess-wait-for-process)
    (with-current-buffer procbuf
      (setq-local gpb-ess:initial-space-output-filter-flag t))
    (ess-send-string (ess-get-process) cmd3 t)))


(defun gpb:ess-set-active-interpreter (bufname)
  "The current buffer to be the active interpreter."
  (update-ess-process-name-list)
  (let* ((procbuf (get-buffer bufname))
         (procname (process-name (get-buffer-process procbuf)))
         (tramp-prefix (with-current-buffer procbuf
                         (file-remote-p default-directory)))
         ;; Find all the buffer visiting files on the same machine that the
         ;; interpreter is running.
         (buffer-list (remove-if-not
                       (lambda (buf) (with-current-buffer buf
                                       (and (string=
                                             (ignore-errors
                                               (file-remote-p default-directory))
                                             tramp-prefix)
                                            (not (null (buffer-file-name))))))
                       (buffer-list))))
    (dolist (buf buffer-list)
      (with-current-buffer buf
        (setq-local ess-local-process-name procname)))))


(defun gpb:ess-choose-interpreter ()
  (interactive)
  (update-ess-process-name-list)
  (when (null ess-process-name-list) (error "No R processes"))
  (let ((keymap (make-sparse-keymap))
        (indent "    ")
        (dir default-directory)
        (menu-buffer-name "*choose interpreter*"))
    (cl-flet ((make-button
               (lambda (name)
                 (insert-button
                  name 'action `(lambda (button)
                                  (gpb:ess-set-active-interpreter ,name)
                                  (display-buffer ,name)
                                  (kill-buffer ,menu-buffer-name))))))

      ;; (set-keymap-parent keymap view-mode-map)
      (define-key keymap "\t" 'forward-button)
      (define-key keymap [(backtab)] 'backward-button)
      (define-key keymap "q" 'View-quit)

      (with-current-buffer (get-buffer-create menu-buffer-name)
        (erase-buffer)
        (insert "Select an R process:\n\n")

        (dolist (name (sort (mapcar 'car ess-process-name-list) 'string<))
          (insert indent)
          (make-button (buffer-name (process-buffer (get-process name))))
          (insert "\n\n"))

        (use-local-map keymap)
        (beginning-of-buffer)
        (forward-button 1))
      (switch-to-buffer menu-buffer-name))))


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


(defun gpb-ess:initial-space-output-filter (text)
  "Remove an initial space at the beginning of some process output.

Somewhere in the complexity of the interaction between ESS and
comint we pick up extra initial spaces for some common commands.
This filter removes that newline."
  (let* ((checkmark-character (char-to-string 10004))
         (regex (format "^\n\\(Loading\\|%s\\)" checkmark-character)))
    ;; If we match, remove the first character.
    (if (string-match regex text) (substring text 1) text)))


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


(defun gpb:ess-get-tmp-file (prefix &optional dir-flag suffix where)
  (let* ((where (or where default-directory))
         (remote (file-remote-p default-directory))
         (tmpdir-override (ignore-errors
                            (file-name-as-directory
                             (cdr (assoc remote gpb:ess-tmpdir-map)))))
         (make-file (or (and (fboundp 'make-nearby-temp-file)
                             'make-nearby-temp-file)
                        'make-temp-file)))
    (funcall make-file (concat remote tmpdir-override prefix) dir-flag suffix)))


(defun gpb:ess-send-command (cmd &optional async buf)
  "Send CMD to R process in BUF.

If BUF is omitted, we use the current buffer.  If ASYNC is nil,
we make the call, wait for the result, and return a buffer that
contains the result.  When ASYNC is not nil, we operate
asynchronously and return nil immediately.  If ASYNC is a
function, we will call ASYNC from a buffer that contains the
output of the call as it arrives.  In this case, ASYNC should
accept a single boolean argument COMPLETE; it will be nil while
the command is running and t when is it done."
  (interactive "sR Command: ")
  (let* ((buf (or buf (gpb-ess:get-interpreter-buffer)))
         (proc (get-buffer-process buf))
         (buf-name (concat (buffer-name buf) "[server]"))
         (server-buf (get-buffer-create buf-name))
         (inhibit-read-only t)
         (full-cmd (format
                    "cat('%s\\n'); tryCatch({ %s }, finally = cat('%s\\n'))\n"
                    gpb:ess-start-of-output-marker
                    cmd
                    gpb:ess-end-of-output-marker)))

    (with-current-buffer server-buf
      (erase-buffer)
      (setq-local callback-func (and (not (eq async t)) async))
      (setq-local original-proc-buffer buf)
      (setq-local original-sentinel-func (process-sentinel proc))
      (setq-local original-filter-func (process-filter proc))
      (setq-local original-mark-pos (marker-position (process-mark proc)))
      (setq-local command-complete nil)

      (set-process-buffer proc server-buf)
      (set-process-sentinel proc nil)
      (set-process-filter proc 'gpb:ess-send-command--process-filter)
      (insert full-cmd)
      (set-marker (process-mark proc) (point))
      (send-string proc full-cmd)
      (unless async
        (while (null command-complete)
          (accept-process-output proc 1))
        (goto-char (point-min))
        (re-search-forward (format "^%s" gpb:ess-start-of-output-marker))
        (forward-line 1)
        server-buf))))


(defun gpb:ess-send-command--return-process ()
  (let* ((buf (current-buffer))
         (proc (get-buffer-process buf)))
    (set-process-buffer proc original-proc-buffer)
    (set-process-sentinel proc original-sentinel-func)
    (set-process-filter proc original-filter-func)
    (set-marker (process-mark proc)
                original-mark-pos original-proc-buffer)))


(defun gpb:ess-send-command--process-filter (proc string)
  (let ((buf (process-buffer proc)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (save-excursion
          (goto-char (process-mark proc))
          (insert string)
          (set-marker (process-mark proc) (point))
          (when (re-search-backward
                 (concat "^" gpb:ess-end-of-output-marker) nil t)
            ;; (forward-line 0)
            ;; (delete-region (point) (point-max))
            (gpb:ess-send-command--return-process)
            (setq-local command-complete t)))
        (when callback-func (funcall callback-func command-complete))))))


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

