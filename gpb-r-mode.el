;; Provides an alternative inferior R process mode that inherits from
;; `comint-mode' and a related minor mode for use in buffers that are
;; editting R code.

(require 'cl-lib)

(defgroup gpb-r-mode nil
  "Settings related to the Emacs 'gpb-r-mode' package/feature.")

(defcustom gpb-r-x-display
  (format "%s:0" (format-network-address
                  (car (network-interface-info "eth0")) t))
  "The DISPLAY environment variable to use on a server."
  :type 'string
  :group 'gpb-r-mode)

(defcustom gpb-r-inferior-compilation-error-regexp-alist
  `(;; Don't underline the " at " part of the traceback.
    ("\\(?: at \\|(@\\)\\(\\([^#()\n]+\\)[#:]\\([0-9]+\\)\\)"
     2 3 nil 2 1)

    ;; This matches Shiny tracebacks like:
    ;;   <reactive> [CrossAssetBeta/R/module_tradeTable.R#136]
    ("\\[?\\(\\([^[() \n]+\\)#\\([0-9]+\\)\\)" 2 3 nil 2 1)

    ;; This matches parse errors like:
    ;;   Package/R/file.R:465:3: unexpected symbol
    ;; (,(format "%s %s"
    ;;           "^ *\\(\\([^:\n]+\\):\\([0-9]+\\):\\([0-9]+:\\)?\\)"
    ;;           "\\(unexpected\\|failure\\|error\\)")
    ;;  2 3 4 2 1)

    ;; This matches parse errors like:
    ;;   Package/R/file.R:465:3: unexpected symbol
    ;; and testthat tracebacks like:
    ;;   5. base::stopifnot(...) Package/R/file.R:465:3
    ;; and documentation errors like:
    ;;   Warning in tools::parse_Rd(path, macros = macros) :
    ;;     .../man/file.Rd:50: unknown macro '\item'
    ("\\(\\([^:\n ]+[.][Rr]\\(m?d\\)?\\):\\([0-9]+\\):\\([0-9]+:\\)?\\)"
     2 3 4 2 1)

    ;; This matches R Markdown build failures like:
    ;;   Quitting from lines 257-329 (report.Rmd)
    ;; If we see
    ;;   Quitting from lines 257-329 (./report.Rmd)
    ;; We don't include the "./" prefix in the file match as it confuses
    ;; compilation mode.  Recall that rmarkdow::render changes the working
    ;; directory.
    (,(format "Quitting from lines %s %s"
              ;; Matches 257-329 part
              "\\(\\([0-9]+\\)-[0-9]+"
              ;; Matches filename, excluding any initial "./"
              "([.]?/?\\(.*\\))\\)")
     3 2 nil 1))
  "An alist of regular exprssions that match source code location output.

The value of `compilation-error-regexp-alist' is set to this
alist in an inferior R process buffer."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'gpb-ess)

(defvar gpb-r-start-of-output-marker
  "START:75b30f72-85a0-483c-98ce-d24414394ff0"
  "Arbitrary string used to denote the end of R output.")

(defvar gpb-r-end-of-output-marker
  "END:75b30f72-85a0-483c-98ce-d24414394ff0"
  "Arbitrary string used to denote the end of R output.")

(defvar gpb-r-active-process-buffer nil
  "The currently active R process.

At the moment, there can only be one active process")

(defvar gpb-inferior-r-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\t] 'gpb-r-tab-command)
    (define-key map "\C-c\C-c" 'gpb-r-set-active-process)
    ;; (define-key map "\r" 'gpb-r-send-or-copy-input)
    ;; (define-key map "\C-ct" 'gpb:ess-send-traceback-command)
    ;; (define-key map "\C-cq" 'gpb:ess-send-quit-command)
    ;; (define-key map [(backtab)] 'gpb:inferior-ess-previous-traceback)
    ;; (define-key map "\C-co" 'gpb:ess-view-data-frame)
    ;; (when (require 'gpb-text-objects nil t)
    ;;   (define-key map "g" 'gpb:ess-goto-line))
    ;; (define-key map "\C-c\C-v" 'gpb-ess:show-help)

    map)
  "Local keymap `gpb-inferior-r-mode'.")

(define-derived-mode gpb-inferior-r-mode comint-mode "Inferior R Mode"
  "Major mode for an inferior R process.
\\<gpb-inferior-r-mode-map>
\\{gpb-inferior-r-mode-map}"
  (setq truncate-lines t)
  (gpb-r-send-command (with-temp-buffer
                        (insert-file-contents (locate-library "gpb-r-mode.R"))
                        (buffer-string))
                      (current-buffer))
  ;; (setq-local comint-input-filter 'gpb:ess-comint-input-filter)
  (setq-local completion-at-point-functions '(gpb-r-completion-at-point))
  (dolist (face '(underline compilation-error compilation-line-number))
    (face-remap-add-relative face :foreground "royal blue" :weight 'normal))
  ;; (add-hook 'comint-output-filter-functions
  ;;           #'gpb:ess-debug-track-comint-output-filter-function nil t)
  )

(defvar gpb-r-code-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'gpb-r-save-and-exec-command)
    map))

(define-minor-mode gpb-r-code-mode
  "Minor mode for R code buffers."
  :keymap gpb-r-code-minor-mode-map)

(defun gpb-r-set-active-process ()
  (interactive)
  (setq gpb-r-active-process-buffer (current-buffer))
  (message "Active R process buffer: %S" gpb-r-active-process-buffer))

(defun gpb-r-tab-command ()
  (interactive)
  (setq this-command (if (comint-after-pmark-p)
                         'completion-at-point
                       'gpb:inferior-ess-next-traceback))
  (call-interactively this-command))

(defun gpb-r-completion-at-point ()
  (save-excursion
    (let* ((buf (current-buffer))
           (proc (get-buffer-process buf))
           (line (buffer-substring-no-properties (process-mark proc) (point)))
           (cmd (format ".gpb_r_mode_get_completions(%s, %S)"
                        (marker-position (process-mark proc)) line))
           (response (gpb-r-send-command cmd buf)))
      (read response))))

(defun gpb-r-send-or-copy-input ()
  "Send the current line or copy a previous line to the current line."
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

   ((comint-after-pmark-p)
    (comint-send-input))

   (t
    (let ((old-input (funcall comint-get-old-input)))
      (goto-char (point-max))
      (while (looking-back "\n")
        (backward-char))
      (insert old-input)))))

(defun gpb-r-send-command (cmd &optional buf)
  "Send CMD to R process in BUF.

If BUF is omitted, we use the current buffer.  We move the
process into new buffer, send `cmd' to the R process, wait for
the result, and return a buffer that contains the result."
  (interactive "sR Command: ")
  (let* ((buf (or buf gpb-r-active-process-buffer))
         (proc (get-buffer-process buf))
         (server-buf-name (concat (buffer-name buf) " [command buffer]"))
         (server-buf (get-buffer-create server-buf-name))
         (inhibit-read-only t)
         (full-cmd
          (format
           (concat "cat('\\n%s\\n'); "
                   "tryCatch({ %s }, finally = cat('\\n%s\\n'))\n")
           gpb-r-start-of-output-marker
           cmd
           gpb-r-end-of-output-marker)))

    (accept-process-output proc 0.1 nil 1)

    (with-current-buffer server-buf
      (erase-buffer)
      (setq-local original-proc-buffer buf)
      (setq-local original-sentinel-func (process-sentinel proc))
      (setq-local original-filter-func (process-filter proc))
      (setq-local original-mark-pos (marker-position (process-mark proc)))

      (set-process-buffer proc server-buf)
      (set-process-sentinel proc nil)

      (insert full-cmd)
      (set-marker (process-mark proc) (point) server-buf)

      (set-process-filter proc nil)
      (send-string proc full-cmd)
      (condition-case
          error-var
          (unwind-protect
              (with-temp-message "Waiting on R process..."
                ;; Accept output until we see the end-of-output marker
                (while (not (save-excursion
                              (goto-char (point-max))
                              (re-search-backward
                               (concat "^" gpb-r-end-of-output-marker "\n> ")
                               nil t)))
                  (accept-process-output proc 1))
                (goto-char (point-min))
                (re-search-forward (format "^%s" gpb-r-start-of-output-marker))
                (forward-line 1)
                server-buf)

            ;; Return the process to its original buffer even if there is an
            ;; error.
            (set-process-buffer proc original-proc-buffer)
            (set-process-sentinel proc original-sentinel-func)
            (set-process-filter proc original-filter-func)
            (set-marker (process-mark proc)
                        original-mark-pos original-proc-buffer))

        ((error quit)
         (pop-to-buffer server-buf)
         (error "`gpb-r-send-command' failed"))))))


(defun gpb-r-save-and-exec-command (arg)
  "Save the current buffer and then source, reload, or render it.

With a prefix argument, we source with chdir = TRUE or update the
Rmarkdown render expression."
  (interactive "P")
  (let* ((filename (or (buffer-file-name)
                       (error "Buffer is not visiting a file")))
         (localname (ignore-errors (file-local-name filename)))
         (r-proc-buf gpb-r-active-process-buffer)
         (r-proc (get-buffer-process r-proc-buf))
         (r-proc-dir (with-current-buffer r-proc-buf default-directory))
         ;; Get the name of the directory that contains filename.
         (dir (ignore-errors (directory-file-name
                              (file-name-directory localname))))
         ;; Get the path relative to the interpreter's working directory
         (relpath (with-current-buffer r-proc-buf
                    (file-relative-name filename)))
         cmd)

    (save-buffer)

    (cond
     ;; If this command does not have an argument and the buffer local
     ;; variable `gpb-r-exec-cmd' is set from a previous call, use that.
     ((and (null arg) (boundp 'gpb-r-exec-cmd))
      (setq cmd gpb-r-exec-cmd))

     ;; We have an R markdown document, render it.
     ((string-suffix-p ".Rmd" localname t)
      (setq cmd (read-string "Render command: "
                             (format "rmarkdown::render('%s')" relpath))))

     ;; If we are in a package, save any modified source files in the
     ;; package and reload the package.
     ((string-equal (ignore-errors (file-name-base dir)) "R")
      (gpb-r-mode-save-package)
      (let* ((pkg-dir (ignore-errors (directory-file-name
                                      (file-name-directory dir))))
             (pkg-relpath (with-current-buffer r-proc-buf
                            (file-relative-name pkg-dir))))
        (setq cmd (read-string "Load command: "
                               (format "pkgload::load_all('%s')"
                                       pkg-relpath)))))

     ;; If we are in a test file, source the file but evaluate in the
     ;; current package namespace with the current directory as the working
     ;; directory.
     ;; ((string-equal (ignore-errors (file-name-base dir)) "testthat")
     ;;  (let* ((cmd (format "cat(pkgload::pkg_name('%s'), fill = TRUE)\n"
     ;;                      (file-local-name (buffer-file-name))))
     ;;         (package-name (ess-string-command cmd nil 1)))
     ;;    (gpb:ess-eval-region (point-min) (point-max) package-name dir)))

     ;; Otherwise, source the file.
     (t
      (setq cmd (read-string "Load command: "
                             (format "source('%s')" relpath)))))

    ;; Save the command in the buffer so you can easily reuse it.
    (setq-local gpb-r-exec-cmd cmd)

    (with-current-buffer r-proc-buf
      (goto-char (point-max))
      (comint-delete-input)
      (insert cmd)
      (comint-send-input))

    (display-buffer r-proc-buf)))


(defun gpb-r-mode-save-package ()
  "Save all files in the current package that have been edited."
  (interactive)
  (let* ((filename (buffer-file-name))
         (dir (ignore-errors (directory-file-name
                              (file-name-directory filename))))
         (is-pkg-buf-p (lambda (buf)
                         (ignore-errors
                           (let ((fn (buffer-file-name buf)))
                             (and (string-prefix-p dir fn)
                                  (string-suffix-p ".R" fn t))))))
         src-files)

    (unless (string-equal (ignore-errors (file-name-base dir)) "R")
      (error "Buffer is not visiting a package source file"))

    (setq src-files (cl-remove-if-not is-pkg-buf-p (buffer-list)))
    (message "Package buffers: %S" src-files)

    (dolist (buf src-files)
      (when (buffer-modified-p buf)
        (with-current-buffer buf (save-buffer))))))

(provide 'gpb-r-mode)
