;; Provides an alternative inferior R process mode that inherits from
;; `comint-mode' and a related minor mode for use in buffers that are
;; editting R code.

(require 'cl-lib)
(require 'subr-x)

(defgroup gpb-r-mode nil
  "Settings related to the Emacs 'gpb-r-mode' package/feature.")

(defcustom gpb-r-x-display
  (format "%s:0" (format-network-address
                  (car (network-interface-info "eth0")) t))
  "The DISPLAY environment variable to use on a server."
  :type 'string
  :group 'gpb-r-mode)

(defcustom gpb-r-process-header-regex
  "^R [vV]ersion [1-9][.][1-9]+[.][1-9]+"
  "The regex used to recognize the start of an R process in a shell buffer."
  :type 'string
  :group 'gpb-r-mode)

(defcustom gpb-r-history-save-frequency (* 5 60)
  "The frequency with which we save the history of the R process in seconds.

Set to nil or zero to disable auto-saving history."
  :type 'integer
  :group 'gpb-r-mode)

(defcustom gpb-r-file-link-definitions
  `(;; Don't underline the " at " part of the traceback.
    ("\\(?: at \\|(@\\)\\(\\([^#():\n]+\\)[#:]\\([0-9]+\\)\\)" 2 3 1)

    ;; This matches Shiny tracebacks like:
    ;;   <reactive> [CrossAssetBeta/R/module_tradeTable.R#136]
    ("\\[?\\(\\([^[() \n]+\\)#\\([0-9]+\\)\\)\\]" 2 3 1)

    ;; This matches parse errors like:
    ;;   Package/R/file.R:465:3: unexpected symbol
    ;; and testthat tracebacks like:
    ;;   5. base::stopifnot(...) Package/R/file.R:465:3
    ;; and documentation errors like:
    ;;   Warning in tools::parse_Rd(path, macros = macros) :
    ;;     .../man/file.Rd:50: unknown macro '\item'
    ("\\(\\([^:\n ]+[.][Rr]\\(m?d\\)?\\):\\([0-9]+\\):\\)"
     2 4 1)

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
     3 2 1))
  "An alist of regular expressions that match source code location output.

A simplified version of `compilation-error-regexp-alist'.  Each
entry has the form (REGEXP FILE LINE HYPERLINK) where FILE, LINE,
and HYPERLINK are integers giving match indices in REGEXP."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'gpb-r-mode)

(defcustom gpb-r-preinput-filter-functions
  '(gpb-r--region-eval-preinput-filter)
  "A list of functions that modify R process input.

These functions are called after the input is added to the input
ring and immediately before the input is sent to the inferior R
process.  The current use case is to interpret 'magic' commands
that are imbedded in R comments that require coordination with
Emacs.")

(defvar gpb-r-end-of-output-marker
  "END:75b30f72-85a0-483c-98ce-d24414394ff0"
  "Arbitrary string used to denote the end of R output.")

(defvar gpb-r-active-process-buffer nil
  "The currently active R process.

At the moment, there can only be one active process")

(defvar gpb-inferior-r-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\t] 'gpb-r-tab-command)
    (define-key map [(backtab)] 'gpb-r-backward-button)
    (define-key map "\C-c\C-c" 'gpb-r-set-active-process)
    (define-key map "\C-c\C-t" 'gpb-r-send-traceback)
    ;; (define-key map "\C-co" 'gpb:ess-view-data-frame)
    ;; (define-key map "\C-c\C-v" 'gpb-ess:show-help)

    map)
  "Local keymap `gpb-inferior-r-mode'.")

(defvar gpb-inferior-r-mode--syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?. "_" st)
    st))

(define-derived-mode gpb-inferior-r-mode comint-mode "Inferior R Mode"
  "Major mode for an inferior R process.
\\<gpb-inferior-r-mode-map>
\\{gpb-inferior-r-mode-map}"
  (setq truncate-lines t)

  ;; History setup
  ;;
  ;; Use an absolute path for `comint-input-ring-file-name' so it is not
  ;; impacted by changes to the R working.
  (setq-local comint-input-ring-file-name (expand-file-name ".Rhistory"))
  (setq-local comint-input-ring-separator "\n")
  (setq-local comint-input-filter #'gpb-r--comint-input-filter)

  ;; Keep comments in the history as we use comments for some special
  ;; commands like eval region that are actually handled by input filter
  ;; functions.
  (setq-local comint-input-history-ignore "^$")
  (comint-read-input-ring t)
  (gpb-r--maybe-save-history-later)

  ;; Try to shutdown gracefully when the buffer is killed.
  (add-hook 'kill-buffer-hook #'gpb-r--kill-buffer-hook nil t)

  (gpb-r-send-command (with-temp-buffer
                        (insert-file-contents (locate-library "gpb-r-mode.R"))
                        (buffer-string))
                      (current-buffer))
  (setq-local completion-at-point-functions '(gpb-r-completion-at-point))
  (setq-local comint-input-autoexpand nil)
  (setq-local comint-input-sender #'gpb-r--input-sender)

  (set-syntax-table gpb-inferior-r-mode--syntax-table)


  ;; Append `gpb-r--busy-state-input-filter' so it runs after and escape
  ;; sequences have been processed.
  (add-hook 'comint-input-filter-functions
            #'gpb-r--busy-state-input-filter t t)

  (add-hook 'comint-output-filter-functions
            #'gpb-r-mode-debug-filter-function nil t)
  (add-hook 'comint-output-filter-functions
            #'gpb-r--add-links-filter-function nil t)
  (add-hook 'comint-output-filter-functions
            #'gpb-r--busy-state-output-filter nil t)


  ;; We add the callback filter last as it may insert text in the buffer
  ;; that we want the other filter functions to pick up (e.g. file link
  ;; text).
  (add-hook 'comint-output-filter-functions
            #'gpb-r--callback-filter-function nil t)

  ;; eldoc support
  (setq-local eldoc-documentation-function
              #'gpb-r--eldoc-documentation-function)
  (eldoc-mode 1))


(defvar gpb-r-code-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'gpb-r-save-and-exec-command)
    map))

(define-minor-mode gpb-r-code-mode
  "Minor mode for R code buffers."
  :keymap gpb-r-code-minor-mode-map
  :lighter "gpb-r"
  (setq-local eldoc-documentation-function
              #'gpb-r--eldoc-documentation-function)
  (eldoc-mode 1))

(defun gpb-r-set-active-process ()
  (interactive)
  (setq gpb-r-active-process-buffer (current-buffer))
  (message "Active R process buffer: %S" gpb-r-active-process-buffer))

(defun gpb-r-tab-command ()
  (interactive)
  (setq this-command (if (comint-after-pmark-p)
                         'completion-at-point
                       'gpb-r-forward-button))
  (call-interactively this-command))

(defun gpb-r-forward-button ()
  (interactive)
  (condition-case exc
      (forward-button 1 nil t)
    (error (goto-char (point-max)))))

(defun gpb-r-backward-button ()
  (interactive)
  (forward-button -1 nil t))

(defun gpb-r-completion-at-point ()
  (save-excursion
    (let* ((buf (current-buffer))
           (proc (get-buffer-process buf))
           (line (buffer-substring-no-properties (process-mark proc) (point)))
           (cmd (format ".gpb_r_mode$get_completions(%s, %S)"
                        (marker-position (process-mark proc)) line))
           (response (gpb-r-send-command cmd buf)))
      (read response))))

(defun gpb-r-send-command (cmd &optional buf)
  "Send CMD to R process in BUF and the return the output as a string.

If BUF is omitted, we use the current buffer.  We move the
process into new buffer, send `cmd' to the R process, wait for
the result, and return a buffer that contains the result."
  (interactive "sR Command: ")
  (let* ((buf (gpb-r-get-proc-buffer buf))
         (wrapped-cmd (format "tryCatch({ %s }, finally = cat('\\n%s\\n'))\n"
                              cmd gpb-r-end-of-output-marker))
         (proc (or (get-buffer-process buf)
                   (error "No R process available")))
         (server-buf-name (concat (buffer-name buf) " [commands]"))
         (server-buf (get-buffer-create server-buf-name))
         (inhibit-read-only t)
         start end)

    ;; Try to pull any pending output from the proces before we send
    ;; `wrapped-cmd'.
    (while (accept-process-output proc 0.1 nil 1))

    (with-current-buffer server-buf
      (erase-buffer)
      (setq-local original-proc-buffer buf)
      (setq-local original-sentinel-func (process-sentinel proc))
      (setq-local original-filter-func (process-filter proc))
      (setq-local original-mark-pos (marker-position (process-mark proc)))

      (set-process-buffer proc server-buf)
      (set-process-sentinel proc nil)
      (set-process-filter proc nil)

      (insert wrapped-cmd)
      (set-marker (process-mark proc) (point) server-buf)
      (setq start (point))
      (send-string proc wrapped-cmd)

      (condition-case
          error-var
          (unwind-protect
              (with-temp-message "Waiting on R process..."
                ;; Accept output until we see the end-of-output marker
                (while (not (save-excursion
                              (goto-char start)
                              (re-search-forward
                               (concat "^" gpb-r-end-of-output-marker
                                       "\n\\(Browse.*\\)?>")
                               nil t)))
                  ;; If we are trying to run a command when the prompt is
                  ;; in a Browser state, we may need to send a few "n" to
                  ;; complete the command.
                  (when (save-excursion (goto-char start)
                                        (forward-line 0)
                                        (re-search-forward
                                         "Browse\\[[0-9]+\\]> " nil t))
                    (end-of-line)
                    (insert "n\n")
                    (move-marker (process-mark proc) (point))
                    (setq start (point))
                    (send-string proc "n\n"))
                  (accept-process-output proc 0.5 nil t)
                  (ansi-color-filter-region (point-min) (point-max)))
                (goto-char (match-beginning 0))
                ;; We use a marker for `end` so that it is not disturbed by
                ;; the deletions below.
                (setq end (copy-marker (point)))
                (goto-char start)
                ;; Remove any continuation prompts between `start' and
                ;; `and' in the buffer
                (save-excursion
                  (while (re-search-forward "^+ " end t)
                    (delete-region (match-beginning 0) (match-end 0))))
                (string-trim (buffer-substring-no-properties start end)))

            (let ((inhibit-message t)) (message "Waiting on R process...done"))

            ;; Return the process to its original buffer even if there is an
            ;; error.
            (set-process-buffer proc original-proc-buffer)
            (set-process-sentinel proc original-sentinel-func)
            (set-process-filter proc original-filter-func)
            (set-marker (process-mark proc)
                        original-mark-pos original-proc-buffer))

        ;; If there is an error, of the call hangs and the user has to
        ;; `keyboard-quit' to get control of Emacs, we show the server
        ;; buffer and raise an error.
        ((error quit)
         (pop-to-buffer server-buf)
         (error "`gpb-r-send-command' failed"))))))


(defun gpb-r-getwd (&optional buf)
  "Get the TRAMP-qualified working directory of the current R process.

Also updates `default-directory' in the process buffer.  This
function is safe to call when the R process in the browser
debugging state."
  (interactive)
  (let* ((buf (gpb-r-get-proc-buffer buf))
         (local-wd (gpb-r-send-command "cat(sprintf('%s\\n', getwd()))" buf)))
    (with-current-buffer buf
      (setq default-directory (concat (gpb-r--get-tramp-prefix)
                                      (file-name-as-directory local-wd)))
      default-directory)))


(defun gpb-r-save-and-exec-command (arg)
  "Save the current buffer and then source, reload, or render it.

With a prefix argument, we source with chdir = TRUE or update the
Rmarkdown render expression."
  (interactive "P")
  (let* ((filename (or (buffer-file-name)
                       (error "Buffer is not visiting a file")))
         (localname (file-local-name filename))
         (r-proc-buf (or (gpb-r-get-proc-buffer)
                         (error "No R process is associated with buffer")))
         ;; Get the name of the directory that contains filename.
         (dir (ignore-errors (directory-file-name
                              (file-name-directory localname))))
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
                             (format "rmarkdown::render('%s')" localname))))

     ;; If we are in a package, save any modified source files in the
     ;; package and reload the package.
     ((string-equal (ignore-errors (file-name-base dir)) "R")
      (gpb-r-mode-save-package)
      (let* ((pkg-dir (ignore-errors (directory-file-name
                                      (file-name-directory dir)))))
        (setq cmd (read-string "Load command: "
                               (format "pkgload::load_all('%s')"
                                       (file-local-name pkg-dir))))))

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
                             (format "source('%s')" localname)))))


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


(defun gpb-r-mode-debug-filter-function (output)
  "Filter function that tracks debug output"
  ;; This function is called with empty output at odd times that can lead
  ;; to misleading highlighting if you don't ignore it those calls.
  (when (> (string-width output) 0)
    (save-match-data
      (save-excursion
        (goto-char comint-last-output-start)
        (when (re-search-forward "^debug at \\([^#:]+\\)[#:]\\([0-9]+\\):" nil t)
          (gpb-log-forms 'gpb-r-mode-debug-filter-function
                         'comint-last-output-start
                         '(match-beginning 0))
          ;; We grab the match substrings immediately as some of the later
          ;; functions might reset the match data.
          (let* ((filename (match-string-no-properties 1))
                 (line-number (string-to-number (match-string-no-properties 2)))
                 (buf (gpb-r--find-buffer filename)))
            (gpb-r-show-line buf line-number)))))))


(defvar gpb-r-show-line--overlay nil
  "Current highlighting overlay used in `gpb-r-show-line")


(defun gpb-r-show-line (buf line-number &optional pop-to face)
  "Show line `line' in `buf' in some other window.

If `pop-to' is non-nil, switch to the buffer in which the line is
displayed."
  (when (and buf (buffer-live-p buf))
    (let* ((window (display-buffer buf 'other-window))
           (vertical-margin (and window (/ (window-height window) 4)))
           (face (or face 'next-error)))
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
        (when (overlayp gpb-r-show-line--overlay)
          (delete-overlay gpb-r-show-line--overlay))
        (setq gpb-r-show-line--overlay (make-overlay (save-excursion
                                                       (beginning-of-line)
                                                       (point))
                                                     (save-excursion
                                                       (end-of-line)
                                                       (when (looking-at-p "\n")
                                                         (forward-char 1))
                                                       (point))))
        (overlay-put gpb-r-show-line--overlay 'face face)
        (overlay-put gpb-r-show-line--overlay 'window window)
        (run-at-time 0.25 nil (lambda ()
                                (delete-overlay gpb-r-show-line--overlay)))))
    (when pop-to (select-window window))))


(defun gpb-r-get-proc-buffer (&optional buf)
  "Get the currently active R process buffer"
  (or buf
      (let ((proc (get-buffer-process (current-buffer))))
        (and (process-live-p proc) (current-buffer)))
      (let ((proc (get-buffer-process gpb-r-active-process-buffer)))
        (and (process-live-p proc) gpb-r-active-process-buffer))))


(defun gpb-r--notice-r-process-start (str)
  "Switch to `gpb-inferior-r-mode' when we notice an R process starting."
  (let ((start (if (and (markerp comint-last-output-start)
                        (eq (marker-buffer comint-last-output-start)
                            (current-buffer))
                        (marker-position comint-last-output-start))
                   comint-last-output-start
                 (point-min)))
        (end (process-mark (get-buffer-process (current-buffer))))
        ;; We need to save buffer local state between calls.
        (state (and (boundp 'gpb-r--notice-r-process-start--state)
                    gpb-r--notice-r-process-start--state)))
    (gpb-log-forms 'gpb-r--notice-r-process-start 'str 'start 'end 'state)
    (save-excursion
      (save-match-data
        (goto-char start)
        (cond
         ;; Waiting for the R process to complete its initial output.
         ((eq state 'waiting)
          (when (re-search-forward "^> " end t)
            ;; We don't want to change modes in the middle of a call to
            ;; `comint-output-filter'; this seems to cause all kinds of
            ;; problems, so wait a bit and the do the change.
            (remove-hook 'comint-output-filter-functions
                         'gpb-r--notice-r-process-start t)
            (run-at-time 0.5 nil 'gpb-r--notice-r-process-start--switch-mode
                         (current-buffer))))
         (t
          (when (re-search-forward gpb-r-process-header-regex end t)
            (setq-local gpb-r--notice-r-process-start--state 'waiting))))))))


(defun gpb-r--notice-r-process-start--switch-mode (buf)
  "Implementation detail of `gpb-r--notice-r-process-start'"
  (gpb-log-forms 'gpb-r--notice-r-process-start--switch-mode
                 'buf)
  (with-current-buffer buf
    (remove-hook 'post-command-hook
                 'gpb-r--notice-r-process-start--switch-mode t)
    (gpb-inferior-r-mode)
    (delete-region (save-excursion (forward-line 0) (point)) (point))
    (comint-send-string (get-buffer-process (current-buffer)) "\n")))


(defun gpb-r-watch-for-r-process ()
  "Configure a buffer to automatically switch to `gpb-inferior-r-mode'.

If you add the following to your .emacs:

(add-hook 'shell-mode-hook #'gpb-r-watch-for-r-process)

then shell buffers will automatically switch to `gpb-inferior-r-mode'
when they see an R process start.  This used the standard R
header output so it won't work if you pass the R process the
--quiet flag."
  (interactive)
  (add-hook 'comint-output-filter-functions
            'gpb-r--notice-r-process-start nil t))


(defun gpb-r--add-links (beg end)
  "Add buttons that allow one to jump to souce code locations."
  (interactive "r")
  (dolist (link-def gpb-r-file-link-definitions)
    (let ((regex (car link-def))
          (file-subexp (nth 1 link-def))
          (line-subexp (nth 2 link-def))
          (link-subexp (nth 3 link-def)))
      (save-excursion
        (save-match-data
          (goto-char beg)
          (while (re-search-forward regex end t)
            (let ((file (buffer-substring-no-properties
                         (match-beginning file-subexp) (match-end file-subexp)))
                  (line (ignore-errors
                          (string-to-number
                           (buffer-substring-no-properties
                            (match-beginning line-subexp)
                            (match-end line-subexp))))))
              (make-text-button
               (match-beginning link-subexp) (match-end link-subexp)
               'file file
               'line line
               'action #'gpb-r--visit-link
               ;; Abbreviate the name in the buffer, but show the full path
               ;; in the echo area when you tab to the button.
               'display (format "%s#%s" (file-name-nondirectory file) line)
               'help-echo (format "%s#%s" file line)))))))))


(defun gpb-r--visit-link (button)
  "Visit the line recorded in `button'"
  (let* ((file (concat (gpb-r--get-tramp-prefix)
                       (button-get button 'file)))
         (line (button-get button 'line)))
    (setq buf (gpb-r--find-buffer file t))
    (gpb-r-show-line buf line)))


(defvar-local gpb-r--add-links-marker nil
  "We have already looked for source links prior to this marker")

(defun gpb-r--add-links-filter-function (output)
  (gpb-log-forms 'gpb-r--add-links-filter-function
                 'comint-last-output-start
                 'output)
  (when (> (string-width output) 0)
    (let* ((proc (get-buffer-process (current-buffer)))
           (start (or gpb-r--add-links-marker (point-min)))
           (end (save-excursion (goto-char (process-mark proc))
                                (forward-line 0)
                                (point))))
      (gpb-r--add-links start end)
      (setq-local gpb-r--add-links-marker (copy-marker end)))))


(defun gpb-r--callback-filter-function (output)
  "Looks for specific callback requests for the R code.

All callback requests look like gpb-r-callback: [lisp sexp]
followed by `gpb-r-end-of-output-marker'.  the sexp is evaluated
in the process buffer with point set to the start of the line
that contains the callback."
  (when (> (string-width output) 0)
    (let ((proc (get-buffer-process (current-buffer)))
          (marker (or (and (boundp 'gpb-r--callback-filter-function--marker)
                           (markerp gpb-r--callback-filter-function--marker)
                           gpb-r--callback-filter-function--marker)
                      (copy-marker (point-min))))
          (regex (format "^gpb-r-callback: \\(.*\\) %s\n"
                         gpb-r-end-of-output-marker))
          sexp)

      (when comint-last-output-start
        (set-marker marker (max comint-last-output-start marker)))

      (gpb-log-forms 'gpb-r--callback-filter-function
                     'comint-last-output-start
                     'output 'marker 'regex)

      (save-excursion
        (goto-char marker)
        (while (re-search-forward regex nil t)
          (setq sexp (read (match-string-no-properties 1)))
          (gpb-log-forms 'gpb-r--callback-filter-function 'sexp)
          (save-excursion
            (goto-char (match-beginning 0))
            (delete-region (match-beginning 0) (match-end 0))
            (eval sexp)))
        (goto-char (process-mark proc))
        (forward-line 0)
        (move-marker marker (point)))

      (setq-local gpb-r--callback-filter-function--marker marker))))


;; Callback functions.  See gpb-r-mode.R.

(defun gpb-r--add-error-info (file line)
  (save-excursion
    (skip-chars-backward " \n")
    (insert (format " at %s#%s" file line))))


;; eldoc integration

(defvar-local gpb-r--eldoc-hash
  (make-hash-table :test #'equal))

(defun gpb-r-clear-eldoc-cache ()
  "Clear the eldoc documentation cache.

If the R function definitions have changed, call this function to
clear the cache."
  (interactive)
  (with-current-buffer (gpb-r-get-proc-buffer)
    (clrhash gpb-r--eldoc-hash)))

(defun gpb-r--eldoc-documentation-function ()
  "Identify the current function and return argument info.

This function only recognizes a function once you enter the
parenthesis for its arguments.  We don't try to highlight the
current argument.  Handling this correctly in the presence of
named arguments would be complicated and it's just not worth it.

This function returns a string or nil"
  ;; We bound how far we search to look for an opening parenthesis.  In an
  ;; inferion process buffer, you want to ignore any previous input or
  ;; error output before the current prompt.  In an R code buffer, you want
  ;; to avoid invalid code that is far away from the point.
  (save-match-data
    (let* ((lbound (if (derived-mode-p 'comint-mode)
                       (comint-line-beginning-position)
                     (save-excursion (forward-line -25) (point))))
           (parse-info (parse-partial-sexp lbound (point)))
           end func-name)

      ;; Only proceed if we are inside a parenthetical expression that starts
      ;; on or after `lbound'.  A half-open expression counts.
      (when (> (car parse-info) 0)

        ;; Find the function name immediately before the openning
        ;; parenthesis, including any module prefix.
        (setq func-name (save-excursion
                          ;; Go to opening parenthesis
                          (goto-char (cadr parse-info))
                          (skip-chars-backward " \t")
                          (setq end (point))
                          (skip-chars-backward "a-zA-Z0-9_.")
                          (when (looking-back "::")
                            (backward-char 2)
                            (skip-chars-backward "a-zA-Z0-9_."))
                          (buffer-substring-no-properties (point) end)))

        (when (and func-name
                   (not (member func-name '("if" "for" "function")))
                   (gpb-r-get-proc-buffer))
          (with-current-buffer (gpb-r-get-proc-buffer)
            ;; We associate a single buffer local cache with each process.
            (setq info (or (gethash func-name gpb-r--eldoc-hash)
                           ;; If the inferior process is busy, we don't try
                           ;; to get eldoc info now.
                           (and (not gpb-r--inferior-process-busy)
                                (puthash func-name
                                         (gpb-r-send-command
                                          (format ".gpb_r_mode$get_args(%S)"
                                                  func-name)
                                          (current-buffer))
                                         gpb-r--eldoc-hash))))
            (if (string= info "NULL")
                nil
              info)))))))


(defun gpb-r-update-tags (&optional dir)
  "Update TAGS in dir or one of its parent dirctories.

Looks for the TAGS_DIR file and then calls underyling R code."
  (interactive)
  (let ((dir (file-name-as-directory (or dir default-directory)))
        next file)
    (while (and dir (not (file-exists-p (concat dir "TAG_DIRS"))))
      (setq next (file-name-directory (directory-file-name dir))
            dir (if (string= next dir) nil next)))
    (setq file (concat dir "TAG_DIRS"))
    (when (file-exists-p file)
      (gpb-r-send-command (format ".gpb_r_mode$update_tags(%S)" file)))))


(defun gpb-r--get-tramp-prefix (&optional dir)
  (let ((dir (or dir default-directory)))
    (or (file-remote-p dir) "")))


(defun gpb-r-eval-region (beg end)
  (interactive "r")
  (let* ((srcbuf (buffer-name))
         (line1 (line-number-at-pos beg t))
         (line2 (save-excursion
                  (goto-char end)
                  (when (bolp) (forward-line -1))
                  (line-number-at-pos (point) t)))
         (text (buffer-substring-no-properties beg end))
         (cmd (if (= line1 line2)
                  ;; If the region is a single line, we just insert it directly.
                  (string-trim text)
                (format "# eval region: %S %s-%s" srcbuf line1 line2))))

    (gpb-r-send-input cmd t)))

(defun gpb-r-eval-text-object (obj start end)
  (gpb-r-eval-region start end))


(defvar-local gpb-r--region-file nil
  "Holds the region file in an inferior R process buffer")

(defun gpb-r--get-region-file ()
  "Should be called from an R process buffer"

  (let ((file gpb-r--region-file))
    (when (null file)
      (setq file (concat (gpb-r--get-tramp-prefix)
                         (gpb-r-send-command
                          "cat(sprintf('%s\n', .gpb_r_mode$region_file))")))
      (setq-local gpb-r--region-file file))
    file))

(defun gpb-r--region-eval-preinput-filter (line)
  "Input filter that looks for eval region commands"
  (when (string-match "# *eval region: +\\(\".*\"\\) +\\([0-9]+\\)-\\([0-9]+\\)$"
                      line)
    (let ((srcbuf (read (match-string 1 line)))
          (line1 (string-to-number (match-string 2 line)))
          (line2 (string-to-number (match-string 3 line)))
          (procbuf (current-buffer))
          (region-filename (gpb-r--get-region-file))
          text)

      (with-current-buffer srcbuf
        (save-restriction
          (widen)
          (setq text (buffer-substring-no-properties
                      (save-excursion
                        (goto-char (point-min))
                        (forward-line (1- line1))
                        (point))
                      (save-excursion
                        (goto-char (point-min))
                        (forward-line (1- line2))
                        (end-of-line)
                        (point))))

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
            (insert (make-string (1- line1) ?\n))
            (goto-char (point-max))
            (insert "\n\n"))

          ;; Only write tothe message buffer to avoid flicker
          (let ((inhibit-message t)) (message "Wrote %s" region-filename))

          (setq line (format ".gpb_r_mode$eval_region_file(%S)" srcbuf))))))
  line)


;; Track the busy state of the inferior process

(defvar-local gpb-r--inferior-process-busy nil
  "Is the inferrior process currently running.

We track this so we know when we can send commands (e.g. eldoc
info).  The mark the process buffer as busy after each input
until we see the next prompt.")

(defun gpb-r--busy-state-input-filter (line)
  (setq-local gpb-r--inferior-process-busy t))

(defun gpb-r--busy-state-output-filter (line)
  (save-restriction
    (widen)
    (when (save-excursion
            (goto-char (point-max))
            (forward-line 0)
            (looking-at-p "^> *$"))
      (setq-local gpb-r--inferior-process-busy nil))))


(provide 'gpb-r-mode)


;; History file management

(defvar-local gpb-r--history-file nil
  "The file that store the inferior R history.

This value is set in `gpb-inferior-r-mode' based on the
`default-directory' when the interpreter is started.")

(defun gpb-r--maybe-save-history-later ()
  "Save history later when `gpb-r-history-save-frequency' is set"
  (when (and gpb-r-history-save-frequency
             (> gpb-r-history-save-frequency 0))
    (run-at-time gpb-r-history-save-frequency nil
                 'gpb-r--maybe-save-history-later-1 (current-buffer))))

(defun gpb-r--maybe-save-history-later-1 (buf)
  "Save the history ring in `buf'"
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (comint-write-input-ring)
      (gpb-r--maybe-save-history-later))))


;; Filter comint ring history

(defun gpb-r--comint-input-filter (input)
  "Returns t if  `input' should be included in history."
  (and (comint-nonblank-p input)
       ;; Don't recordd browser() commands.
       (not (string-match "Q *" input))
       (not (string-match "q(.*) *" input))))


;; Shut down gracefully

(defun gpb-r--close-inferior-process (&optional buf)
  "Attempt to close inferior process gracefully."
  (let* ((buf (or buf (current-buffer)))
         (proc (get-buffer-process buf)))
    (when (and (buffer-live-p buf) (process-live-p proc))
      (with-current-buffer buf
        (save-excursion
          (goto-char (process-mark proc))
          (forward-line 0)
          (when (looking-at-p "^Browse\\[[0-9]+\\]> *")
            (send-string proc "Q\n"))
          (send-string proc "q(save = 'no')\n")
          (accept-process-output proc 0.1 nil t))))))

(defun gpb-r--kill-buffer-hook ()
  (comint-write-input-ring)
  (gpb-r--close-inferior-process))


;; Input preprocessing

(defun gpb-r--input-sender (proc input)
  "This function is bound to `comint-input-sender'.

Apply each function in `gpb-r-preinput-filter-functions' and then
send the resulting string to `comint-simple-send'."
  (dolist (f gpb-r-preinput-filter-functions)
    (setq input (funcall f input))
  (comint-simple-send proc input)))


;; Shortcuts for commands

(defun gpb-r-send-input (cmd &optional pop buf)
  "Insert `cmd' into R process buffer and send."
  (let* ((procbuf (gpb-r-get-proc-buffer buf)))

    (or (process-live-p (get-buffer-process procbuf))
        (error "No R process available"))

    (with-current-buffer procbuf
      (save-excursion
        (comint-goto-process-mark)
        (insert (string-trim cmd))
        (comint-send-input)))

    (when pop
      (pop-to-buffer procbuf)
      (with-current-buffer procbuf (goto-char (point-max))))))

(defun gpb-r-send-traceback (&optional buf)
  (interactive)
  (gpb-r-send-input "traceback()" t buf))


;; Utility functions

(defvar gpb-r--find-buffer--last-buf nil
  "Implementation detail of `gpb-r--find-buffer'")

(defun gpb-r--find-buffer (path &optional cycle)
  "Return a buffer visiting `path'

If `cycle' is t and we can't find a match to the full path of
`path', we cycle through the buffers with the same filename
ignoring the directory."
  (let ((file-name (file-name-nondirectory path))
        ;; Keys are filenames and values are buffers.
        buf buf-file-name bufs dir)
    (cond
     ;; When we eval a buffer that has no file attached, we wrap the buffer
     ;; name in square braces and pretend that is the file name.
     ((string-match "^\\[\\(.*\\)\\]$" (file-name-nondirectory path))
      (setq gpb-r--find-buffer--last-buf nil)
      (get-buffer (match-string 1 (file-name-nondirectory path))))

     ;; If the file path exists, visit that file.
     ((file-exists-p (concat (gpb-r--get-tramp-prefix) path))
      (setq gpb-r--find-buffer--last-buf nil)
      (find-file-noselect path))

     ;; Otherwise, look for the first buffer with the same filename as
     ;; `path', ignoring the directory information.  If `cycle' is t and
     ;; `gpb-r--find-buffer--last-buf' is set, we look for the first buffer
     ;; strictly after `gpb-r--find-buffer--last-buf'.
     (t
      (setq bufs (nconc
                  (when (and cycle gpb-r--find-buffer--last-buf)
                    ;; The buffers after `gpb-r--find-buffer--last-buf'
                    (cdr (member gpb-r--find-buffer--last-buf (buffer-list))))
                  (buffer-list)))
      (while bufs
        (setq buf-file-name (buffer-file-name (car bufs)))
        (if (and buf-file-name
                 (string= (file-name-nondirectory buf-file-name) file-name))
            (setq buf (car bufs)
                  bufs nil)
          (setq bufs (cdr bufs))))

      ;; If we still haven't found a buffer, prompt the user.
      (unless buf (message "Can't find %s" path))

      (setq gpb-r--find-buffer--last-buf buf)

      buf))))
