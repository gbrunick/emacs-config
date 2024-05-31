;; Provides an alternative inferior R process mode that inherits from
;; `comint-mode' and attempts to use a little in `ess-r-mode' as possible.

(require 'cl-lib)
(require 'subr-x)
(require 'evil)
(require 'ess-r-mode)
(require 'gpb-logging)

(defvar gpb-r-debug nil
  "When t, we takes steps to make things easier to debug.")

(defvar-local gpb-r-region-file nil
  "File used to pass code snippets to the R process.
Each `gpb-inferior-r-mode' buffer uses a different region file.")

;; We use ESS for R source code files, but try to stop it from doing things
;; in the background.  This seems to improve my Emacs stability.
(setq ess-r-mode-hook nil
      ess-use-auto-complete nil
      ess-use-company nil
      ess-use-tracebug nil
      ess-imenu-use-S nil
      ess-roxy-hide-show-p nil
      ess-roxy-fold-examples nil
      ess-history-file nil
      ;; ESS doing background stuff over TRAMP may by hanging Emacs.
      ess-can-eval-in-background nil
      poly-r-can-eval-in-background nil)

(remove-hook 'shell-mode-hook 'ess-r-package-activate-directory-tracker)

;; We use a hook to highly customize `ess-r-mode' rather than create our
;; own R code mode to avoid breaking `poly-R' integration.
(add-hook 'ess-r-mode-hook 'gpb-ess-r-mode-hook)

(defvar gpb-ess-r-mode-map-orig ess-r-mode-map
  "A copy of `ess-r-mode-map' before we redfined it.")

;; Completely stomp over `ess-r-mode-map'.
(setq ess-r-mode-map (let ((keymap (make-sparse-keymap)))
                       (define-key keymap "\C-c\C-b" 'gpb-r-insert-browser)
                       (define-key keymap "\C-c\C-c" 'gpb-r-save-and-exec-command)
                       (define-key keymap "\C-c\C-d" 'gpb-r-show-docs)
                       ;; Eval current function
                       (define-key keymap "\C-\M-x" "!id")
                       ;; You have to set a keymap to avoid inheriting bindings.
                       (set-keymap-parent keymap (make-sparse-keymap))
                       keymap))

(defvar gpb-inferior-r-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\t] 'gpb-r-tab-command)
    (define-key map [(backtab)] 'backward-button)
    (define-key map "\C-c\C-c" 'gpb-r-set-active-process)
    (define-key map "\C-c\C-t" 'gpb-r-send-traceback)
    (define-key map "\C-c\C-w" 'gpb-r-sync-working-dir)
    (define-key map "\C-c\C-d" 'gpb-r-show-docs)
    (define-key map "\C-s" 'gpb-r-save-history)
    (define-key map "\C-c\C-r" 'gpb-r-read-history)
    (define-key map [remap forward-button] 'gpb-r-forward-button)
    (define-key map [remap backward-button] 'gpb-r-backward-button)
    ;; (define-key map "\C-c\C-v" 'gpb-ess:show-help)
    map)
  "Keymap for `gpb-inferior-r-mode'.")

(when (and (boundp 'evil-mode) evil-mode)
  (evil-define-key 'normal gpb-inferior-r-mode-map (kbd "TAB") 'forward-button)
  (evil-define-key 'normal gpb-inferior-r-mode-map [(backtab)] 'backward-button))
  ;; (evil-define-key 'insert gpb-inferior-r-mode-map [?\t] 'completion-at-point))


(defgroup gpb-r-mode nil
  "Settings related to the Emacs 'gpb-r-mode' package/feature.")

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
  '(gpb-r-region-eval-preinput-filter
    gpb-r-remap-functions-preinput-filter)
  "A list of functions that modify R process input.

These functions are called after the input is added to the input
ring and immediately before the input is sent to the inferior R
process.  The current use case is to interpret 'magic' commands
that are imbedded in R comments")

(defvar gpb-r-end-of-output-marker
  "END:75b30f72-85a0-483c-98ce-d24414394ff0"
  "Arbitrary string used to denote the end of R output.")

(defvar gpb-r-active-process-buffer nil
  "The currently active R process.

At the moment, there can only be one active process")

(defvar gpb-r-all-inferior-buffers nil
  "A list of buffers.  Used by `gpb-r-kill-all-inferior-buffers'.")

(defvar gpb-inferior-r-mode--syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?. "_" st)
    st))

(defun gpb-ess-r-mode-hook ()
  "Hook function that overrides most of `ess-r-mode'"
  ;; Allow movement within camel-case identifiers.
  (subword-mode 1)

  ;; Use etags rather than ESS's custom xref implementation.
  (xref-etags-mode 1)

  ;; (setq-local indent-line-function #'gpb-r-indent-line)
  (setq-local completion-at-point-functions
              '(tags-completion-at-point-function dabbrev-capf))
  (setq-local eldoc-documentation-functions nil)
  (setq-local ess-idle-timer-functions nil)

  ;; Configure "!" command.  See gpb-evil.el.
  (gpb-define-eval-code-operator #'gpb-r-eval-region)

  (remove-hook 'xref-backend-functions #'ess-r-xref-backend 'local)
  (remove-hook 'project-find-functions #'ess-r-project 'local)

  ;; Not sure exactly what is required here.
  (setq ess-indent-offset 2
        ess-indent-with-fancy-comments nil))


(define-derived-mode gpb-inferior-r-mode comint-mode "Inferior R Mode"
  "Major mode for an inferior R process.
\\<gpb-inferior-r-mode-map>
\\{gpb-inferior-r-mode-map}"
  (push (current-buffer) gpb-r-all-inferior-buffers)

  ;; Try to shutdown gracefully when the buffer is killed.
  (add-hook 'kill-buffer-hook #'gpb-r-kill-buffer-hook nil t)

  (setq-local completion-at-point-functions '(gpb-r-completion-at-point))
  (setq-local comint-input-autoexpand nil)
  (setq-local comint-input-sender #'gpb-r-input-sender)
  (setq-local eldoc-documentation-functions nil)

  (set-syntax-table gpb-inferior-r-mode--syntax-table)

  ;; Set up the helper buffer.
  (let ((staging-buffer (gpb-r-get-staging-buffer (current-buffer) 'ensure)))
    (with-current-buffer staging-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (special-mode)))
    (push staging-buffer gpb-r-all-inferior-buffers))

  (add-hook 'comint-preoutput-filter-functions
            #'gpb-r-preoutput-filter nil t)

  ;; Looks for debug breakpoints and jump to the corresponding buffer
  ;; location.
  (add-hook 'comint-output-filter-functions
            #'gpb-r-debug-filter-function nil t)

  ;; Make filenames clickable buttons.
  (add-hook 'comint-output-filter-functions
            #'gpb-r-add-buttons-filter nil t)

  (gpb-r-read-history)

  ;; Initialize Emacs/R connection.
  (gpb-r-source-R-init-file))


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
  (or
   (forward-button 1 nil t t)
   (goto-char (point-max))))

(defun gpb-r-backward-button ()
  (interactive)
  (or
   (forward-button -1 nil t t)
   (goto-char (point-min))))

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


(defvar gpb-r-show-line--overlay nil
  "Current highlighting overlay used in `gpb-r-show-line")

(defun gpb-r-show-line (place line-number &optional pop-to face)
  "Show line LINE in PLACE in some other window.

PLACE is either a filename or a buffer name wrapped in square braces like
\"[*temp*]\".  If `pop-to' is non-nil, switch to the buffer in which the line is
displayed."
  ;; If given a filename, convert that to a buffer.
  (setq buf (or
             (and (stringp place)
                  (or
                   ;; A line in a buffer that may not have a file.
                   (and (string-match "\\[\\(.+\\)\\]" place)
                        (get-buffer (match-string 1 place)))

                   (let ((filename (gpb-r-expand-filename place)))
                     (and (file-exists-p filename)
                          (find-file-noselect filename)))
                   (find-file-noselect (read-file-name
                                        (format "Find %s: "
                                                (file-name-nondirectory filename))
                                        (get-text-property
                                         0 'current-working-dir filename)))))
             place))

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


(defun gpb-r-get-proc-buffer ()
  "Get the currently active R process buffer"
  ;; (message "gpb-r-get-proc-buffer: %S %S"
  ;;          (current-buffer) gpb-r-active-process-buffer)
  (let ((proc1 (get-buffer-process (current-buffer)))
        (proc2 (get-buffer-process gpb-r-active-process-buffer)))
    (or
     (and (process-live-p proc1) (current-buffer))
     (and (process-live-p proc2) gpb-r-active-process-buffer))))


(defun gpb-r-insert-browser ()
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


(defun gpb-r-region-eval-preinput-filter (line)
  "Input filter that looks for eval region commands"
  (when (string-match "# *eval region: +\\(\".*\"\\) +\\([0-9]+\\)-\\([0-9]+\\)$"
                      line)
    (let ((srcbuf (read (match-string 1 line)))
          (line1 (string-to-number (match-string 2 line)))
          (line2 (string-to-number (match-string 3 line)))
          (procbuf (current-buffer))
          (region-filename (or gpb-r-region-file
                               (error "gpb-r-region-file not set")))
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

          ;; Only write to the message buffer to avoid flicker
          (let ((inhibit-message t)) (message "Wrote %s" region-filename))

          (setq line (format ".gpb_r_mode$eval_region_file(%S)" srcbuf))))))
  line)


(defun gpb-r-remap-functions-preinput-filter (line)
  "Input filter that remaps some functions to advised versions."
  (let ((pairs '(("^traceback(" ".gpb_r_mode$traceback(")
                 ("^source(" ".gpb_r_mode$source(")
                 ("^rmarkdown::render(" ".gpb_r_mode$render("))))
   (dolist (from-to pairs)
     (setq line (replace-regexp-in-string (car from-to) (cadr from-to)
                                          line)))
   line))


;; Shut down gracefully

(defun gpb-r-kill-buffer-hook (&optional buf)
  "Attempt to close inferior process gracefully."
  (let* ((buf (or buf (current-buffer)))
         (proc (get-buffer-process buf))
         (staging-buf (gpb-r-get-staging-buffer buf)))
    ;; (and comint-input-ring-file-name
    ;;      (y-or-n-p (format "Write %s?" (expand-file-name
    ;;                                     comint-input-ring-file-name)))
    ;;      (gpb-r-save-history))
    (and staging-buf
         (buffer-live-p staging-buf)
         (progn (kill-buffer staging-buf)
                (message "Killed %S" staging-buf)))))


;; Input preprocessing

(defun gpb-r-input-sender (proc input)
  "This function is bound to `comint-input-sender'.

Apply each function in `gpb-r-preinput-filter-functions' and then
send the resulting string to `comint-simple-send'."
  (dolist (f gpb-r-preinput-filter-functions)
    (setq input (funcall f input)))
  (comint-simple-send proc input))


;; Shortcuts for commands

(defun gpb-r-send-input (cmd &optional pop buf)
  "Insert `cmd' into R process buffer and send."
  (let* ((procbuf (or buf (gpb-r-get-proc-buffer)))
         (proc (get-buffer-process procbuf)))

    (or (process-live-p proc)
        (error "No R process available"))

    (with-current-buffer procbuf
      (save-excursion
        ;; Give R a chance to get the terminal up to date before we send
        ;; the string.
        (comint-goto-process-mark)
        (insert (string-trim cmd))
        (comint-send-input)))

    (when pop (pop-to-buffer procbuf))
    (with-current-buffer procbuf (goto-char (point-max)))))

(defun gpb-r-send-traceback (&optional buf)
  (interactive)
  (gpb-r-send-input "traceback()" t buf))


;; Utility functions


(defun gpb-r-create-function-header ()
  (interactive)
  (let (start args (indent "#'"))
    (save-excursion
      (search-forward-regexp "function(")
      (setq start (match-end 0))
      (backward-char 1)
      (forward-sexp 1)
      (setq end (point))
      (goto-char start)
      (while (re-search-forward " *\\([a-zA-Z0-9_]+\\) *[,=)]" end t)
        (push (match-string 1) args)))

    (skip-chars-forward " \n")
    (forward-line 0)
    (setq indent (concat (string-pad "" (current-indentation)) indent))
    (insert indent " ")   ;; We leave the point here
    (setq pt (point))
    (insert "\n" indent "\n")
    (dolist (arg (reverse args))
      (insert (format "%s @param %s \n" indent (string-trim arg))))
    (insert indent "\n")
    (insert indent " @return \n")
    (insert indent "\n")
    (insert indent " @export \n")
    (insert indent "\n")
    (goto-char pt)
    (when (and (boundp 'evil-mode) evil-mode)
      (run-at-time 0.1 nil (lambda ()
                             (evil-insert-state)
                             (goto-char (1+ (point))))))))

(defun gpb-r-get-staging-buffer (process-buffer &optional ensure)
  "Get the staging buffer associated with PROCESS-BUFFER.
If ENSURE is non-nil, we create the buffer if it doesn't already exist."
  (let* ((buf-name (concat (buffer-name process-buffer) " [staging]"))
         (existing-buf (get-buffer buf-name)))
    (cond
     (existing-buf existing-buf)
     (ensure (let ((buf (get-buffer-create buf-name)))
               (with-current-buffer buf (special-mode))
               buf)))))


(defun gpb-r-kill-all-inferior-buffers ()
  "Kill all inferior R process buffers."
  (interactive)
  (dolist (buf gpb-r-all-inferior-buffers)
    (and buf (buffer-live-p buf) (kill-buffer buf)))
  (setq gpb-r-all-inferior-buffers nil))

(defun gpb-r-read-history ()
  "Read .Rhistory into comint input ring."
  (interactive)

  ;; Use an absolute path for `comint-input-ring-file-name' so it is not
  ;; impacted by changes to the R working directory.
  (setq-local comint-input-ring-file-name (expand-file-name ".Rhistory"))
  (setq-local comint-input-ring-separator "\n")
  ;; (setq-local comint-input-filter #'gpb-r-comint-input-filter)

  ;; Keep comments in the history as we use comments for some special
  ;; commands like eval region that are actually handled by input filter
  ;; functions.
  (setq-local comint-input-history-ignore "^.* # Emacs command$")
  (comint-read-input-ring)
  (message "Read %s." comint-input-ring-file-name))


(defun gpb-r-save-history ()
  (interactive)
  (comint-write-input-ring)
  (message "Wrote %s." comint-input-ring-file-name))


;;
;; Preoutput filter functions
;;

(defvar gpb-r-guid "75b30f72-85a0-483c-98ce-d24414394ff0")
(defvar gpb-r-prompt (format "PROMPT:%s" gpb-r-guid))
(defvar gpb-r-output-marker (format "OUTPUT:%s\n" gpb-r-guid)
  "Used by `gpb-r-send-command' to mark the start of output.")

(defvar-local gpb-r-pending-commands nil
  "Defined in inferior R buffers.  List of callback functions.")


(defun gpb-r-preoutput-filter (string)
  "Preprocess R process output.

Called by `comint' in an R inferior process buffer.  STRING gives new
output from the R process.  This function is called before output is
inserted in the `comint' buffer, so we can use it to intercept output
before it gets to the R process buffer.

Accumulates output into a staging buffer so that it may be returned in
chunks that contain complete lines.  As a result, all
`comint-output-filter-functions' receive only complete lines.

Looks for messages we send from R that indicate when the working directory
has changed in the R process (e.g., when knitting an R markdown document)
and sets the text property `current-working-dir' on STRING to reflect the
working directory at the time the output was generated.  Functions in
'comint-output-filter-functions' can use this text property to properly
expand relative paths in the R output.

We also check for the marker `gpb-r-output-marker'.  If we see this, we
collect all output until the next `gpb-r-prompt' and pass it as a string to
the next function we pop from `gpb-r-pending-commands' instead of sending it to
the inferior R buffer.  `gpb-r-send-command' uses this mechanism."
  ;; (message "gpb-r-preoutput-filter in: %S" string)
  (let ((string-out (save-match-data
                      (save-excursion
                        (gpb-r-preoutput-filter-1 (current-buffer) string)))))
    ;; (message "gpb-r-preoutput-filter out: %S" string-out)
    string-out))

(defun gpb-r-preoutput-filter-1 (buf string)
  "Implementation of `gpb-r-preoutput-filter'

BUF is an inferior R process buffer.  STRING contins output from the R
process."
  (when gpb-r-debug
    (message "gpb-r-preoutput-filter-1 string in %S\n%s" string string))
  (with-current-buffer buf
    (cl-assert (derived-mode-p 'gpb-inferior-r-mode)))

  (if (or (null string) (= (length string) 0))
      "" ; `comint-output-filter' doesn't like it if you return nil.

    (let ((staging-buf (with-current-buffer buf
                         ;; If we create the buffer, we want it to pick up
                         ;; `default-directory' from `buf'.
                         (gpb-r-get-staging-buffer buf 'ensure)))
          (inhibit-read-only t)
          (prompt-regex (format "\\(%s\\)\\|\\(%s\\)"
                                gpb-r-prompt "Browse[[0-9]+]> "))
          ;; `beg' and `end' will lie on line breaks.
          beg end previous-output)

      ;; (gpb-r-dump-buffer staging-buf "gpb-r-preoutput-filter before")

      (with-current-buffer staging-buf
        ;; We use `default-directory' to track the working directory of the
        ;; inferior R process.
        (cl-assert default-directory)
        (goto-char (point-max))
        (setq beg (save-excursion (forward-line 0) (copy-marker (point))))
        (insert string)
        (setq end (save-excursion (forward-line 0)
                                  (copy-marker (point) t)))

        ;; (gpb-r-dump-buffer (current-buffer) "gpb-r-preoutput-filter insert")

        ;; We only apply this to complete lines.
        (save-excursion (comint-carriage-motion beg end))

        ;; Add `current-working-dir' text properties that give to the
        ;; R processes working directory at the time of output.
        (goto-char beg)
        (while (re-search-forward "^chdir: +\\(.*\\)\n" nil t)
          (put-text-property beg (match-beginning 0)
                             'current-working-dir
                             default-directory)
          (setq default-directory (gpb-r-expand-filename
                                   (string-trim (match-string 1))))
          (message "%s Working directory: %s" buf default-directory)
          ;; Delete the output marker
          (delete-region (match-beginning 0) (match-end 0)))

        (unless (eobp)
          (put-text-property (point) (point-max)
                             'current-working-dir
                             default-directory))
        ;; (gpb-r-dump-buffer (current-buffer) "gpb-r-preoutput-filter clean"))

        ;; Include a final prompt on an incomplete line in the output if
        ;; present.
        (goto-char end)
        (when (re-search-forward prompt-regex nil t)
          (setq end (copy-marker (match-end 0) t)))

        (goto-char (point-min))
        (let* ((command-output-start (search-forward gpb-r-output-marker nil t))
               ;; When there is a command pending, there may be R process
               ;; output from before the command started.  We store
               ;; such output in `previous-output'.
               (previous-output-end (and command-output-start
                                         (match-beginning 0)))
               ;; If we see a prompt after the start of a command, we know
               ;; the command is complete.
               (prompt-end (and command-output-start
                                (re-search-forward prompt-regex nil t)))
               (command-output-end (and prompt-end
                                        (match-beginning 0))))

          (cond
           (prompt-end
            ;; We have all of the output associated with the next command.
            (let ((callback (with-current-buffer buf
                              (pop gpb-r-pending-commands)))
                  (previous-output (buffer-substring (point-min)
                                                     previous-output-end))
                  (command-output (buffer-substring command-output-start
                                                    command-output-end))
                  (next-output (buffer-substring prompt-end (point-max))))

              ;; Don't run `callback' immediately so we don't have to worry
              ;; about it erroring out or changing state in the filter
              ;; function.
              (run-at-time 0 nil callback buf command-output)

              ;; `next-output' might contain another command response, so we
              ;; need to recurse to handle that case.
              (erase-buffer)
              (setq string (concat previous-output
                                   (gpb-r-preoutput-filter-1 buf next-output)))))

           (command-output-start
            ;; There is a pending command that is not complete.
            (setq string (gpb-r-cut-region (point-min) previous-output-end)))

           (t
            ;; Replace prompt hashes with standard prompts.
            (save-excursion
              (goto-char (point-min))
              (while (search-forward gpb-r-prompt end t)
                (replace-match "> ")))

            (setq string (gpb-r-cut-region (point-min) end)))))))

    (when gpb-r-debug
      (gpb-r-dump-buffer (current-buffer) "gpb-r-preoutput-filter after")
      (message "gpb-r-preoutput-filter string out: %S\n%s" string string))

    string))


;;
;; Output filter functions
;;

(defun gpb-r-debug-filter-function (output)
  "Filter function that tracks debug source locations"
  ;; This function is called with empty output at odd times that can lead
  ;; to misleading highlighting if you don't ignore it those calls.
  (when (and output (> (length output) 0))
    (save-match-data
      (save-excursion
        (goto-char comint-last-output-start)
        (when (re-search-forward "^debug at \\([^#:]+\\)[#:]\\([0-9]+\\):" nil t)
          ;; We grab the match substrings first as the later functions
          ;; might reset the match data.
          (let* ((filename (match-string 1))
                 (line-number (string-to-number (match-string-no-properties 2))))
            (gpb-r-show-line filename line-number)))))))


(defun gpb-r-add-buttons-filter (output)
  (when (and output (> (length output) 0))
    (save-match-data
      (save-excursion
        (let* ((proc (get-buffer-process (current-buffer))))
          (gpb-r-add-buttons-filter-1 comint-last-output-start
                                     (process-mark proc)))))))


(defun gpb-r-add-buttons-filter-1 (beg end)
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
            (let* ((file (buffer-substring (match-beginning file-subexp)
                                           (match-end file-subexp)))
                   (line (ignore-errors
                           (string-to-number (buffer-substring
                                              (match-beginning line-subexp)
                                              (match-end line-subexp)))))
                   (beg (match-beginning link-subexp))
                   (end (match-end link-subexp)))

              ;; Hide the file directory to save space.
              ;; (goto-char (match-end link-subexp))
              ;; (when (re-search-backward "/" beg t)
              ;;   (put-text-property beg (match-end 0) 'display "")
              ;;   (setq beg (match-end 0)))

              (make-text-button beg end
               'file file
               'line line
               'action #'gpb-r-follow-link
               ;; Abbreviate the name in the buffer, but show the full path
               ;; in the echo area when you tab to the button.
               ;; 'display (format "%s#%s" (file-name-nondirectory file) line)
               'help-echo (format "%s#%s" file line)))))))))


(defun gpb-r-follow-link (button)
  (let* ((file (button-get button 'file))
         (line (button-get button 'line)))
    (gpb-r-show-line file line)))


;;
;; Sending commands to R
;;

(defvar gpb-r-send-command--response nil
  "Implementation detail of `gpb-r-send-command'.
Used to pass R process output picked up in the filter function
`gpb-r-preoutput-filter' back to `gpb-r-send-command' during a blocking
call to `gpb-r-send-command'.")


(defun gpb-r-send-command (cmd &optional buf callback)
  "Send CMD to R process in BUF the return the output as a string.

If BUF is omitted, we use the current buffer.  We move the
process into new buffer, send `cmd' to the R process, wait for
the result, and return a buffer that contains the result.

Echoes `gpb-r-output-marker' from the R process.  This tells
`gpb-r-preoutput-filter' to steal all the lines of output until the next
`gpb-r-prompt' and send them to the first function in the list
`gpb-r-pending-commands'.

If CALLBACK is `nil', this function blocks until we have a response from R.

Otherwise, CALLBACK is called once with two arguments: the buffer
containing the inferior R process and a string containing the command
output.  This usage is preferred to blocking when possible.  If you don't
care about the result, pass `ignore' as CALLBACK."
  (interactive "sR Command: ")
  (let* ((buf (or buf (gpb-r-get-proc-buffer)))
         (staging-buf (and buf (gpb-r-get-staging-buffer buf 'ensure)))
         ;; We send a string and parse it on the R side.
         (wrapped-cmd (format "cat(%S); eval(parse(text = %S))\n"
                              gpb-r-output-marker cmd))
         (proc (or (get-buffer-process buf)
                   (error "No R process available")))
         (msg "Waiting on R process (C-G to cancel)...")
         timer response)

    ;; `gpb-r-pending-commands' is local to `buf'
    (with-current-buffer buf
      (if callback
          (progn
            ;; Add `callback' to the back of the list.
            (setq gpb-r-pending-commands (nconc gpb-r-pending-commands
                                                (list callback)))
            (send-string proc wrapped-cmd))

        (setq gpb-r-send-command--response 'waiting
              callback (lambda (buf txt)
                         ;; (message "callback: %S" txt)
                         (setq gpb-r-send-command--response txt))
              gpb-r-pending-commands (nconc gpb-r-pending-commands
                                            (list callback)))

        ;; Provide some feedback if the command takes longer than a second.
        (setq timer (run-at-time 1 nil
                                 `(lambda ()
                                    (message ,msg)
                                    (pop-to-buffer ,staging-buf)
                                    (redisplay))))

        ;; Send the string and wait for a response.
        (send-string proc wrapped-cmd)
        (condition-case error-var
            (progn
              ;; Accept output until the callback above is triggered.
              (while (eq gpb-r-send-command--response 'waiting)
                ;; Filters and timers run as we wait for more output.
                (accept-process-output proc 1 nil))

              (when gpb-r-debug
                (message "gpb-r-send-command--response: %S"
                         gpb-r-send-command--response))

              ;; Cancel the timer if it is still pending.
              (cancel-timer timer)

              (let ((value gpb-r-send-command--response))
                (setq gpb-r-send-command--response nil)
                value))

          ;; If the call hangs and the user has to `keyboard-quit' to get
          ;; control of Emacs, show the staging buffer to help debug.
          ('quit
           (pop-to-buffer staging-buf)
           (error "`gpb-r-send-command' failed")))))))


(defun gpb-r-show-docs (object-name &optional buf)
  "Show help on OBJECT-NAME."
  (interactive
   (list (gpb-r-read-r-object "Show Docs: " (gpb-r-object-at-point))))
  (let* ((buf (or buf (gpb-r-get-proc-buffer)))
         (cmd (format "?%s" object-name)))
    (gpb-r-send-command cmd buf `(lambda (buf txt)
                                   (gpb-r-show-docs-1 ,object-name txt)))))


(defun gpb-r-show-docs-1 (objname help-txt)
  ;; (message "gpb-r-show-docs-1: %S %S" buf txt)
  (let ((buf (get-buffer-create (format "*R Help: %s*" objname)))
        (inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer)
      (special-mode)
      (insert help-txt)
      (goto-char (point-min)))
    (pop-to-buffer buf)))


(defun gpb-r-sync-working-dir (&optional buf)
  "Ensure that `default-director' reflects the R working directory."
  (interactive)
  (let* ((buf (or buf (gpb-r-get-proc-buffer))))
    (gpb-r-send-command ".gpb_r_mode$sync_working_dir()" buf 'ignore)))


(defun gpb-r-source-R-init-file (&optional buf)
  "Source gpb-r-mode.R in the interpreter.

Should be called from the interpreter buffer.  Returns the region file path."
  ;; (message "gpb-r-source-R-init-file: %S" (current-buffer))
  (let* ((buf (or buf (current-buffer)))
         (basename "gpb-r-mode.R")
         (local-init-script (locate-library "gpb-r-mode.R"))
         (remote-init-script (expand-file-name (concat "." basename)))
         path init-output region-file)

    (cond
     ;; If we are running R on a remote machine over TRAMP, we write our R
     ;; init script to ".gpb-r-mode.R" at the root of the R repo and then
     ;; source it.
     ((file-remote-p remote-init-script)
      (let ((coding-system-for-write 'us-ascii-unix))
        (with-temp-file remote-init-script
          (insert-file-contents local-init-script)
          ;; Another attempt to clean up line endings.
          (delete-trailing-whitespace)))
      (setq path (concat "." basename)))

     ;; Otherwise, we can just source the file directly.
     (t
      (setq path (concat "." local-init-script))))

    (gpb-r-send-command (format "source(%S)\n" path) buf
                        #'gpb-r-source-R-init-file-1)))


(defun gpb-r-source-R-init-file-1 (buf output)
  (let ((region-file (with-temp-buffer
                       ;; (message "gpb-r-source-R-init-file:\n%s\n" init-output)
                       (insert output)
                       (goto-char (point-min))
                       (re-search-forward "^region-file: +\\(.*\\)$")
                       (match-string 1))))

    ;; `gpb-r-preoutput-filter' adds a text property `current-working-dir'
    ;; to `output'.  `gpb-r-expand-filename' uses this and
    ;; `default-directory' to get an absolute path.  At which point, we can
    ;; drop the text properties.
    (setq region-file (substring-no-properties
                       (gpb-r-expand-filename region-file)))

    (when gpb-r-debug
      (message "Region file: %S" region-file))

    (with-current-buffer buf
      (setq-local gpb-r-region-file region-file))

    region-file))

;;
;; Completion
;;

(defvar gpb-r-get-completions--cache nil
  "A plist with keys :buf :line :beg :end :completions")

(defun gpb-r-get-completions (line &optional buf)
  "Returns a plist with keys :buf :line :beg :end :completions."
  (save-excursion
    (let* ((buf (or buf (gpb-r-get-proc-buffer)))
           (proc (get-buffer-process buf))
           (cmd (format ".gpb_r_mode$get_completions(%S)" line))
           (cache-buf  (plist-get gpb-r-get-completions--cache 'buf))
           (cache-line (plist-get gpb-r-get-completions--cache 'line))
           response)
      ;; Only call R if we need to update the cache value.
      (unless (and (> (length line) 0)
                   (> (length cache-line) 0)
                   (eq buf cache-buf)
                   (string-prefix-p cache-line line))
        (setq response (read (gpb-r-send-command cmd buf)))
        ;; (message "response: %S" response)
        (setq gpb-r-get-completions--cache `( :buf ,buf
                                              :line ,line
                                              :beg ,(nth 0 response)
                                              :end ,(nth 1 response)
                                              :completions ,(nth 2 response))))
     gpb-r-get-completions--cache)))

(defun gpb-r-completion-at-point ()
  (save-excursion
    (let* ((buf (gpb-r-get-proc-buffer))
           (proc (get-buffer-process buf))
           (line (buffer-substring-no-properties (process-mark proc) (point)))
           (completion-info (gpb-r-get-completions line buf)))
      ;; (message "completion-info: %S" completion-info)
      (list (+ (process-mark proc) (plist-get completion-info :beg))
            (+ (process-mark proc) (plist-get completion-info :end))
            (plist-get completion-info :completions)))))

(defun gpb-r-minibuffer-complete (line)
  (let ((completion-info (gpb-r-get-completions line)))
   (plist-get completion-info :completions)))

(defvar gpb-r-read-r-object--history nil)

(defun gpb-r-read-r-object (prompt &optional initial-input)
  (interactive)
  (completing-read prompt
                   (completion-table-dynamic #'gpb-r-minibuffer-complete t)
                   nil
                   nil
                   initial-input
                   'gpb-r-read-r-object--history))


;;
;; Utility Functions
;;

(defun gpb-r-expand-filename (name &optional dir)
  (let* ((dir (or dir
                  (get-text-property 0 'current-working-dir name)
                  default-directory))
         (tramp-prefix (or (file-remote-p dir) "")))
    (cond
     ((file-remote-p name)
      name)
     ((file-name-absolute-p name)
      (concat tramp-prefix name))
     (t
      (expand-file-name name dir)))))


(defun gpb-r-string-limit (txt &optional n)
  "Truncate string TXT to length at most N."
  (let ((len (length txt))
       (n (or n 120))
        m)
    (if (< (length txt) n)
        txt
      (cl-assert (>= n 2))
      (setq m (/ n 2))
      (concat (substring txt 0 m)
             "..."
             (substring txt (- len m) len)))))

(defun gpb-r-cut-region (&optional beg end)
  "Delete and return region as a string."
  (let ((beg (or beg (point-min)))
        (end (or end (point-max))))
  (prog1 (buffer-substring beg end)
    ;; (message "deleting region: %S" (buffer-substring beg end))
    (delete-region beg end))))

(defun gpb-r-object-at-point ()
  (interactive)
  (let ((table (copy-syntax-table (syntax-table))))
    (modify-syntax-entry ?: "_" table)
    (with-syntax-table table
      (save-excursion
        (skip-syntax-forward "_")
        (thing-at-point 'symbol)))))


;; Testing
;;
;; In inferior R buffer:
;;
;; (gpb-r-send-command "print(1:10)" "*R*" nil)

(defun gpb-r-test-command (&optional buf)
  "A simple command testing."
  (interactive)
  (let* ((buf (or buf (gpb-r-get-proc-buffer))))
    (gpb-r-send-command "print(1:10)\n" buf)))

(defun gpb-r-hanging-command (&optional buf)
  "A command that hangs for testing."
  (interactive)
  (let* ((buf (or buf (gpb-r-get-proc-buffer))))
    (gpb-r-send-command "readline('Press <return> to continue')" buf)))

(defun gpb-r-dump-buffer (buf &optional label)
  (save-restriction
    (widen)
    (let* ((buf (get-buffer buf))
           (label (if label (format " at %s" label) ""))
           (contents (with-current-buffer buf
                       (buffer-substring (point-min) (point-max)))))
      (message "Contents of %S%s: %S\n%s" buf label contents contents))))


(provide 'gpb-r-mode)
