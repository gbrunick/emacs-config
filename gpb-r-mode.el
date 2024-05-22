;; Provides an alternative inferior R process mode that inherits from
;; `comint-mode' and attempts to use a little in `ess-r-mode' as possible.

(require 'cl-lib)
(require 'subr-x)
(require 'evil)
(require 'ess-r-mode)
(require 'gpb-logging)

(defvar gpb-r-mode-debug t
  "When t we takes steps to make things easier to debug.")

;; Use ESS for R source code files but attempt to stop it from installing
;; all of its seemingly buggy hooks.
(setq ess-r-mode-hook nil
      ess-use-auto-complete nil
      ess-use-company nil
      ess-use-tracebug nil
      ess-imenu-use-S nil
      ess-roxy-hide-show-p nil
      ess-roxy-fold-examples nil
      ess-indent-with-fancy-comments nil
      ess-history-file nil
      ;; ESS doing background stuff over TRAMP may by hanging Emacs.
      ess-can-eval-in-background nil
      poly-r-can-eval-in-background nil)

;; We use a hook to highly customize `ess-r-mode' rather than create our
;; own R code mode to avoid breaking `poly-R' integration.
(add-hook 'ess-r-mode-hook 'gpb-ess-r-mode-hook)
(remove-hook 'shell-mode-hook 'ess-r-package-activate-directory-tracker)

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
    (define-key map "\C-c\C-w" 'gpb-r-getwd)
    (define-key map "\C-c\C-d" 'gpb-r-show-docs)
    (define-key map "\C-s" 'gpb-r-save-history)
    (define-key map "\C-c\C-r" 'gpb-r-read-history)
    (define-key map [remap forward-button] 'gpb-r-forward-button)
    (define-key map [remap backward-button] 'gpb-r-backward-button)
    ;; (define-key map "\C-c\C-v" 'gpb-ess:show-help)
    map)
  "Keymap for `gpb-inferior-r-mode'.")

(when (and (boundp 'evil-mode) evil-mode)
  (evil-define-key 'motion gpb-inferior-r-mode-map [?\t] 'forward-button)
  (evil-define-key 'motion gpb-inferior-r-mode-map [(backtab)] 'backward-button)
  (evil-define-key 'insert gpb-inferior-r-mode-map [?\t] 'completion-at-point))


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
  '(gpb-r--region-eval-preinput-filter)
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

  (setq-local ess-indent-offset 2)
  ;; (setq-local indent-line-function #'gpb-r-indent-line)
  (setq-local completion-at-point-functions
              '(tags-completion-at-point-function dabbrev-capf))
  (setq-local eldoc-documentation-functions nil)
  (setq-local ess-idle-timer-functions nil)

  ;; Configure "!" command.  See gpb-evil.el.
  (gpb-define-eval-code-operator #'gpb-r-eval-region)

  (remove-hook 'xref-backend-functions #'ess-r-xref-backend 'local)
  (remove-hook 'project-find-functions #'ess-r-project 'local)

  (setq ess-indent-offset 2
        ess-indent-with-fancy-comments nil))


(define-derived-mode gpb-inferior-r-mode comint-mode "Inferior R Mode"
  "Major mode for an inferior R process.
\\<gpb-inferior-r-mode-map>
\\{gpb-inferior-r-mode-map}"
  (push (current-buffer) gpb-r-all-inferior-buffers)

  ;; Try to shutdown gracefully when the buffer is killed.
  (add-hook 'kill-buffer-hook #'gpb-r--kill-buffer-hook nil t)

  (setq-local completion-at-point-functions '(gpb-r-completion-at-point))
  (setq-local comint-input-autoexpand nil)
  (setq-local comint-input-sender #'gpb-r--input-sender)
  (setq-local eldoc-documentation-functions nil)

  (set-syntax-table gpb-inferior-r-mode--syntax-table)

  ;; Set up the helper buffer.
  (let ((staging-buffer (gpb-r--get-staging-buffer (current-buffer) 'ensure)))
    (with-current-buffer staging-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (special-mode)))
    (push staging-buffer gpb-r-all-inferior-buffers))

  (add-hook 'comint-preoutput-filter-functions #'gpb-r-mode-preoutput-filter)

  ;; Look for traceback and debug output.
  (add-hook 'comint-output-filter-functions
            #'gpb-r-mode-debug-filter-function nil t)
  (add-hook 'comint-output-filter-functions
            #'gpb-r--add-links-filter-function nil t)

  ;; We add the callback filter last as it may insert text in the buffer
  ;; that we want the other filter functions to pick up (e.g. file link
  ;; text).
  (add-hook 'comint-output-filter-functions
            #'gpb-r--callback-filter-function nil t)

  (gpb-r-read-history)
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

(defun gpb-r-completion-at-point ()
  (save-excursion
    (let* ((buf (current-buffer))
           (proc (get-buffer-process buf))
           (line (buffer-substring-no-properties (process-mark proc) (point)))
           (cmd (format ".gpb_r_mode$get_completions(%s, %S)"
                        (marker-position (process-mark proc)) line))
           (response (gpb-r-send-command cmd buf)))
      (read response))))


(defvar gpb-r-waiting-timer nil)

(defvar gpb-r-mode-lock nil
  "We can only send one syncronous command at a time.")

(defun gpb-r-send-command (cmd &optional buf callback steal-lock)
  "Send CMD to R process in BUF the return the output as a string.

If BUF is omitted, we use the current buffer.  We move the
process into new buffer, send `cmd' to the R process, wait for
the result, and return a buffer that contains the result."
  (interactive "sR Command: ")
  (let* ((buf (or buf (gpb-r-get-proc-buffer)))
         (wrapped-cmd (format ".emacs_cmd({ %S })\n" cmd))
         (proc (or (get-buffer-process buf)
                   (error "No R process available")))
         (msg "Waiting on R process (C-G to cancel)..."))

    ;; Try to pull any pending output from the proces before we send
    ;; `wrapped-cmd'.
    (while (accept-process-output proc 0 nil t))

    (with-current-buffer buf
      (if callback
          (progn
            (push callback gpb-r-mode--async-calls)
            (send-string proc wrapped-cmd))

        ;; Otherwise we have to wait.
        (when gpb-r-mode-lock
          (unless steal-lock (error "gpb-r-mode-lock is not nil")))
        (setq gpb-r-mode-lock 'waiting)
        (push (lambda (txt)
                (message "response: %S" txt)
                (setq gpb-r-mode-lock txt))
              gpb-r-mode--async-calls)

        ;; Provide some feedback if the command takes longer than a second.
        (setq gpb-r-waiting-timer (run-at-time 1 nil `(lambda () (message ,msg))))

        ;; Send the string and wait for a response.
        (send-string proc wrapped-cmd)
        (condition-case error-var
            (progn
              ;; Accept output until the callback above it triggered.
              (while (eq gpb-r-mode-lock 'waiting)
                (accept-process-output proc 1 nil t))

              ;; Cancel the timer if it is still pending.
              (cancel-timer gpb-r-waiting-timer)

              gpb-r-mode-lock)

          ;; If there is an error, of the call hangs and the user has to
          ;; `keyboard-quit' to get control of Emacs, we show the server
          ;; buffer and raise an error.
          ('quit
           (pop-to-buffer (gpb-r--get-staging-buffer buf))
           (error "`gpb-r-send-command' failed")))))))


(defun gpb-r-getwd (&optional buf)
  "Get the TRAMP-qualified working directory of the current R process.

Also updates `default-directory' in the process buffer.  This
function is safe to call when the R process in the browser
debugging state."
  (interactive)
  (let* ((buf (or buf (gpb-r-get-proc-buffer)))
         (local-wd (gpb-r-send-command "cat(sprintf('%s\\n', getwd()))" buf)))
    (with-current-buffer buf
      (setq default-directory (concat (gpb-r--get-tramp-prefix)
                                      (file-name-as-directory local-wd)))

      (when (called-interactively-p)
        (message "Working Dir: %s" default-directory))
      default-directory)))


(defun gpb-r-hanging-command (&optional buf)
  "A command that hangs for testing."
  (interactive)
  (let* ((buf (or buf (gpb-r-get-proc-buffer))))
    (gpb-r-send-command "readline('Press <return> to continue')" buf)))


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


(defun gpb-r-get-proc-buffer ()
  "Get the currently active R process buffer"
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


;; Callback functions.  See gpb-r-mode.R.

(defun gpb-r--add-error-info (file line)
  (save-excursion
    (skip-chars-backward " \n")
    (insert (format " at %s#%s" file line))))


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

;; (defun gpb-r-eval-text-object (obj start end)
;;   (gpb-r-eval-region start end))


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

          ;; Only write to the message buffer to avoid flicker
          (let ((inhibit-message t)) (message "Wrote %s" region-filename))

          (setq line (format ".gpb_r_mode$eval_region_file(%S)" srcbuf))))))
  line)


;; Shut down gracefully

(defun gpb-r--kill-buffer-hook (&optional buf)
  "Attempt to close inferior process gracefully."
  (let* ((buf (or buf (current-buffer)))
         (proc (get-buffer-process buf))
         (staging-buf (gpb-r--get-staging-buffer buf)))
    (and comint-input-ring-file-name
         (y-or-n-p (format "Write %s?" (expand-file-name
                                        comint-input-ring-file-name)))
         (gpb-r-save-history))
    (and staging-buf
         (buffer-live-p staging-buf)
         (progn (kill-buffer server-buf)
                (message "Killed %S" server-buf)))))


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
  (let* ((procbuf (or buf (gpb-r-get-proc-buffer)))
         (proc (get-buffer-process procbuf)))

    (or (process-live-p proc)
        (error "No R process available"))

    (with-current-buffer procbuf
      (save-excursion
        ;; Give R a chance to get the terminal up to date before we send
        ;; the string.
        (accept-process-output proc 0 nil t)
        (comint-goto-process-mark)
        (insert (string-trim cmd))
        (comint-send-input)))

    (when pop (pop-to-buffer procbuf))
    (with-current-buffer procbuf (goto-char (point-max)))))

(defun gpb-r-send-traceback (&optional buf)
  (interactive)
  (gpb-r-send-input "traceback()" t buf))


;; Utility functions

(defvar gpb-r--find-buffer--last-buf nil
  "Implementation detail of `gpb-r--find-buffer'")

(defun gpb-r--find-buffer (path &optional cycle)
  (let* ((working-dir (get-text-property 0 'current-working-dir path))
         (abspath (gpb-r-mode--expand-filename path working-dir)))
    (message "path: %S" path)
    (message "working-dir: %S" working-dir)
    (message "abspath: %S" abspath)
    (cond
     ((file-exists-p abspath)
      (find-file-noselect abspath))
     (t
      (error "Invalid path: %S" abspath)))))


(defun gpb-r--find-buffer-old (path &optional cycle)
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


(defun gpb-r-show-docs (object-name &optional buf)
  "Show help on OBJECT-NAME."
  (interactive (list (read-string "Show Docs: "
                                  (save-excursion
                                    (skip-chars-backward " (")
                                    (symbol-name (symbol-at-point))))))

  (let* ((buf (or buf (gpb-r-get-proc-buffer)))
         (cmd (format "print(help(\"%s\", try.all.packages = FALSE))"
                      object-name))
         (help-buf (get-buffer-create (format "*R Help: %s" object-name)))
         (inhibit-read-only t))

    (gpb-r-send-command cmd buf `(lambda (txt)
                                   (let ((inhibit-read-only t))
                                     (with-current-buffer ,help-buf
                                       (erase-buffer)
                                       (special-mode)
                                       (insert txt)
                                       (goto-char (point-min)))
                                     (display-buffer ,help-buf))))))



;; (defun gpb-r-indent-line ()
;;   (prog1
;;       (ess-r-indent-line)
;;     (let ((pt (point))
;;           (continue t)
;;           init-depth col)
;;       (setq init-depth (car (syntax-ppss)))
;;       (catch 'done
;;         (setq start (save-excursion
;;                       (ess-backward-up-list)
;;                       (unless (looking-at-p "(\\|\\[") (throw 'done t))
;;                       (point)))

;;         ;; Look for an outer comma.  If we find one, we are in an argument
;;         ;; list and should only look back to the start of this argument to
;;         ;; find the "~" or ":=".
;;         (save-excursion
;;           (while (and continue (re-search-backward "," start t))
;;             (when (<= (car (syntax-ppss)) init-depth)
;;               (setq continue nil
;;                     start (point)))))

;;         (save-excursion
;;           (setq continue t)
;;           (beginning-of-line)
;;           (while (and continue (>= (point) start))
;;             (if (re-search-backward "~\\|:=" start t)
;;                 (cond
;;                  ;; If the "~" or ":=" we found is not inside a string or
;;                  ;; some other nested expression.
;;                  ((and (null (nth 3 (syntax-ppss)))
;;                        (<= (car (syntax-ppss)) init-depth))
;;                   (goto-char (match-end 0))
;;                   (skip-chars-forward " ")
;;                   (setq col (current-column)
;;                         continue nil))
;;                  ;; Otherwise, keep looking.
;;                  (t (backward-char)))
;;               ;; We didn't find a string match
;;               (throw 'done t))))

;;         (unless (null col)
;;           (beginning-of-line)
;;           (when (looking-at " +")
;;             (delete-region (match-beginning 0) (match-end 0)))
;;           (indent-to-column col))))))


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

(defun gpb-r--get-staging-buffer (process-buffer &optional ensure)
  "Get the staging buffer associated with PROCESS-BUFFER.
If ENSURE is non-nil, we create the buffer if it doesn't already exist."
  (let ((buf-name (concat (buffer-name process-buffer) " [staging]")))
    (if ensure
        (let ((buf (get-buffer-create buf-name)))
          (with-current-buffer buf (special-mode))
          buf)
      (get-buffer buf-name))))


(defun gpb-r-kill-all-inferior-buffers ()
  "Kill all inferior R process buffers."
  (interactive)
  (dolist (buf gpb-r-all-inferior-buffers)
    (and buf (buffer-live-p buf) (kill-buffer buf)))
  (setq gpb-r-all-inferior-buffers nil))

(defun gpb-r-source-R-init-file ()
  "Source gpb-r-mode.R in the interpreter.

Should be called from the interpreter buffer."
  (let* ((basename "gpb-r-mode.R")
         (local-init-script (locate-library "gpb-r-mode.R"))
         (remote-init-script (expand-file-name (concat "." basename))))

    (cond
     ;; If we are running R on a remote machine over TRAMP, we write our R
     ;; init script to ".gpb-r-mode.R" at the root of the R repo and then
     ;; source it.
     ((file-remote-p remote-init-script)
      (let ((coding-system-for-write 'us-ascii-unix))
        (with-temp-file remote-init-script
          (insert "# Written by Emacs package gpb-r-mode\n\n")
          (insert-file-contents local-init-script)
          ;; Another attempt to clean up line endings.
          (delete-trailing-whitespace)))
      (gpb-r-send-input (format "source(\".%s\")\n" basename)))

     ;; Otherwise, we can just source the file directly.
     (t
      (gpb-r-send-input (format "source(\".%s\")\n" local-init-script))))))


(defun gpb-r-read-history ()
  "Read .Rhistory into comint input ring."
  (interactive)

  ;; Use an absolute path for `comint-input-ring-file-name' so it is not
  ;; impacted by changes to the R working directory.
  (setq-local comint-input-ring-file-name (expand-file-name ".Rhistory"))
  (setq-local comint-input-ring-separator "\n")
  ;; (setq-local comint-input-filter #'gpb-r--comint-input-filter)

  ;; Keep comments in the history as we use comments for some special
  ;; commands like eval region that are actually handled by input filter
  ;; functions.
  (setq-local comint-input-history-ignore "^.* # Emacs command$")
  (comint-read-input-ring)
  (message "Read: %s" comint-input-ring-file-name))


(defun gpb-r-save-history ()
  (interactive)
  (comint-write-input-ring)
  (message "Wrote: %s" comint-input-ring-file-name))


;;
;; Preoutput filter functions
;;

(defvar gpb-r-mode-guid "75b30f72-85a0-483c-98ce-d24414394ff0")
(defvar gpb-r-mode-prompt-regex (format "PROMPT:%s" gpb-r-mode-guid))
(defvar gpb-r-mode-prompt-continue-regex (format "CONTINUE:%s" gpb-r-mode-guid))

(defvar-local gpb-r-mode--async-calls nil
  "Defined in inferior R buffers.  List of callback functions.")


(defun gpb-r-mode-preoutput-filter (string)
  "Preprocess R process output.

Accumulates output into a staging buffer so that it may be returned in
chunks that contain complete lines.  As a result, all
`comint-output-filter-functions' receive only complete lines.

Looks for messages we send from R that indicate when the working directory
has changed in the R process (e.g., when knitting an R markdown document)
and sets the text property `current-working-dir' on STRING to reflect the
working directory at the time the output was generated.  Functions in
'comint-output-filter-functions' can use this text property to properly
expand relative paths in the R output.

Also checks `gpb-r-mode--async-calls' to see if we are in the middle of a
`gpb-r-send-input'.  If so, we accumulate all the R command
output until the next prompt.  We then pop the first function off
`gpb-r-mode--async-calls' and pass it the contents of the buffer as a
string.

Called by `comint' in an R inferior process buffer."
  (save-match-data
    (save-excursion
      (gpb-r-mode-preoutput-filter-1 (current-buffer) string))))


(defun gpb-r-mode-preoutput-filter-1 (buf string)
  "Implementation of `gpb-r-mode-preoutput-filter'

BUF is an inferior R process buffer.  STRING contins output from the R
process."
  (message "gpb-r-mode-preoutput-filter-1 %S %S" buf string)
  (with-current-buffer buf
    (cl-assert (derived-mode-p 'gpb-inferior-r-mode)))

  (if (or (null string) (= (length string) 0))
      "" ; `comint-output-filter' doesn't like it if you return nil.

    (let ((staging-buf (with-current-buffer buf
                         ;; If we create the buffer, we want it to pick up
                         ;; `default-directory' from `buf'.
                         (gpb-r--get-staging-buffer buf 'ensure)))
          (inhibit-read-only t)
          ;; `beg' and `end' will lie on line breaks.
          beg end)
      (with-current-buffer staging-buf
        ;; We use `default-directory' to track the working directory of the
        ;; inferior R process.
        (cl-assert default-directory)

        (save-excursion
          (goto-char (point-max))
          (setq beg (save-excursion (forward-line 0) (point)))
          (insert string)
          (setq end (save-excursion (forward-line 0) (point)))
          (goto-char beg)

          ;; Add `current-working-dir' text properties that track
          ;; changes to the R processes working directory.
          (save-excursion
            (while (re-search-forward "^chdir: \\(.*\\)\n" nil t)
              (put-text-property beg (match-beginning 0)
                                 'current-working-dir
                                 default-directory)
              (setq default-directory (gpb-r-mode--expand-filename
                                       (match-string 1)))
              (unless gpb-r-mode-debug
                (delete-region (match-beginning 0) (match-end 0))))
            (unless (eobp)
              (put-text-property (point) (point-max)
                                 'current-working-dir
                                 default-directory)))

          ;; We only apply this to complete lines.
          (comint-carriage-motion beg end)

          (save-excursion
            (cond
             ;; There is an async call pending.
             ((with-current-buffer buf gpb-r-mode--async-calls)
              (message "asyn calls: %S" gpb-r-mode-prompt-regex)
              ;; If there is no prompt, we return nil.
              (cond
               ((re-search-forward gpb-r-mode-prompt-regex nil t)
                (message "prompt found")
                ;; We have all of the output associated with an async
                ;; command.
                ;;
                ;; Save everything after the prompt.
                (setq string (buffer-substring (match-end 0) (point-max)))
                (message "string %S" string)
                ;; Erase everything except the R process output prior to the
                ;; prompt we found above.
                (delete-region (match-beginning 0) (point-max))
                (goto-char (point-min))
                (let ((callback (with-current-buffer buf
                                  (pop gpb-r-mode--async-calls))))
                  (message "callback: %S" callback)
                  (funcall callback (buffer-substring (point-min) (point-max))))
                ;; Now empty buffer and recurse to handle the rest of `string',
                (erase-buffer)
                (gpb-r-mode-preoutput-filter-1 buf string))

               (t "")))

             ;; There are no async calls pending.
             (t
              (let* ((regex (format "\\(%s\\)\\|\\(%s\\)"
                                    gpb-r-mode-prompt-regex
                                    "Browse\\[[0-9]+\\]>"))
                     (ends-with-prompt (save-excursion
                                         (goto-char (point-max))
                                         (forward-line 0)
                                         (re-search-forward regex nil t))))

                (message "regex %S" regex)
                (message "ends-with-prompt: %S" ends-with-prompt)

                ;; Replace prompt hashes with standard prompts.
                (save-excursion
                  (while (re-search-forward gpb-r-mode-prompt-regex nil t)
                    (replace-match "> ")))

                (if ends-with-prompt
                    ;; The current output ends with a prompt, so return it
                    ;; all.
                    (progn (setq string (buffer-substring (point-min) (point-max)))
                           (erase-buffer)
                           string)

                  ;; The current output does not end with a prompt, so we
                  ;; return all completed lines.
                  (let ((from (point-min))
                        (to (save-excursion (goto-char (point-max))
                                            (forward-line 0)
                                            (point))))
                    (setq string (buffer-substring from to))
                    (delete-region from to)
                    string)))))))))))


;;
;; Output filter functions
;;

(defun gpb-r-mode-debug-filter-function (output)
  "Filter function that tracks debug output"
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
                 (line-number (string-to-number (match-string-no-properties 2)))
                 (buf (gpb-r--find-buffer filename)))
            (gpb-r-show-line buf line-number)))))))

(defun gpb-r-mode--expand-filename (name &optional dir)
  (message "gpb-r-mode--expand-filename: %S %S" name dir)
  (let* ((dir (or dir default-directory))
         (tramp-prefix (or (file-remote-p dir) "")))
    (if (file-name-absolute-p name)
        (concat tramp-prefix name)
      (expand-file-name name dir))))



;; Testing
;;
;; In inferior R buffer:
;;
;; (setq gpb-r-mode-lock nil)
;; (gpb-r-send-command "print(1:10)" "*R*" nil 'steal-lock)


(provide 'gpb-r-mode)
