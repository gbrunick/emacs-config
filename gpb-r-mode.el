;; Provides an alternative inferior R process mode that inherits from
;; `comint-mode' and attempts to use a little in `ess-r-mode' as possible.

(require 'cl-lib)
(require 'subr-x)
(require 'evil)
(require 'gpb-logging)

;; We need to set these before we load ESS.
(setq-default ess-indent-offset 2
              ess-indent-with-fancy-comments nil)

(require 'ess-r-mode)

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
                       (define-key keymap [?\t] 'gpb-r-tab-command)
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
    (define-key map "\C-c\C-f" 'gpb-r-toggle-auto-follow)
    (define-key map [remap forward-button] 'gpb-r-forward-button)
    (define-key map [remap backward-button] 'gpb-r-backward-button)
    ;; (define-key map "\C-c\C-v" 'gpb-ess:show-help)
    map)
  "Keymap for `gpb-inferior-r-mode'.")

(when (and (boundp 'evil-mode) evil-mode)
  (evil-define-key 'normal gpb-inferior-r-mode-map (kbd "TAB") 'forward-button)
  (evil-define-key 'normal gpb-inferior-r-mode-map [(backtab)] 'backward-button)
  (evil-define-key 'insert ess-r-mode-map [?\t] 'completion-at-point))


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
  '(gpb-r-region-eval-preinput-filter)
  "A list of functions that modify R process input.

These functions are called after the input is added to the input
ring and immediately before the input is sent to the inferior R
process.  The current use case is to interpret 'magic' commands
that are imbedded in R comments")

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
  (subword-mode 1)
  (xref-etags-mode 1)
  (auto-fill-mode 1)

  (ess-set-style 'DEFAULT)

  ;; (setq-local completion-at-point-functions
  ;;             '(tags-completion-at-point-function dabbrev-capf))
  (setq-local completion-at-point-functions '(gpb-r-completion-at-point))
  (setq-local eldoc-documentation-functions nil)
  (setq-local ess-idle-timer-functions nil)

  (remove-hook 'xref-backend-functions #'ess-r-xref-backend 'local)
  (remove-hook 'project-find-functions #'ess-r-project 'local)

  ;; Configure "!" command.  See gpb-evil.el.
  (gpb-define-eval-code-operator #'gpb-r-eval-region))


(define-derived-mode gpb-inferior-r-mode comint-mode "Inferior R Mode"
  "Major mode for an inferior R process.
\\<gpb-inferior-r-mode-map>
\\{gpb-inferior-r-mode-map}"
  (push (current-buffer) gpb-r-all-inferior-buffers)

  (subword-mode 1)

  ;; Try to shutdown gracefully when the buffer is killed.
  (add-hook 'kill-buffer-hook #'gpb-r-kill-buffer-hook nil t)

  (setq-local completion-at-point-functions '(gpb-r-completion-at-point))
  (setq-local comint-input-autoexpand nil)
  (setq-local comint-input-sender #'gpb-r-input-sender)
  (setq-local eldoc-documentation-functions nil)

  (set-syntax-table gpb-inferior-r-mode--syntax-table)

  ;; Initialize the helper buffer.
  (let ((staging-buffer (gpb-r-get-staging-buffer (current-buffer) 'ensure)))
    (with-current-buffer staging-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (special-mode)))
    (push staging-buffer gpb-r-all-inferior-buffers))

  (gpb-r-read-history)

  ;; We don't send anything to the R process until it has echoed its first
  ;; prompt.
  (add-hook 'comint-output-filter-functions
            #'gpb-inferior-r-mode-2 nil t))


(defun gpb-inferior-r-mode-2 (&rest args)
  "Implementation detail of `gpb-inferior-r-mode'
Waits for the first prompt from R."
  (save-excursion
    (save-match-data
      (goto-char (point-max))
      (forward-line 0)
      (when (search-forward "> " nil t)
        (gpb-r-message "Found R prompt")

        ;; Remove this function.
        (remove-hook 'comint-output-filter-functions
                     #'gpb-inferior-r-mode-2 t)

        ;; Low level command processing.
        (add-hook 'comint-preoutput-filter-functions
                  #'gpb-r-preoutput-filter nil t)

        ;; Look for debug breakpoints and show the corresponding buffer
        ;; location.
        (add-hook 'comint-output-filter-functions
                  #'gpb-r-debug-filter-function nil t)

        ;; Make filenames clickable buttons.
        (add-hook 'comint-output-filter-functions
                  #'gpb-r-add-buttons-filter nil t)

        ;; The preoutput filter `gpb-r-preoutput-filter' will pick up this
        ;; output and pass it on to `gpb-inferior-r-mode-3'
        (send-string
         (get-buffer-process (current-buffer))
         (format "cat(sprintf('%s%%s\\n', tempdir()))\n"
                 gpb-r-tmpdir-marker))))))


(defun gpb-inferior-r-mode-3 (buf r-tmp-dir)
  "Complete the initialization of the R process in buffer BUF.

Ensures that the R process sources the init file gpr-r-mode.R.  If the R
process is remote, we first copy this file to `r-tmp-dir' on the remote
system."
  (save-match-data
    ;; (gpb-r-message "gpb-inferior-r-mode-3: %S %S" buf r-tmp-dir)
    (with-current-buffer buf
      (setq gpb-r-proc-tmp-dir (file-name-as-directory
                                (concat (file-remote-p default-directory)
                                        r-tmp-dir))
            gpb-r-region-file (concat gpb-r-proc-tmp-dir "emacs-region.R"))

      (let* ((local-init-script (locate-library "gpb-r-mode.R"))
             (remote-init-script (expand-file-name "copy-of-gpb-r-mode.R"
                                                   gpb-r-proc-tmp-dir))
             r-local-path cmd)

        (cond
         ;; If we are running R on a remote machine over TRAMP, we write our R
         ;; init script into the remote temp dir.
         ((file-remote-p gpb-r-proc-tmp-dir)
          (let ((coding-system-for-write 'us-ascii-unix))
            (with-temp-file remote-init-script
              (insert-file-contents local-init-script)))
          (setq r-local-path (file-local-name remote-init-script)))

         ;; Otherwise, we can just source the file directly.
         (t
          (setq r-local-path local-init-script)))

        ;; Now source `local-path'
        (setq cmd (format "source(%S)\n" r-local-path))
        (gpb-r-message "R Init command: %s" (string-trim cmd))
        (gpb-r-send-command cmd buf nil 'wait)))))


(defun gpb-r-set-active-process ()
  (interactive)
  (setq gpb-r-active-process-buffer (current-buffer))
  (message "Active R process buffer: %S" gpb-r-active-process-buffer))


(defvar-local gpb-r-auto-follow nil
  "If non-nil, we automatically trigger buttons when we tab to them")

(defun gpb-r-toggle-auto-follow ()
  (interactive)
  (cond
   (gpb-r-auto-follow
    (setq gpb-r-auto-follow nil)
    (message "Disabled auto-follow mode"))
   (t
    (setq gpb-r-auto-follow t)
    (message "Enabled auto-follow mode"))))

(defun gpb-r-tab-command ()
  (interactive)
  (setq this-command (if (comint-after-pmark-p)
                         'completion-at-point
                       'gpb-r-forward-button))
  (call-interactively this-command))

(defun gpb-r-forward-button ()
  (interactive)
  (cond
   ((forward-button 1 nil t t)
    (when gpb-r-auto-follow (push-button)))
   (t
    (goto-char (point-max)))))

(defun gpb-r-backward-button ()
  (interactive)
  (cond
   ((forward-button -1 nil t t)
    (when gpb-r-auto-follow (push-button)))
   (t
    (goto-char (point-min)))))

(defvar-local gpb-r-save-and-exec-command--command nil
  "The R command `gpb-r-save-and-exec-command' should use.")

(defun gpb-r-save-and-exec-command (arg)
  "Save the current buffer and then source, reload, or render it.

With a prefix argument, we source with chdir = TRUE or update the
Rmarkdown render expression."
  (interactive "P")
  (let* ((filename (or (buffer-file-name)
                       (error "Buffer is not visiting a file")))
         (localname (file-local-name filename))
         (r-proc-buf (or (gpb-r-get-proc-buffer)
                         (error "No R process is associated with buffer"))))

    (save-buffer)

    ;; If this command does not have an argument and the buffer local
    ;; variable `gpb-r-save-and-exec-command--command' is set from a
    ;; previous call, we use that.
    (if (and (null arg) gpb-r-save-and-exec-command--command)
        (setq cmd gpb-r-save-and-exec-command--command)

      ;; Otherwise we query the user with a reasonable default.
      (setq cmd (gpb-r-read-expression
                 "R command: " (gpb-r-suggest-exec-cmd localname arg))))

    ;; Save the command in the buffer so you can reuse it.
    (setq-local gpb-r-save-and-exec-command--command cmd)

    (with-current-buffer r-proc-buf
      (goto-char (point-max))
      (comint-delete-input)
      (insert cmd)
      (comint-send-input))

    (display-buffer r-proc-buf)))


(defun gpb-r-suggest-exec-cmd (localname &optional arg)
  ;; The `-file' suffix means no trailing slash.
  (let* ((dir (file-name-directory localname))
         (dir-file (ignore-errors (directory-file-name dir)))
         (parent (ignore-errors (file-name-directory dir-file)))
         (parent-file (ignore-errors (directory-file-name parent))))

    (cond
     ;; We render an R markdown document.
     ((string-suffix-p ".Rmd" localname t)
      (let ((extra (if arg ", params = list()" "")))
        (format "render('%s'%s)" localname extra)))

     ;; If we are in a package, load the package.
     ;; Get the name of the directory that contains filename.
     ((and parent-file
           (string-equal (ignore-errors (file-name-base dir-file)) "R"))
      (format "pkgload::load_all('%s')" parent-file))

     ;; Otherwise, source the file.
     (t
      (let ((extra-args (if arg ", chdir = TRUE" "")))
        (format "source('%s'%s)" localname extra-args))))))



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
                   (and (string-match "^\\[\\(.+\\)\\]$" place)
                        (get-buffer (match-string 1 place)))

                   (let ((filename (gpb-r-expand-filename place)) dir)
                     (or
                      (and (file-exists-p filename)
                           (find-file-noselect filename))
                      (and (setq dir (read-directory-name
                                      (format "Find %s: "
                                              (file-name-nondirectory
                                               filename))
                                      (get-text-property
                                       0 'current-workking-dir filename)))
                           (find-file-noselect
                            (concat dir (file-name-nondirectory filename))))))))
             place))

  (when (and buf (buffer-live-p buf))
    (let* ((window (display-buffer buf 'other-window))
           (vertical-margin (and window (/ (window-height window) 4)))
           (face (or face 'next-error))
           (inhibit-message t))
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
    (when pop-to (select-window window)))

  (message "%s#%s" place line-number))


(defun gpb-r-get-proc-buffer ()
  "Get the currently active R process buffer"
  ;; (message "gpb-r-get-proc-buffer: %S %S"
  ;;          (current-buffer) gpb-r-active-process-buffer)
  (cond
   ((derived-mode-p #'gpb-inferior-r-mode)
    (current-buffer))

   ((and gpb-r-active-process-buffer
         (buffer-live-p gpb-r-active-process-buffer)
         (process-live-p (get-buffer-process gpb-r-active-process-buffer)))
    gpb-r-active-process-buffer)

   (t
    (let* ((choices (mapcar #'buffer-name (gpb-r-all-live-interpreters)))
           (buf-name (completing-read "R buffer: " choices nil t
                                      (when (= (length choices) 1)
                                        (car choices))))
           (buf (get-buffer buf-name)))
      (setq gpb-r-active-process-buffer buf)
      buf))))


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

          (gpb-r-message "Wrote %s" region-filename)
          (setq line (format ".gpb_r_mode$eval_region_file(%S)" srcbuf))))))
  line)


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

    (when pop (display-buffer procbuf))
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
  (let* ((buf-name (concat (buffer-name (get-buffer process-buffer))
                           " [staging]"))
         (existing-buf (get-buffer buf-name)))
    (cond
     (existing-buf existing-buf)
     (ensure (let ((buf (get-buffer-create buf-name)))
               (with-current-buffer buf (special-mode))
               buf)))))


(defun gpb-r-kill-all-inferior-buffers ()
  "Kill all inferior R process buffers."
  (interactive)
  (if (null gpb-r-all-inferior-buffers)
      (message "There are no inferior R buffers")
    (let* ((buffers (reverse gpb-r-all-inferior-buffers))
           (msg (format "Killed %s" (mapconcat #'buffer-name buffers ", ")))
           ;; Don't query; just kill.
           kill-buffer-query-functions)
      (dolist (buf buffers)
        (and buf (buffer-live-p buf) (kill-buffer buf)))
      (setq gpb-r-all-inferior-buffers nil)
      (message "%S" msg))))

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
  (message "Loaded %s." comint-input-ring-file-name))


(defun gpb-r-save-history ()
  (interactive)
  (comint-write-input-ring)
  (message "Wrote %s." comint-input-ring-file-name))


;;
;; Preoutput filter functions
;;
;;
;; There are various low-level output markers that allow the R process to
;; communicate with Emacs.  These all start with `gpb-r-guid'.  We strip
;; these from the output and handle them in out preoutput filter.
;;

(defvar gpb-r-guid "7b530f72-85a0-483c-98ce-d24414394ff0"
  "A string that is unlikely to appear at the start of a line of output.")

(defvar gpb-r-prompt (format "%s:PROMPT" gpb-r-guid))

(defvar gpb-r-tmpdir-marker (format "%s:R_TEMP_DIR:" gpb-r-guid)
  "Used by `gpb-inferior-r-mode-2' to get R's temp directory.")

(defvar gpb-r-chdir-marker (format "%s:CHDIR:" gpb-r-guid)
  "Used by R to notify Emacs of changes in the R working directory.")

(defvar gpb-r-output-marker (format "%s:OUTPUT:" gpb-r-guid)
  "Used by `gpb-r-send-command' to mark the start of output.")

(defvar gpb-r-output-regex (format "%s:OUTPUT:.*\n" gpb-r-guid)
  "Used by `gpb-r-send-command' to mark the start of output.
This may not be at the start of the line.")

(defvar gpb-r-flush-marker (format "%s:FLUSH\n" gpb-r-guid)
  "Used by `gpb-r-cancel-commands' to flush the staging buffer.")

(defvar-local gpb-r-pending-commands nil
  "Defined in inferior R buffers.  List of (list callback args) pairs.")

(defvar gpb-r-command-timeout 2
  "Default R command timeout in seconds for `gpb-r-send-command'")

(defvar-local gpb-r-command-timeout-timer nil
  "Timer used to deal with blocking R commands.

This timer is set when by `gpb-r-send-command' when a command starts and
cancelled by `gpb-r-send-command' when we see the next prompt.  If triggered,
it shows a warning that the command seems to be blocking.

Each inferior R process has its own timer.")

(defvar-local gpb-r-proc-tmp-dir nil
  "Defined in each R inferior process buffer.
The remote R process's temp directory. Includes a TRAMP prefix for remote
files.")

(defvar-local gpb-r-region-file nil
  "Defined in each R inferior process buffer.
File used to pass code snippets to the R process. Each R process uses a
different region file. Includes a TRAMP prefix for remote files.")


(defun gpb-r-might-be-looking-at-guid-p (&optional pos buf)
  "Could the partial line starting at POS in BUF match `gpb-r-guid'?"
  (let* ((pos (or pos (point)))
         (buf (or buf (current-buffer)))
         (line (buffer-substring-no-properties
                pos (min (+ pos (length gpb-r-guid))
                         (point-max)))))
    (string-prefix-p line gpb-r-guid
                     (with-current-buffer buf
                       (save-excursion
                         (goto-char pos))))))


(defun gpb-r-flush-staging-buffer (&optional buf)
  (interactive)
  (let* ((buf (or buf (gpb-r-get-proc-buffer)))
         (proc (or (get-buffer-process buf)
                   (error "No R process available")))
         (f (process-filter proc)))
    (with-current-buffer buf
     (funcall f proc gpb-r-flush-marker))))


(defun gpb-r-preoutput-filter (string)
  "Preprocess R process output.

Called by `comint' in an R inferior process buffer.  STRING gives new
output from the R process.  This function is called before output is
inserted in the `comint' buffer, so we can use it to intercept output
before it gets to the R process buffer.

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
  (setq buf (get-buffer buf))
  ;; (message "gpb-r-preoutput-filter-1 string in %S\n%s" string string)
  (with-current-buffer buf
    (cl-assert (derived-mode-p 'gpb-inferior-r-mode)))

  (if (or (null string) (= (length string) 0))
      "" ; `comint-output-filter' doesn't like it if you return nil.

    (let ((staging-buf (with-current-buffer buf
                         ;; If we create the buffer, we want it to pick up
                         ;; `default-directory' from `buf'.
                         (gpb-r-get-staging-buffer buf 'ensure)))
          (tmpdir-regex (format "%s\\(.*\\)\n>? *" gpb-r-tmpdir-marker))
          (prompt-regex (format "\\(%s\\)\\|\\(%s\\)"
                                gpb-r-prompt "Browse[[0-9]+]> "))
          (chdir-regex (format "%s\\(.*\\)\n" gpb-r-chdir-marker))
          (output-regex (format "%s\\(.*\\)\n" gpb-r-output-marker))
          (inhibit-read-only t)
          first-new-line first-line-contains-cr
          ;; This are set to increasing buffer positions below.
          previous-output-end command-output-start command-output-end prompt-end)

      ;; (gpb-r-dump-buffer "gpb-r-preoutput-filter before")

      (with-current-buffer staging-buf
        ;; We use `default-directory' to track the working directory of the
        ;; inferior R process.
        (cl-assert default-directory)
        (goto-char (point-max))
        (setq first-new-line (copy-marker (pos-bol)))
        (insert string)

        ;; Is there a carraige return in the first line?
        (setq first-line-contains-cr (save-excursion
                                       (goto-char (point-min))
                                       (search-forward "\r" (pos-eol) t)))

        ;; Process terminal control characters
        (save-excursion (comint-carriage-motion (point-min) (point-max)))
        (save-excursion (ansi-color-apply-on-region first-new-line (point-max)))

        ;; If there was a carraige return on the first line, we should
        ;; overwrite the current output line in the R inferior process
        ;; buffer so we insert an initial carraige return.
        (when first-line-contains-cr
          (goto-char (point-min))
          (insert "\r"))

        ;; (gpb-r-dump-buffer "gpb-r-preoutput-filter insert")

        ;; Look for `gpb-r-tmpdir-marker'
        ;;
        ;; We request `tempdir()` from the R process and pass it to
        ;; `gpb-inferior-r-mode-3'.
        (goto-char first-new-line)
        (when (re-search-forward tmpdir-regex nil t)
          ;; Sometimes the R process writes the R_TEMP_DIR output twice.
          ;; Maybe this is related to terminal echo settings?  We only want
          ;; to call `gpb-inferior-r-mode-3' once.
          (run-at-time 0.1 nil #'gpb-inferior-r-mode-3 buf (match-string 1))
          (delete-region (match-beginning 0) (match-end 0)))

        ;; Look for `gpb-r-chdir-marker'
        ;;
        ;; Add `current-working-dir' text properties that give the R
        ;; processes working directory at the time of output.
        (goto-char first-new-line)
        (while (re-search-forward chdir-regex nil t)
          (put-text-property first-new-line (match-beginning 0)
                             'current-working-dir
                             default-directory)
          (setq default-directory (gpb-r-expand-filename
                                   (string-trim (match-string 1))))
          (gpb-r-message "Working dir in %s changed to %s"
                         buf default-directory)
          ;; Delete the CHDIR marker
          (delete-region (match-beginning 0) (match-end 0)))

        (unless (eobp)
          (put-text-property (point) (point-max)
                             'current-working-dir
                             default-directory))

        ;; (gpb-r-dump-buffer "gpb-r-preoutput-filter clean"))

        (goto-char (point-min))
        (setq command-output-start (re-search-forward gpb-r-output-regex nil t)

              ;; When there is a command pending, there may be R process
              ;; output from before the command started.  We return such
              ;; output immediately.
              previous-output-end (and command-output-start
                                       (match-beginning 0))

              ;; If we see a prompt after the start of a command, we know
              ;; the command is complete.
              prompt-end (and command-output-start
                              (re-search-forward prompt-regex nil t))

              command-output-end (and prompt-end
                                      (match-beginning 0)))

        (cond

         ;; If we see a flush command, we stop waiting on the current
         ;; command and dump all pending output to the R inferior buffer.
         ((save-excursion
            (goto-char (point-min))
            (search-forward gpb-r-flush-marker nil t))

          (gpb-r-message "Flushing R output in %S" buf)
          ;; Delete the FLUSH command.
          (delete-region (match-beginning 0) (match-end 0))
          ;; And improve the appearance of any hung pending commands.
          (save-excursion
            (goto-char (point-min))
            (while (search-forward gpb-r-output-marker nil t)
              (replace-match "# Timeout during: ")))
          (with-current-buffer buf
            (setq-local gpb-r-pending-commands nil
                        gpb-r-command-timeout-timer nil))
          (gpb-r-cut-region (point-min) (point-max)))

         ;; If we see a command with its terminating prompt, we can process
         ;; the result.
         (prompt-end
          (let* ((callback-args (with-current-buffer buf
                                  (pop gpb-r-pending-commands)))
                 (callback (car callback-args))
                 (args (cadr callback-args))
                 (previous-output (buffer-substring (point-min)
                                                    previous-output-end))
                 (command-output (buffer-substring command-output-start
                                                   command-output-end))
                 (next-output (buffer-substring prompt-end (point-max))))

            ;; Don't run `callback' immediately so we don't have to worry
            ;; about it erroring out or changing state in the filter
            ;; function.
            (if callback
                (apply #'run-at-time 0 nil callback buf command-output t args)
              (message "Error: callback is nil in `gpb-r-preoutput-filter-1'"))

            (with-current-buffer buf
              (when gpb-r-command-timeout-timer
                (cancel-timer gpb-r-command-timeout-timer)
                (setq gpb-r-command-timeout-timer nil)))

            ;; `next-output' might contain another command response, so we
            ;; need to recurse to handle that case.
            (erase-buffer)
            (concat previous-output (gpb-r-preoutput-filter-1 buf next-output))))

         ;; We are in the middle of a command that is not complete.
         (command-output-start
          (let* ((callback-args (with-current-buffer buf
                                  (car gpb-r-pending-commands)))
                 (callback (car callback-args))
                 (args (cadr callback-args))
                 (command-output (buffer-substring command-output-start
                                                   (point-max))))
            (apply #'run-at-time 0 nil callback buf command-output nil args)

            ;; Return any output prior to the start of the current command.
            (gpb-r-cut-region (point-min) previous-output-end)))

         (t
          ;; Replace prompt hashes with standard prompts.
          (save-excursion
            (goto-char (point-min))
            (while (search-forward gpb-r-prompt nil t)
              (replace-match "> ")))

          ;; We only return the final line once we know it doesn't match
          ;; `gpb-r-guid'.  We do this to avoid missing messages from R if
          ;; they are delivered across multiple to this function.
          (goto-char (point-max))
          (forward-line 0)
          (unless (gpb-r-might-be-looking-at-guid-p)
            (goto-char (point-max)))

          (gpb-r-cut-region (point-min) (point))))))))


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
            ;; Don't call `gpb-r-show-line' in the filter.
            (run-at-time 0 nil #'gpb-r-show-line filename line-number)))))))


(defun gpb-r-add-buttons-filter (output)
  (when (and output (> (length output) 0))
    (save-match-data
      (save-excursion
        (let* ((proc (get-buffer-process (current-buffer))))
          (unless (with-local-quit
                    (gpb-r-add-buttons-filter-1 comint-last-output-start
                                                (process-mark proc))
                    t)
            (message "Local quit in `gpb-r-add-buttons-filter'.")))))))


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
            ;; We need to ensure we always move strictly forward or we
            ;; could get stuck in an infinite loop.
            (save-excursion
              (let* ((file (buffer-substring (match-beginning file-subexp)
                                             (match-end file-subexp)))
                     (line (ignore-errors
                             (string-to-number (buffer-substring
                                                (match-beginning line-subexp)
                                                (match-end line-subexp)))))
                     (beg (match-beginning link-subexp))
                     (end (match-end link-subexp)))

                ;; Hide the file directory to save space.
                (goto-char (match-end link-subexp))
                (when (re-search-backward "/" beg t)
                  (put-text-property beg (match-end 0) 'display "")
                  (setq beg (match-end 0)))

                (make-text-button
                 beg end
                 'file file
                 'line line
                 'action #'gpb-r-follow-link
                 ;; Abbreviate the name in the buffer, but show the full path
                 ;; in the echo area when you tab to the button.
                 ;; 'display (format "%s#%s" (file-name-nondirectory file) line)
                 'help-echo (format "%s#%s" file line))))))))))


(defun gpb-r-follow-link (&optional button)
  (let* ((button (or button (button-at (point))))
         (file (button-get button 'file))
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


(defun gpb-r-send-command (cmd &optional buf callback timeout &rest args)
  "Send CMD to R process in BUF the return the output as a string.

If BUF is omitted, we use the current buffer.  We move the
process into new buffer, send `cmd' to the R process, wait for
the result, and return a buffer that contains the result.

Echoes `gpb-r-output-marker' from the R process.  This tells
`gpb-r-preoutput-filter' to steal all the lines of output until the next
`gpb-r-prompt' and send them to the first function in the list
`gpb-r-pending-commands'.

If CALLBACK is `nil', this function blocks until we have a response from R.

Otherwise, CALLBACK is called with the arguments: BUF, OUTPUT, COMPLETE
plus any addition arguments in ARGS.  BUF is the buffer
containing the inferior R process.  OUTPUT is a string containing all
output collected from the command.  COMPLETE is t if the command is
complete.  CALLBACK will be called exactly once with COMPLETE true and most
CALLBACKS can ignore any previous calls.

Passing a callback is much preferred to blocking when possible.  If you
don't care about the result, pass `ignore' as CALLBACK.

TIMEOUT defaults to `gpb-r-command-timeout-timer'.  If the command takes
longer than this we send all pending output to the inferior R buffer and
stop waiting for the command to complete.  Pass 'wait as TIMEOUT to wait as
long as it takes.

See `gpb-r-show-docs-1' for an example of a CALLBACK that checks if a
command is blocking because it needs user input."
  (interactive "sR Command: ")
  (let* ((buf (or buf (gpb-r-get-proc-buffer)))
         (staging-buf (and buf (gpb-r-get-staging-buffer buf 'ensure)))
         ;; We send a string and parse it on the R side.
         ;; Are there any differences between R and Emacs quoting?
         (wrapped-cmd (format "cat(%S); eval(parse(text = %S))\n"
                              (format "%s%s\n" gpb-r-output-marker cmd) cmd))
         (proc (or (get-buffer-process buf)
                   (error "No R process available")))
         (msg "Error: Timeout during R command")
         (timeout (or timeout gpb-r-command-timeout)))

    ;; `gpb-r-pending-commands' is local to `buf'
    (with-current-buffer buf
      ;; We flush output back into the inferior R process buffer and cancel
      ;; any pending commands if the process hangs longer than
      ;; `timeout' seconds.
      (unless (eq timeout 'wait)
        (gpb-r-message "Setting timeout: %S %S" buf timeout)
        (setq gpb-r-command-timeout-timer
              (run-at-time timeout nil
                           `(lambda ()
                              (with-current-buffer ,buf
                                (gpb-r-flush-staging-buffer)
                                (cond
                                 ((eq gpb-r-send-command--response 'waiting)
                                  (setq gpb-r-send-command--response 'cancelled)
                                  (signal 'quit nil))
                                 (t
                                  (message ,msg))))))))

      (if callback
          (progn
            ;; Add `callback' to the back of the list.
            (setq gpb-r-pending-commands (nconc gpb-r-pending-commands
                                                (list (list callback args))))
            (send-string proc wrapped-cmd))

        (and args (error "ARGS requires CALLBACK"))
        (setq gpb-r-send-command--response 'waiting
              callback (lambda (buf txt complete)
                         (when complete
                           ;; (message "callback: %S" txt)
                           (setq gpb-r-send-command--response txt)))
              gpb-r-pending-commands (nconc gpb-r-pending-commands
                                            (list (list callback nil))))

        ;; Send the string and wait for a response.
        (send-string proc wrapped-cmd)
        (condition-case error-var
            (progn
              ;; Accept output until the callback above is triggered.
              (while (eq gpb-r-send-command--response 'waiting)
                ;; Filters and timers run as we wait for more output.
                (with-local-quit
                  (accept-process-output proc)))

              ;; (message "gpb-r-send-command--response: %S"
              ;;          gpb-r-send-command--response)

              ;; Cancel the timer if it is still pending.
              (when gpb-r-command-timeout-timer
                (cancel-timer gpb-r-command-timeout-timer))

              (let ((value gpb-r-send-command--response))
                (setq gpb-r-send-command--response nil)
                value))

          ;; If the call hangs and the user has to `keyboard-quit' to get
          ;; control of Emacs, show the staging buffer to help debug.
          ('quit
           (error "`gpb-r-send-command' failed")))))))


(defun gpb-r-show-docs (object-name &optional buf)
  "Show help on OBJECT-NAME."
  (interactive
   (list (gpb-r-read-object "Show Docs: " (gpb-r-object-at-point))))
  (let* ((buf (or buf (gpb-r-get-proc-buffer)))
         (cmd (format "?%s" object-name)))
    (gpb-r-send-command cmd buf #'gpb-r-show-docs-1 nil object-name)))

(defun gpb-r-show-docs-1 (buf txt complete objname)
  ;; (message "gpb-r-show-docs-1: %S %S" buf txt)
  (cond
   (complete
    (let ((buf (get-buffer-create (format "*R Help: %s*" objname)))
          (inhibit-read-only t))
      (with-current-buffer buf
        (erase-buffer)
        (special-mode)
        (insert txt)
        (comint-carriage-motion (point-min) (point-max))
        (ansi-color-apply-on-region (point-min) (point-max))
        (goto-char (point-min))
        (save-excursion
          (when (re-search-forward "^Selection: " nil t)
           (delete-region (point-min) (match-end 0)))))
      (pop-to-buffer buf)))

   ;; Output is not complete, but we might be stuck waiting for user input.
   (t
    (when (string-match-p "Selection: \\'" txt)
      (with-temp-buffer
        (insert txt)
        (goto-char (point-min))
        (rename-buffer "*Choose one*")
        (save-excursion
          (re-search-forward "^Selection: ")
          (delete-region (match-beginning 0) (match-end 0)))

        ;; Show the choices in the current window.
        (set-window-buffer (selected-window) (current-buffer))

        ;; Cancel the timeout timer.
        (with-current-buffer buf
          (when gpb-r-command-timeout-timer
            (cancel-timer gpb-r-command-timeout-timer)
            (setq-local gpb-r-command-timeout-timer nil)))

        (let ((proc (get-buffer-process buf))
              (number (read-number "Selection: " 1)))
          (process-send-string proc (format "%i\n" number))))))))


(defun gpb-r-sync-working-dir (&optional buf)
  "Ensure that `default-director' reflects the R working directory."
  (interactive)
  (let* ((buf (or buf (gpb-r-get-proc-buffer))))
    (gpb-r-send-command ".gpb_r_mode$sync_working_dir()"
                        buf 'gpb-r-sync-working-dir-1)))

(defun gpb-r-sync-working-dir-1 (buf txt complete)
  (when complete
    (with-temp-buffer
      (insert txt)
      (goto-char (point-min))
      (re-search-forward "\\(Working dir:.*\\)\n")
      (message "%s" (match-string 1)))))

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
           (bol (line-beginning-position))
           (line (buffer-substring-no-properties bol (point)))
           (completion-info (gpb-r-get-completions line buf)))
      ;; (message "completion-info: %S" completion-info)
      (list (+ bol (plist-get completion-info :beg))
            (+ bol (plist-get completion-info :end))
            (plist-get completion-info :completions)))))

(defun gpb-r-minibuffer-complete (line)
  (let* ((completion-info (gpb-r-get-completions line))
         (prefix (substring line 0 (plist-get completion-info :beg)))
         (completions (plist-get completion-info :completions)))
    ;; (gpb-r-message "completion-info: %S" completion-info)
    ;; (gpb-r-message "completions: %S" completions)
    (mapcar (lambda (suffix) (concat prefix suffix)) completions)))

(defvar gpb-r-read-object--history nil)

(defun gpb-r-read-object (prompt &optional initial-input)
  (interactive)
  (let ((keymap (copy-keymap minibuffer-local-completion-map)))
    (define-key keymap (kbd "SPC") 'self-insert-command)
    (let ((minibuffer-local-completion-map keymap))
      (completing-read prompt
                       (completion-table-dynamic #'gpb-r-minibuffer-complete t)
                       nil
                       nil
                       initial-input
                       'gpb-r-read-object--history))))

(defun gpb-r-read-expression (prompt &optional initial-input)
  "Like `gpb-r-read-object' but allows spaces"
  (interactive)
  (let ((keymap (copy-keymap minibuffer-local-completion-map)))
    (define-key keymap (kbd "SPC") 'self-insert-command)
    (let ((minibuffer-local-completion-map keymap))
      (completing-read prompt
                       (completion-table-dynamic #'gpb-r-minibuffer-complete t)
                       nil
                       nil
                       initial-input
                       'gpb-r-read-object--history))))


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

(defun gpb-r-message (format-string &rest args)
  "Write message to *Messages* but don't echo in minibuffer."
  (let ((inhibit-message t))
    (apply 'message format-string args)))

(defun gpb-r-all-live-interpreters ()
  (cl-remove-if-not (lambda (buf)
                      (with-current-buffer buf
                        (and
                         (derived-mode-p #'gpb-inferior-r-mode)
                         (process-live-p (get-buffer-process buf)))))
                    (buffer-list)))

;; Testing
;;
;; In inferior R buffer:
;;
;; (gpb-r-send-command "print(1:10)" "*R*" nil)

(defun gpb-r-test-command (&optional buf)
  "A simple command testing."
  (interactive)
  (let* ((buf (or buf (gpb-r-get-proc-buffer)))
         (response (string-trim
                    (substring-no-properties
                     (gpb-r-send-command "print(1:10)\n" buf nil 10)))))
    (message "Response: %S" response)))

(defun gpb-r-hanging-command (&optional buf)
  "A command that hangs for testing."
  (interactive)
  (let* ((buf (or buf (gpb-r-get-proc-buffer))))
    (gpb-r-send-command "readline('Press <return> to continue: ')" buf)))

(defun gpb-r-dump-buffer (&optional label)
  (save-restriction
    (widen)
    (let* ((buf (current-buffer))
           (label (if label (format " at %s" label) ""))
           (contents (with-current-buffer buf
                       (buffer-substring (point-min) (point-max)))))
      (gpb-r-message "Contents of %S%s: %S\n%s" buf label contents contents))))


(provide 'gpb-r-mode)
