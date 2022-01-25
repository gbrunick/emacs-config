;; Provides an alternative inferior R process mode.

(defgroup gpb-r-mode nil
  "Settings related to the Emacs 'gpb-r-mode' package/feature.")

(defcustom gpb-r-x-display
  (format "%s:0" (format-network-address
                  (car (network-interface-info "eth0")) t))
  "The DISPLAY environment variable to use on the servers."
  :type 'string
  :group 'gpb-r-mode)

(defcustom gpb-r-default-R-executables nil
  "Default R executables by directory.

The value is a alist where the keys are paths and the values are
paths to R executables (or executable wrappers).  If no default
is provided, we just use the first R that is found on PATH."
  :type '(alist :key-type (string :tag "Directory")
                :value-type (string :tag "R Executable"))
  :group 'trp-tools)


(defcustom gpb:ess-inferior-compilation-error-regexp-alist
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


(defvar gpb-inferior-r-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'gpb-rmode--send-or-copy-input)
    (define-key map "\C-n" 'comint-next-input)
    (define-key map "\C-p" 'gpb-comint:previous-input)
    (define-key map "\C-ct" 'gpb:ess-send-traceback-command)
    (define-key map "\C-cq" 'gpb:ess-send-quit-command)
    (define-key map [?\t] 'gpb-r-mode---tab-command)
    (define-key map [(backtab)] 'gpb:inferior-ess-previous-traceback)
    (define-key map "\C-co" 'gpb:ess-view-data-frame)

    ;; (when (require 'gpb-text-objects nil t)
    ;;   (define-key map "g" 'gpb:ess-goto-line))

    (define-key map "\C-c\C-v" 'gpb-ess:show-help)

    map)
  "Local keymap `gpb-inferior-r-mode'.")


(define-derived-mode gpb-inferior-r-mode comint-mode "Inferior R Mode"
  "Major mode for an inferior R process.
\\<gpb-inferior-r-mode-map>
\\{gpb-inferior-r-mode-map}"
  (setq truncate-lines t)
  (setq-local comint-input-filter 'gpb:ess-comint-input-filter)
  (setq-local completion-at-point-functions '(gpb-r-mode--completion-at-point))
  (dolist (face '(underline compilation-error compilation-line-number))
    (face-remap-add-relative face :foreground "royal blue" :weight 'normal))
  (add-hook 'comint-output-filter-functions
            #'gpb:ess-debug-track-comint-output-filter-function nil t))


(defun run-R2 (&optional working-dir)
  (interactive (list (read-directory-name "Working Dir: " nil nil t)))
  (let* ((working-dir (or working-dir default-directory))
         (buf-name (format "*R:%s*" (file-name-base
                                     (directory-file-name working-dir))))
         (buf (generate-new-buffer buf-name))
         ;; Set up the DISPLAY environment variable.  This only matter
         ;; if we are running remotely, but it costs very little to set
         ;; it up either way.  We add this entry last in the hopes that
         ;; this would override any prior entry.
         (tramp-remote-process-environment
          (append tramp-remote-process-environment
                  (list (format "DISPLAY=%s" gpb-r-x-display))))
         proc)
    (with-current-buffer buf
      ;; `comint-exec-1' uses `default-directory', even if
      ;; `make-comint-in-buffer' is given a fully qualified path.
      (setq default-directory working-dir)
      (goto-char (point-min))
      (insert (format "Working Dir: %s" default-directory))
      (make-comint-in-buffer
       "R" buf ;; "/plinkx:AWS-beta:/nfs/home/trpgh47/bin/trp-r-3.6.sh"
       "/export/appl/ifos/R/R-3.6.0/bin/R")
      (gpb-inferior-r-mode)
      (setq proc (get-buffer-process buf)
            comint-process-echoes nil)
      (process-send-string proc "options(menu.graphics = FALSE)\n")
      (process-send-string proc "options(gpb.r.mode = TRUE)\n")
      (compilation-shell-minor-mode 1)
      (setq-local compilation-error-regexp-alist
                  gpb:ess-inferior-compilation-error-regexp-alist))
    (pop-to-buffer buf)))

(defun gpb-r--start-bash ()
  (let* ((buf (generate-new-buffer "*TEST*"))
         proc)
    (with-current-buffer buf
      (setq default-directory "/plinkx:AWS-beta:/nfs/home/trpgh47/bin/")
      (setq proc (start-file-process "test" (current-buffer) "bash"))
      (accept-process-output proc)
      (process-send-string proc "echo test\n"))
    (pop-to-buffer buf)))

(defun gpb-r--start-bash2 ()
  (let* ((buf (generate-new-buffer "*TEST*"))
         proc)
    (with-current-buffer buf
      (setq default-directory "/plinkx:AWS-beta:/nfs/home/trpgh47/bin/")
      (setq proc (start-file-process "test" (current-buffer) "/nfs/home/trpgh47/bin/test-bash.sh"))
      (accept-process-output proc)
      (process-send-string proc "stty --all\n"))
    (pop-to-buffer buf)))


(defun gpb-r--start-bash3 ()
  (let* ((buf (generate-new-buffer "*TEST*"))
         proc)
    (with-current-buffer buf
      (setq default-directory "/plinkx:AWS-beta:/nfs/home/trpgh47/bin/")
      (setq proc (start-file-process "test" (current-buffer) "/nfs/home/trpgh47/bin/trp-r-3.6.sh"))
      (accept-process-output proc 1)
      (accept-process-output proc 1)
      (accept-process-output proc 1)
      (process-send-string proc "x <- 10\n"))
    (pop-to-buffer buf)))


(defun gpb-r-mode---tab-command ()
  (interactive)
  (setq this-command (if (comint-after-pmark-p)
                         'completion-at-point
                       'gpb:inferior-ess-next-traceback))
  (call-interactively this-command))

(defun gpb-r-mode--completion-at-point ()
  (let* ((proc (get-buffer-process (current-buffer)))
         (line (buffer-substring-no-properties (process-mark proc) (point)))
         (cmd (format ".gpb_r_mode_get_completions(%s, %S)"
                      (marker-position (process-mark proc)) line))
         args)
    (with-current-buffer (gpb:ess-send-command cmd)
      (setq args (read (current-buffer))))
    (and args (third args)) args))

;; (message "Args: %S" args)
;; (completion-in-region (first args) (second args) (third args)))))


(defun gpb-rmode--send-or-copy-input ()
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

(provide 'gpb-r-mode)
