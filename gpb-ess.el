;;
;;  Customizaiton related to the ESS package
;;
;;  See: https://ess.r-project.org/
;;

(require 'cl-lib)
(require 'ffap)

;; For `string-trim'.
(require 'subr-x)


(defvar gpb:ess-region-file-cache nil
  "An alist mapping from TRAMP remotes to region files.

Each entry has a key corresponding to the TRAMP prefix string as
returned by `file-remote-p' and value that is list containing two
filenames for temporary files that are used by
`gpb:ess-eval-region' to allow for evaluation of regions of
code.")

(add-hook 'R-mode-hook 'gpb:R-mode-hook)
(add-hook 'inferior-ess-mode-hook 'gpb:inferior-ess-mode-hook)


(defvar gpb:ess-last-eval-region nil
  "The last region that was sent to the interpreter.
Contains a cons of two markers.")

(setq ess-use-auto-complete nil)

(defun gpb:R-mode-hook ()
  ;; Get rid of the annoying "smart underscore" behaviour.
  (local-set-key "_" 'self-insert-command)

  (local-set-key "\C-cb" 'gpb:ess-insert-browser)
  (local-set-key "\C-cq" 'gpb:ess-send-quit-command)
  (setq-local ess-indent-with-fancy-comments nil)
  ;; The Data section of the imenu entries is pretty useless.
  (adelete 'ess-imenu-S-generic-expression "Data")
  ;; Only match top-level function definitions.
  (setcdr (assoc "Functions" ess-imenu-S-generic-expression)
          '("^\\([^ \t]+\\)[ 	\n]*<-[ 	\n]*function[ ]*(" 1))
  (adelete 'ess-imenu-S-generic-expression "Data")

  (gpb:ess-sniff-out-two-space-indentation)

  ;; Use etags rather than ESS's custom xref implementation.
  (xref-etags-mode 1)
  ;; (add-hook 'xref-backend-functions #'etags--xref-backend t)

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

  ;; Get rid of the annoying "smart underscore" behaviour.
  (local-set-key "_" 'self-insert-command)

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
        (comint-add-to-input-history (concat r-object "?")))

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
  (cl-letf (((symbol-function 'ess-start-process-specific)
             (lambda (&rest) (error (concat "No interpreter process is "
                                            "associated with this buffer.")))))
    (ess-force-buffer-current "Process: " nil t nil))

  (cond
   ((eq obj 'ess-last-eval-region)
    (when (null gpb:ess-last-eval-region)
      (error "No region has been evaluated yet."))
    (let* ((beg (car gpb:ess-last-eval-region))
           (buf (marker-buffer beg))
           (end (cdr gpb:ess-last-eval-region)))
      (with-current-buffer buf (gpb:ess-eval-region beg end))))
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
               (buf (or (let ((path (concat default-directory filename)))
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
  (ess-send-string (get-buffer-process (current-buffer)) "traceback()" t))


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
        new-val))))


(defun gpb:ess-get-local-filename (filename)
  "Remote any TRAMP prefix from `FILENAME'"
  (if (tramp-tramp-file-p filename)
      (tramp-file-name-localname (tramp-dissect-file-name filename))
    filename))


(defun gpb:ess-make-region-file (beg end &optional where)
  "Create an R source file containing a region of code.

We jump through hopes to ensure that the R source code references
refer to the current buffer, rather than to the temporary file
that is produced."
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
      (insert (format "srcFile <- %s\n" (prin1-to-string source-filename)))
      (insert (format "regionFile <- %s\n"
                      (prin1-to-string
                       (gpb:ess-get-local-filename region-filename))))
      (insert "src <- srcfilecopy(srcFile, readLines(regionFile),\n")
      (insert "                   timestamp = Sys.time(), isFile = TRUE)\n")
      (insert "expr <- parse(text = readLines(regionFile), srcfile = src)\n")
      (insert "eval(expr)\n"))
    (message "Wrote %s" wrapper-filename)
    wrapper-filename))


(defun gpb:ess-eval-region (beg end)
  (interactive "r")
  (let* ((end (save-excursion
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

    (if (= line1 line2)
        (ess-send-string ess-proc (buffer-substring-no-properties beg end) t)
      (let* ((filename (gpb:ess-make-region-file beg end proc-default-directory))
             (local-filename (if (tramp-tramp-file-p filename)
                                 (tramp-file-name-localname
                                  (tramp-dissect-file-name filename))
                               filename))
             (cmd (format "source(%s)" (prin1-to-string local-filename))))
        (gpb:exit-all-browsers)
        (ess-send-string ess-proc cmd (format "[Evaluate lines %s-%s in %s]"
                                              line1 line2 (buffer-name)))))))


(defun gpb:ess-save-package ()
  "Save all files in the current package that have been edited."
  (interactive)
  (when (car (ess-r-package-project))
    (let* ((local-pkg-dir (cdr (ess-r-package-project)))
           (code-dir (concat (file-remote-p default-directory)
                             (file-name-as-directory local-pkg-dir)))
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
      (with-current-buffer buf
        (save-excursion
          (goto-char (marker-position (process-mark proc)))
          (beginning-of-line)
          (when (looking-at-p "^Browse\\[[0-9]+\\]> *")
            (ess-send-string (ess-get-process) "Q" t)))))))


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
  (let ((two-space-count 0))
    ;; Count the number of lines with two space indentation.
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (= (current-indentation) 2) (incf two-space-count))
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
  (compilation-next-error -1 nil (point)))
