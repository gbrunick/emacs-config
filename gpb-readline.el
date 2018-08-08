;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  The following section provides tab completion for readline
;;  applications in comint.  This is minor mode that should run on top
;;  of comint.
;;
;;  We use <comint-readline-end-completion> and
;;  <comint-readline-end-history> as a flags to indicate that we have
;;  just finished a completion request.  This is immediately followed
;;  by the readline commands to clear the current input line, so we
;;  see these strings followed by a bunch of control characters as
;;  output from the process.  We remove the control characters in a
;;  preoutput filter and then parse the return value before anything
;;  is inserted into the buffer.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'gpb-logging)
(require 'gpb-util)

(defvar gpb-rl--debug nil
  "A debugging flag.

To enable debugging:  (setq gpb-rl--debug t message-log-max t)
To disable debugging: (setq gpb-rl--debug nil message-log-max 100)")

(defvar gpb-rl-debug nil "Show debug messages.")
;; (setq gpb-rl-debug t)

(defcustom gpb-rl-mark-completion-request-as-input t
  "Should the completion request be marked as a line of input?")

(defvar gpb-rl-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab] 'gpb-rl-complete)
    (define-key map [remap comint-previous-input]
      'gpb-rl-prev-history)
    (define-key map [remap comint-next-input]
      'gpb-rl-next-history)
    (define-key map [(control p)] 'gpb-rl-prev-history)
    (define-key map [(control n)] 'gpb-rl-next-history)
    map)
  "The keymap for readline minor mode.")

(defvar gpb-rl-action-pending nil
  "Is readline-tab-completion-mode waiting for a completion?")
;;(set 'gpb-rl-action-pending nil)

(defvar gpb-rl-output-buffer ""
  "Used to store up responses in the preinput filter.")

(defvar gpb-rl-last-input ""
  "Store the last input sent to the process.")

(define-minor-mode readline-mode
  "A minor mode which allows readline processes to handle their own
  completions."
  :init-value nil :lighter " RL" :keymap gpb-rl-map
  (setq ;;comint-process-echoes t
        gpb-rl-action-pending nil)
  (add-hook 'comint-input-filter-functions
            'gpb-rl-input-filter t)
  (add-hook 'comint-preoutput-filter-functions
            'gpb-rl-preoutput-filter nil t)
  ;; We have to use this filter function rather than
  ;; comint-process-echos.  The values returned by readline may have
  ;; carriage returns imbedded in them.  This confuses
  ;; comint-input-sends, so it does not realize that the output from
  ;; the process matches the input that we sent.  The filter below
  ;; runs after the carriage returns have already been removed from
  ;; the output, so it is not confused by this behaviour.
  ;; (remove-hook 'comint-output-filter-functions
  ;;              'gpb-comint-remove-echoed-output-filter t)
  (make-local-variable 'gpb-rl-action-pending)
  (make-local-variable 'gpb-rl-last-input)
  (make-local-variable 'gpb-rl-output-buffer))

(defun gpb-make-readline-in-buffer (process-name process-command
                                                &optional env-alist)
  "Start a readline process with given name in the current buffer.

Enables readline-mode in the comint buffer.  ENV-ALIST is an
alist of environment variables."
  (unless (derived-mode-p 'comint-mode)
    (error "gpb-make-readline-in-buffer must be run in a comint buffer"))
  ;; Comint usually sets TERM=dumb and TERMCAP=, and readline doesn't
  ;; activate when it sees this.  We also need the tty have echo
  ;; enabled so we run stty before we exec the command (like term-mode
  ;; does).  The following setings from term-mode also work
  ;; `("TERM=eterm-color" ,gpb-termcap-string ,gpb-terminfo-string)
  ;; if you remove the comments from the lines below
  (setq gpb-comint-columns (window-width))
  (make-comint-in-buffer process-name (current-buffer)
                         "sh" nil "-c"
                         (format "stty echo; TERM=vt100; export TERM; exec %s"
                                 process-command))
  (readline-mode 1))

(defun gpb-rl-current-input ()
  "Returns the current line of input."
  (let* ((proc (get-buffer-process (current-buffer)))
         (proc-mark (process-mark proc)))
    (if (> (point) proc-mark)
        (buffer-substring-no-properties proc-mark (point))
      "")))


(defun gpb-rl-complete ()
  (interactive)
  (let ((proc (get-buffer-process (current-buffer)))
        current-input
        (gpb-log-enabled gpb-rl--debug)
        (gpb-log-label 'gpb-rl-complete))
    (gpb-log-form 'gpb-rl-complete 'last-command)
    (cond
     ((not (comint-after-pmark-p))
      (error "Must be on input line to complete."))
     (gpb-rl-action-pending
      (error "Still waiting for previous completion."))
     (t
      (setq gpb-rl-action-pending t
            gpb-rl-first-batch-of-output t
            gpb-rl-last-input (gpb-rl-current-input))
      ;; The space after <comint-readline-end-completion> below seems
      ;; necessary.  Otherwise the closing ">" is sometimes removed.
      (comint-send-string
       proc
       (concat (gpb-rl-current-input)
               ;; "\e?"
               ;; Send two tabs on a repeat
               (if (equal last-command 'gpb-rl-complete)
                   "\t\t"
                 "\t")
               "<comint-readline-end-completion> \C-U\C-K"))))))


(defun gpb-rl-prev-history ()
  (interactive)
  (message "Previous readline history")
  (gpb-rl-send-history-command "\eOA"))


(defun gpb-rl-next-history ()
  (interactive)
  (message "Next readline history")
  (gpb-rl-send-history-command "\eOB"))


(defun gpb-rl-send-history-command (command)
  "Send a history command to the readline process, wait for the
  answer, and replace the current input line with the
  answer. Command is the sequence of characters to send to the
  process after the current input line."
  (let ((proc (get-buffer-process (current-buffer)))
        current-input)
    (cond
     ((not (comint-after-pmark-p))
      (error "Must be on input line to get history."))
     (gpb-rl-action-pending
      (error "Still waiting for previous completion."))
     (t
      (setq gpb-rl-action-pending t)
      ;; The space after <comint-readline-end-answer> below seems
      ;; necessary.  Otherwise the closing ">" is sometimes removed.
      ;; (comint-redirect-send-command
      ;;  (concat (gpb-rl-current-input) command
      ;;          "<comint-readline-end-history> \C-U\C-K")
      ;;  "*readline-history*" nil)
      (comint-send-string
       proc (concat (gpb-rl-current-input) command
                    "<comint-readline-end-history> \C-U\C-K"))))))


(defun gpb-rl-input-filter (text)
  (let ((gpb-log-enabled gpb-rl--debug)
        (gpb-log-label 'gpb-rl-complete))
    (gpb-log-message 'gpb-rl-input-filter "Entering gpb-rl-input-filter...")
    (gpb-log-message 'gpb-rl-input-filter " gpb-rl-input-filter:text=%S" text))
  (setq gpb-rl-last-input text))


(defun gpb-rl-preoutput-filter (text)
  "Handle interaction with readline before the text enters the buffer."
  (let ((buffer (concat gpb-rl-output-buffer text)))
    (gpb-log-forms 'gpb-rl-preoutput-filter 'text 'buffer)
    ;; We handle xterm control sequences which set the title of the
    ;; window by setting the headerline in the buffer
    ;; see http://www.faqs.org/docs/Linux-mini/Xterm-Title.html#s3
    (while (string-match "\C-[][0-2];\\(.*?\\)\C-G" buffer)
      (gpb-log-message 'gpb-rl-preoutput-filter
                       "xterm ctrl seq: %S" (match-string 0 buffer))
      (setq default-directory (match-string 1 buffer)
            header-line-format (gpb-util-center-string
                                (concat
                                 (propertize
                                  "dir: " 'face
                                  '((foreground-color . "gray60")))
                                 default-directory)
                                (window-width)
                                'truncate-left)
            buffer (replace-match "" nil nil buffer))
      (gpb-log-form 'gpb-rl-preoutput-filter 'buffer))
    (if (not readline-mode)
        ;; If we are not in readline mode, just return text
        text
      (let ((suggestions "")
            ;; ipython will change the case of completions
            (case-fold-search nil)
            new-input-line base-regex regex
            buffer-without-linebreaks)
        (setq buffer-without-linebreaks (replace-regexp-in-string
                                         "\\(?:.?\C-M\\)?" "" buffer))
        (gpb-log-forms 'gpb-rl-preoutput-filter '(length text)
                       'text
                       '(length buffer)
                       'buffer
                       'buffer-without-linebreaks
                       'gpb-rl-last-input
                       'gpb-rl-first-batch-of-output)
        (cond
         ;; If no readline interaction is pending
         ((not gpb-rl-action-pending)
          (gpb-rl-preoutput-filter--handle-regular))

         ;; If we have finished a completion
         ((string-match "<comint-readline-end-completion>"
                        buffer-without-linebreaks)
          (gpb-rl-preoutput-filter--handle-completion))

         ;; If we have finished a history request
         ((string-match "<comint-readline-end-history>"
                        buffer-without-linebreaks)
          (gpb-rl-preoutput-filter--handle-history))


         ;; We are not done completing.  Things slow to a crawl if the
         ;; buffer gets too long (probably due to the regular expression
         ;; matching).  To avoid this we cap the max length of the buffer.
         (t
          ;; (when gpb-rl-debug
          ;;   (message "Not done completing..."))
          (let ((return-text "")
                (keep-chars 1000))
            (when (> (length buffer) 2000)
              ;; The buffer is too long.  Return all but the last
              ;; keep-chars characters
              (setq return-text (substring buffer 0 (- keep-chars))
                    buffer (substring buffer (- keep-chars) (length buffer)))
              (dolist (x '("\C-G" ".\b"))
                (setq return-text (gpb-util-reduce-string return-text x "")))
              (gpb-log-form 'gpb-rl-preoutput-filter 'return-text)
              ;; (when gpb-rl-debug
              ;;   (logval 'return-text "gpb-rl-preoutput-filter")
              ;;   (setq return-text (concat return-text "\n***\n")))
              (when gpb-rl-first-batch-of-output
                (let ((first-rest (gpb-util-split-string return-text "\n" 1)))
                  (comint-delete-input)
                  (gpb-rl-insert-as-old-input (car first-rest))
                  (setq return-text (or (cadr first-rest) "")
                        gpb-rl-first-batch-of-output nil))))
            (when (string-match "Display all .* possibilities" buffer)
              (gpb-log-message 'gpb-rl-preoutput-filter "Sending \"y\\n\" to process")
              (comint-send-input "y" t))
            (gpb-log-message 'gpb-rl-preoutput-filter "Waiting for more input...")
            (setq gpb-rl-output-buffer buffer)
            return-text)))))))

(defun gpb-rl-preoutput-filter--handle-regular ()
  (let ((gpb-log-enabled gpb-rl-debug)
        (gpb-log-label 'gpb-rl-preoutput-filter--handle-regular))
    (dolist (x '("\C-G" ".\C-M" "\C-[\\[[0-9]*[ACKP]"))
      (setq buffer (gpb-util-reduce-string buffer x ""))
      (gpb-log-forms 'gpb-rl-preoutput-filter--handle-regular 'x 'buffer))
    (setq gpb-rl-output-buffer "")
    (setq buffer (gpb-util-remove-backspaces buffer))
    (gpb-log-forms 'gpb-rl-preoutput-filter--handle-regular '(length buffer)
                   '(substring buffer 0 (min 100 (length buffer)))
                   'gpb-rl-last-input)
    ;; The process echoes its input, but we can't use
    ;; comint-process-echos becauses the echoed input may have
    ;; line breaks inserted into it which we remove above.
    (cond
     ((and gpb-rl-last-input
           (>= (length buffer) (length gpb-rl-last-input))
           (string-match
            (concat "^" (regexp-quote gpb-rl-last-input)
                    "\\(\\(?:.\\|\n\\)*\\)") ;; <- anything
            buffer))
      ;; If the buffer starts with echoed input, remove echoed input
      (setq buffer (match-string 1 buffer)
            gpb-rl-matching-input nil))
     ((and gpb-rl-last-input
           (< (length buffer) (length gpb-rl-last-input))
           (string-match
            (concat "^" (regexp-quote buffer)
                    "\\(\\(?:.\\|\n\\)*\\)") ;; <- anything
            gpb-rl-last-input))
      ;; If echoed input matches the whole buffer, delete the
      ;; chunk of echoed input that was matched and clear the
      ;; buffer
      (setq gpb-rl-last-input
            (match-string 1 gpb-rl-last-input)
            buffer ""))
     (t
      ;; If the new output doesn't match old input, just give up.
      (setq gpb-rl-last-input "")))
    (gpb-log-form 'gpb-rl-preoutput-filter--handle-regular '(length buffer))
    buffer))

(defun gpb-rl-preoutput-filter--handle-history ()
  (let ((history-regex
         ;; The current input is followed by control characters
         ;; which delete the current input and then the text of the
         ;; command from history
         (concat ;; Newlines can occur within the current input, and
          ;; we quote each character to make it regex safe
          "\\("
          (gpb-util-join-strings
           (mapcar 'regexp-quote
                   (split-string (gpb-rl-current-input)
                                 "" 'omit-nulls))
           "\\(?:.?\C-M\\)?")
          "\\)"
          "\\(\\(?:.\\|\n\\)*?\\)"
          "\\("
          ;; Newlines can occur within the marker
          (gpb-util-join-strings
           (split-string "<comint-readline-end-history>"
                         "" 'omit-nulls)
           "\\(?:.?\C-M\\)?")
          "\\).*"))
        (gpb-log-enabled gpb-rl--debug)
        (gpb-log-label 'gpb-rl-complete))
    (string-match history-regex buffer)
    (let ((new-input (match-string 2 buffer)))
      ;; Get ride of readline cursor motion commands.
      (let ((prompt (gpb-rl-current-input))
            (input (concat (match-string 1 buffer)
                           (match-string 2 buffer))))
        (when gpb-rl-debug
          (message "Entering gpb-rl-preoutput-filter--handle-history...")
          (logval 'buffer "--handle-history")
          (message " match 1: %S" (match-string 1 buffer))
          (message " match 2: %S" (match-string 2 buffer))
          (message " match 3: %S" (match-string 3 buffer))
          (message " new-input: %S" new-input)
          (message " prompt: %S" prompt)
          (message " input: %S" input)
          )
        (setq new-input (gpb-rl-reduce-control-string
                         (concat prompt input)))
        (assert (gpb-util-starts-with new-input prompt))
        (setq new-input (substring new-input (length prompt)))
        )
      ;; (dolist (x '("\\(.\\|\n\\)\C-M" "\C-M" "\C-[\\[[0-9]*[ACKPhl]"
      ;;              ".\b" "\b"))
      ;;   (setq new-input (gpb-util-reduce-string new-input x ""))
      ;;   (when gpb-rl-debug
      ;;     (message "new-input: %S" new-input)))
      (if (gpb-util-ends-with new-input "\C-G")
          ;; If history fails readline returns the bell \C-G
          (message "Readline history command failed.")
        (comint-delete-input)
        (insert new-input))
      (setq gpb-rl-action-pending nil
            gpb-rl-output-buffer ""
            ;; python-pdbtrack-do-tracking-p
            ;;    gpb-rl-save-pdbtrack-state
            ))
    ""))

(defun gpb-rl-preoutput-filter--handle-history-2 ()
  (let ((gpb-log-enabled gpb-rl--debug)
        (gpb-log-label 'gpb-rl-complete))
    (with-current-buffer (get-buffer-create
                          (if gpb-debug-flag "*readline*" " *readline*"))
      (fundamental-mode)
      (erase-buffer)
      ;; (set (make-local-variable 'ansi-color-context) nil)
      ;; (when gpb-rl-debug (message "preansi input: %s" input))
      ;; (setq input (ansi-color-apply input))
      (when gpb-rl-debug
        (logval 'input "gpb-rl-preoutput-filter--handle-history-2"))
      (insert prompt)
      ;; (goto-char (point-max))
      ;; (insert "\n\n")
      ;; (insert prompt)
      ;; (insert input)
      ;;(insert "\n\n")
      (save-excursion
        ;;(insert "\n\n")
        ;;(backward-char 2)
        (while (not (equal input ""))
          ;;(when gpb-rl-debug (message "input: %s" input))
          (cond
           ;; ;; Carriage return
           ;; ((string-match "\\`\r" input)
           ;;  (forward-line 0)
           ;;  (setq input (match-string 2 input)))
           ;; Carriage return
           ((string-match "\\`\r\\(.*\\)" input)
            (forward-line 0)
            ;; (if (looking-at-p "\n")
            ;;     (forward-char)
            ;;   (insert "\n"))
            (setq input (match-string 1 input)))
           ;; backspace
           ((string-match "\\`\b\\(.*\\)" input)
            (delete-char -1)
            (setq input (match-string 1 input)))
           ;; Forward a character
           ((string-match "\\`\C-[\\[C\\(.*\\)" input)
            (forward-char)
            (setq input (match-string 1 input)))
           ;; kill line
           ((string-match "\\`\C-[\\[K\\(.*\\)" input)
            (delete-region (point) (point-max))
            ;;(ignore-errors (kill-line))
            (setq input (match-string 1 input)))
           ;; move cursor up
           ((string-match "\\`\C-[\\[A\\(.*\\)" input)
            (let ((goal-col (current-column)))
              (forward-line -1)
              (move-to-column goal-col t))
            (setq input (match-string 1 input)))
           ;; Overwrite one character
           (t
            ;; first wrap if necessary
            (when (>= (current-column) gpb-comint-columns)
              (unless (search-forward "\n" nil t)
                (goto-char (point-max))
                (insert "\n")))
            (insert (string-to-char input))
            (when (not (eolp)) (delete-char 1))
            ;; (move-end-of-line 1)
            ;; (if (looking-at-p "\n")
            ;;     (forward-char)
            ;;   (insert "\n")))
            (setq input (substring input 1)))
           ))
        ;; Remove line wraps
        (goto-char (point-min))
        (while (search-forward "\n" nil t)
          (replace-match "" nil nil))
        ;; (while (not (eobp))
        ;;   (ignore-errors (forward-char gpb-comint-columns))
        ;;   (when (looking-at-p ".\n")
        ;;     (delete-char 2)))
        ) ;; save-excurions
      (setq new-input (buffer-substring (point) (point-max))))))


(defun gpb-rl-reduce-control-string (input)
  "Remove the control sequence from a text string.

  This is complicated and so do this in a buffer where we can
  follow all the cursor movement commands.

  This command does not touch bells \"\\C-G\"."
  (let ((gpb-log-enabled gpb-rl--debug) (gpb-log-label 'gpb-rl-complete))
    (with-current-buffer (get-buffer-create
                          (if gpb-rl-debug "*gpb-rl*" " *gpb-rl*"))
      (fundamental-mode)
      (erase-buffer)
      ;; (set (make-local-variable 'ansi-color-context) nil)
      ;; (when gpb-rl-debug (message "preansi input: %s" input))
      ;; (setq input (ansi-color-apply input))
      (when (or gpb-rl-debug nil)
        (logval 'input "gpb-rl-reduce-control-string"))
      ;; (goto-char (point-max))
      ;; (insert "\n\n")
      ;; (insert prompt)
      ;; (insert input)
      ;;(insert "\n\n")
      (save-excursion
        ;;(insert "\n\n")
        ;;(backward-char 2)
        (while (not (equal input ""))
          ;;(when gpb-rl-debug (message "input: %s" input))
          (cond
           ;; ;; Carriage return
           ;; ((string-match "\\`\r" input)
           ;;  (forward-line 0)
           ;;  (setq input (match-string 2 input)))
           ;; Carriage return
           ((string-match "\\`\r\\(.*\\)" input)
            (forward-line 0)
            ;; (if (looking-at-p "\n")
            ;;     (forward-char)
            ;;   (insert "\n"))
            (setq input (match-string 1 input)))
           ;; backspace
           ((string-match "\\`\b\\(.*\\)" input)
            (delete-char -1)
            (setq input (match-string 1 input)))
           ;; Forward a character
           ((string-match "\\`\C-[\\[C\\(.*\\)" input)
            (forward-char)
            (setq input (match-string 1 input)))
           ;; kill line
           ((string-match "\\`\C-[\\[K\\(.*\\)" input)
            (delete-region (point) (point-max))
            ;;(ignore-errors (kill-line))
            (setq input (match-string 1 input)))
           ;; move cursor up
           ((string-match "\\`\C-[\\[A\\(.*\\)" input)
            (let ((goal-col (current-column)))
              (forward-line -1)
              (move-to-column goal-col t))
            (setq input (match-string 1 input)))
           ;; Overwrite one character
           (t
            ;; first wrap if necessary
            (when (>= (current-column) gpb-comint-columns)
              (unless (search-forward "\n" nil t)
                (goto-char (point-max))
                (insert "\n")))
            (insert (string-to-char input))
            (when (not (eolp)) (delete-char 1))
            ;; (move-end-of-line 1)
            ;; (if (looking-at-p "\n")
            ;;     (forward-char)
            ;;   (insert "\n")))
            (setq input (substring input 1)))
           ))
        ;; Remove line wraps
        (goto-char (point-min))
        (while (search-forward "\n" nil t)
          (replace-match "" nil nil))
        ;; (while (not (eobp))
        ;;   (ignore-errors (forward-char gpb-comint-columns))
        ;;   (when (looking-at-p ".\n")
        ;;     (delete-char 2)))
        ) ;; save-excurions
      (let ((ansi-color-context-region nil)
            output)
        (ansi-color-apply-on-region (point-min) (point-max))
        (setq output (buffer-substring (point) (point-max)))
        (when (or gpb-rl-debug nil)
          (logval 'output "gpb-rl-reduce-control-string"))
        output))))

(defun gpb-rl-insert-as-old-input (text)
  (insert (propertize text
                      'field 'input
                      'font-lock-face 'comint-highlight-input
                      'fontified    t
                      'front-sticky t))
  (insert (propertize "\n"
                      'field 'boundary 'fontified t
                      'inhibit-line-move-field-capture t
                      'rear-nonsticky t))
  (comint-snapshot-last-prompt)
  (let ((proc (get-buffer-process (current-buffer))))
    (set-marker comint-last-input-start
                (process-mark proc))
    (set-marker comint-last-input-end (point))
    (set-marker (process-mark proc) (point))))

;; (defun gpb-insert-first-line-as-input-return-rest (text)
;;   "Remove the first line from text-symbol and insert it in the comint
;;   buffer as input.  Returns the remaining text.

;;   If comint is using the text property 'fields to delimit input
;;   from output, and we have text, then the first line of text is
;;   the original input and we add text properties to make it look
;;   like user input and then insert it.  We leave the rest of text
;;   alone and return it so it looks like it came from the process."
;;   (when gpb-rl-debug
;;   (message "Entering gpb-insert-first-line-as-input-return-rest...")
;;   (when (> (length text) 0)
;;     (let* (;;(text (symbol-value text-symbol))
;;            (lines (split-string text "\n"))
;;            (first-line (car lines))
;;            (proc (get-buffer-process (current-buffer)))
;;            (debug (or gpb-rl-debug nil)))
;;       (setq gpb-rl-last-input (gpb-rl-remove-control-chars first-line)
;;             text (gpb-util-join-strings (cdr lines) "\n"))
;;       (when debug
;;         (message " gpb-insert-first-line-as-input-return-rest:text: %S" text)
;;         (message " gpb-insert-first-line-as-input-return-rest:first-line: %S"
;;                  first-line)
;;         (message " gpb-insert-first-line-as-input-return-rest:rest:\n%S" text))
;;       (if (or comint-use-prompt-regexp
;;               (not gpb-rl-mark-completion-request-as-input))
;;           (insert first-line "\n")
;;         (comint-delete-input)
;;         (insert (propertize first-line
;;                             'field 'input
;;                             'font-lock-face 'comint-highlight-input
;;                             'fontified    t
;;                             'front-sticky t))
;;         (insert (propertize "\n"
;;                             'field 'boundary 'fontified t
;;                             'inhibit-line-move-field-capture t
;;                             'rear-nonsticky t))
;;         (comint-snapshot-last-prompt)
;;         (set-marker comint-last-input-start (process-mark proc))
;;         (set-marker comint-last-input-end (point))
;;         (set-marker (process-mark proc) (point))
;;         )
;;       (message "gpb-insert-first-line-as-input-return-rest 2")))
;;   text)

(defun gpb-rl-remove-control-chars (text)
  (dolist (x '("\C-G" ".\b" "\b" ".\C-M" "\C-M" "\C-[\\[[0-9]*[ACKP]"))
    (setq text (gpb-util-reduce-string x "" text)))
  text)

(defun gpb-rl-preoutput-filter--handle-completion ()
  "We have finished a completion so we split the output into:

  1) suggestions followed by the prompt which we return so they
     look like they came from the process and get inserted at the
     process mark (which is before the user text that we insert
     below), and

  2) the new input line which we insert in the buffer so it looks
     like it came from the user."
  (let ((gpb-log-enabled gpb-rl--debug)
        (gpb-log-label "gpb-rl-...-handle-completion"))

    (setq buffer buffer-without-linebreaks
          gpb-rl-action-pending nil
          gpb-rl-output-buffer "")
    (let ((completion-regex
           (concat "\\(\\(?:.\\|\n\\)*?\\)"
                   "\\("
                   "<comint-readline-end-completion>"
                   ;; Newlines can occur within the marker
                   ;; (gpb-util-join-strings
                   ;;  (split-string "<comint-readline-end-completion>"
                   ;;                "" 'omit-nulls)
                   ;;  "\\(?:.?\C-M\\)?")
                   "\\).*"))
          )
      (string-match completion-regex buffer))
    (gpb-log-forms 'gpb-rl-preoutput-filter--handle-completion 'buffer '(match-string 1 buffer) '(match-string 2 buffer)
                   'gpb-rl-last-input 'last-command)
    ;; (when (or gpb-rl-debug nil)
    ;;   (message "Entering gpb-rl-preoutput-filter--handle-completion")
    ;;   (logval 'buffer "--handle-completion")
    ;;   (logval '(match-string 1 buffer) "--handle-completion")
    ;;   (logval '(match-string 2 buffer) "--handle-completion")
    ;;   (logval 'gpb-rl-last-input "--handle-completion")
    ;;   (logval 'last-command "--handle-completion")
    ;;   )
    (setq buffer (match-string 1 buffer))
    (dolist (x '("\C-G" ".\b" ".\C-M"))
      (setq buffer (gpb-util-reduce-string buffer x "")))

    ;; The last input should not be empty
    (when (<= (length gpb-rl-last-input) 0)
      (message "Warning: gpb-rl-last-input=%S" gpb-rl-last-input))
    ;; Find the copy of last input after the suggestions
    (let* ((regex (concat (regexp-quote gpb-rl-last-input) ".*"))
           (expand-home-regex (replace-regexp-in-string
                               "~/" (expand-file-name "~/") regex))
           split-position)
      (gpb-log-forms 'gpb-rl-preoutput-filter--handle-completion 'buffer 'regex 'expand-home-regex)
      ;; (when gpb-rl-debug
      ;;   (logval 'buffer "--handle-completion")
      ;;   (logval 'regex "--handle-completion")
      ;;   (logval 'expand-home-regex "--handle-completion"))
      ;; (message " --handle-completion:regex: %S" regex)
      ;; (message " --handle-completion:expand-home-regex: %S" expand-home-regex))
      (or
       (gpb-util-last-string-match regex buffer)
       ;; ipython expands the home directory on completion
       (gpb-util-last-string-match expand-home-regex buffer)
       (let ((case-fold-search t))
         (or
          (gpb-util-last-string-match regex buffer)
          (gpb-util-last-string-match expand-home-regex buffer)))
       ;; This is an odd special case.  In some instances, the
       ;; commandline contains \\~/dir/... instead of ~/dir/.. after
       ;; completion in bash.  I have no idea why, so I try to add a
       ;; workaround that is as specific as possible.
       ;; To reproduce this behavior in bash type:
       ;; emacs-edit ~/usr-u[tab]
       (gpb-util-last-string-match
        regex (gpb-util-replace-string buffer " \\~/" " ~/"))
       (let ((gpb-log-enabled t))
         (gpb-log-forms 'gpb-rl-preoutput-filter--handle-completion 'buffer 'regex 'expand-home-regex 'case-fold-search)
         (error "buffer does not match regex")))

      (gpb-log-form 'gpb-rl-preoutput-filter--handle-completion '(match-string 0 buffer))
      ;; (message " --handle-completion:match 0: %S" (match-string 0 buffer)))
      (setq split-position (match-beginning 0))
      ;; This is a special case for ipython which completes
      ;; "magic" command by prepending a %.  This "%" should be
      ;; included in the input line not the prompt
      (when (and (> split-position 0)
                 (equal (elt buffer (1- split-position)) ?%))
        (setq split-position (1- split-position)))
      (comint-kill-input)
      (if (and gpb-rl-first-batch-of-output (= split-position 0))
          ;; If there are no suggestions, just complete
          (progn
            (gpb-log-message 'gpb-rl-preoutput-filter--handle-completion " --handle-completion: No suggestions")
            (insert buffer)
            "")
        ;; Otherwise return suggestions so they come from the process
        (when gpb-rl-debug
          (message " --handle-completion: Suggestions given"))
        (setq suggestions-and-prompt
              (gpb-util-remove-backspaces (substring buffer 0 split-position))
              new-input-line (substring buffer split-position))
        ;; Sometime it takes two gpb-rl-complete too get completion.  If it
        ;; happens in one, we reset the count.  We reset last-command not
        ;; this-command because this runs in a filter not in a command.  The
        ;; command is already over.
        ;; We only clear this flag after we have received suggestions.
        ;; This way we can send a double tab on on consecutive
        ;; completion attempts.
        (when (equal last-command 'gpb-rl-complete) (setq last-command nil))
        (gpb-log-forms 'gpb-rl-preoutput-filter--handle-completion 'suggestions-and-prompt 'new-input-line
                       'gpb-rl-first-batch-of-output)
        ;; (message " --handle-completion:suggestions-and-prompt: %S"
        ;;          suggestions-and-prompt)
        ;; (message " --handle-completion:Input line=%S" new-input-line)
        ;; (message " --handle-completion:gpb-rl-first-batch-of-output=%S"
        ;;          gpb-rl-first-batch-of-output))
        (when gpb-rl-first-batch-of-output
          (let ((first-rest (gpb-util-split-string suggestions-and-prompt
                                                   "\n" 1)))
            (gpb-log-message 'gpb-rl-preoutput-filter--handle-completion " --handle-completion:first-rest=%S" first-rest)
            (gpb-rl-insert-as-old-input (car first-rest))
            (setq suggestions-and-prompt (or (cadr first-rest) ""))))
        (insert (gpb-util-remove-backspaces new-input-line))
        suggestions-and-prompt))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Some user level commands
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Run bash in a way that allows readline completion
(defun run-bash (arg)
  "Open a bash shell in a buffer.

With a prefix, try to open the shell in a different window (using
`pop-to-buffer')."
  (interactive "P")
  (let ((buffer ;; (if arg
                ;;     (generate-new-buffer "*bash*")
                  (get-buffer-create "*bash*"))
        (dir default-directory))
    (if arg
        (pop-to-buffer buffer)
      (switch-to-buffer buffer))
    (setq default-directory dir)
    (if (comint-check-proc buffer)
        ;; If process is running, cd to the right directory
        (progn
          (goto-char (point-max))
          (comint-kill-input)
          (insert (format "cd %s"
                          (replace-regexp-in-string
                           "'" "\\\\'"
                          (replace-regexp-in-string
                           "(" "\\\\("
                           (replace-regexp-in-string
                            ")" "\\\\)"
                            (replace-regexp-in-string " " "\\\\ " dir))))))
          (comint-send-input))
      ;; Otherwise start up a new process
      (unless (derived-mode-p 'comint-mode)
        (comint-mode))
      (goto-char (point-max))
      (insert "Running bash...\n")
    (gpb-make-readline-in-buffer (buffer-name buffer) "bash")
    (let ((proc (get-buffer-process (current-buffer))))
      (set-process-sentinel proc
                            'gpb-bury-buffer-on-process-exit)
      ;; (set-process-query-on-exit-flag proc nil)
      (accept-process-output proc 1)
      )
    ;; (gpb-comint-make-previous-output-readonly)
    ;; (gpb-comint-disable-prompt-highlighting)
    ;; (gpb-set-local-compilation-errors '(gpb-log gpb-python gpb-pdb-stack
    ;;                                             bash))
    (compilation-shell-minor-mode 1)
    ;; (gpb-comint-reset-compilation-errors-on-send)
    ;; (gpb-cm:add-context-menu-items make-bash-context-menu t)
    )))

(defalias 'terminal 'run-bash)

(defun make-bash-context-menu ()
  '(("Clear buffer" gpb-comint-erase-buffer)
    ("Kill process" comint-kill-subjob)))


(provide 'gpb-readline)
