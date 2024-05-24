(require 'imenu)
(require 'subr-x)
(require 'cl-extra)
(require 'gpb-logging)

(defcustom gpb-fl-echo-delay 1
  "Delay in seconds before echoing the current selection to the minibuffer."
  :type 'integer
  :group 'gpb-r-mode)

(defface gpb-fl-selection-face
  '((t :inherit highlight :extend t))
  "Face used to highlight the selected face in a item selection buffer.")


(defvar gpb-fl--item-buffer nil
  "Let bound to choice buffer during read `gpb-fl--read-choice'")

(defvar gpb-fl--timer nil
  "Timer used to implement the selection echo.")


;; The following variables are defined in the item display buffer.

(defvar-local gpb-fl--items nil
  "The list of items.")
(defvar-local gpb-fl--selection-overlay nil
  "The overlay that highlights the current selection.")
(defvar-local gpb-fl--default-item nil
  "An integer giving an index into `gpb-fl--items'.")

(defvar gpb-choice-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m" 'gpb-fl--return-selected-item)
    (define-key map "\C-g" 'abort-recursive-edit)
    (define-key map "q" 'abort-recursive-edit)
    (define-key map "\C-n" 'gpb-fl--goto-next-item)
    (define-key map "\C-p" 'gpb-fl--goto-previous-item)
    map))

(when (and (boundp 'evil-mode) evil-mode)
  (evil-define-key 'normal gpb-choice-buffer-mode-map
    "\C-g" 'abort-recursive-edit
    "q" 'abort-recursive-edit))

(define-derived-mode gpb-choice-buffer-mode special-mode
  "Choice"
  "Major mode in the buffer displaying choices.")

(defun gpb-fl--make-choice-buffer (items &optional bufname init)
  "Construct a new buffer to display `items'.

If `bufname' is provided, it passed to `generate-new-buffer-name'
to create the name for the new buffer.  If `init' is provided, it
is a zero-based index into `items' giving the item to initially
select.  A negative item counts back from the end of the list so
-1 selects the last item in `items'."
  (let* ((buf (get-buffer-create (generate-new-buffer-name
                                  (or bufname "*Choices*"))))
         (inhibit-read-only t))

    (when (<= (length items) 0) (error "The list `items' is empty"))

    (when init
      ;; Negative init counts back from the end.
      (when (< init 0) (setq init (+ init (length items))))
      (unless (and (>= init 0) (<= init (length items)))
        (error "Invalid `init'")))

    (with-current-buffer  buf
      ;; ESS installs hooks on mode changes that search `default-directory'
      ;; for packages and we don't want to trigger these.
      (gpb-choice-buffer-mode)
      (read-only-mode 1)
      (setq-local default-directory nil)
      (setq-local gpb-fl--items items)
      (setq-local gpb-fl--default-item (or init 0))

      (setq-local gpb-fl--selection-overlay (make-overlay (point-min)
                                                          (point-min)))
      (overlay-put gpb-fl--selection-overlay 'face 'gpb-fl-selection-face)


      (add-hook 'post-command-hook #'gpb-fl--post-command-hook nil t)

      (gpb-fl--filter-choice-buffer nil buf))
    buf))


(defun gpb-fl--filter-choice-buffer (&optional text buf)
  "Filter choices in `buf' according to `text'

If `text' is nil or the empty string, we show all entries."
  (let* ((buf (or (get-buffer (or buf (current-buffer)))
                  (error "Invalid buffer")))
         (inhibit-read-only t)
         (case-fold-search t)
         (terms (and text (split-string (string-trim text) "\\s +")))
         ;; Predicate used to determine if we should keep an item.
         (pred (if terms
                   (lambda (item)
                     (cl-every (lambda (term) (string-match-p
                                               (regexp-quote term)
                                               (gpb-fl-get-item-matcher item)))
                               terms))
                 (lambda (item) t)))
         (i 0)
         (first t)
         ov current-item beg)

    (unless (buffer-live-p buf) (error "Invalid buffer"))

    (with-current-buffer buf
      (setq ov gpb-fl--selection-overlay
            current-item (or (get-text-property (overlay-start ov) 'field)
                             gpb-fl--default-item))
      (erase-buffer)
      (move-overlay ov (point) (point))
      (dolist (item gpb-fl--items)
        (when (funcall pred item)
          (setq beg (point))
          (insert (propertize (concat (gpb-fl-get-item-display item) "\n")
                              'item item
                              'field i
                              'front-sticky t
                              'rear-nonsticky t))
          (when (or first (<= i current-item))
            (move-overlay ov beg (point))
            (setq first nil)))
        (setq i (1+ i)))

      (goto-char (overlay-start ov)))))


(defun gpb-fl--return-selected-item ()
  (interactive)
  (throw 'gpb-fl--selection (gpb-fl--get-selected-item)))


(defun gpb-fl--post-command-hook ()
  "Adjust the selection overlay in an item view buffer after cursor movement."
  ;; Contrain the point to the last item/field
  (when (eobp) (backward-char))
  (move-overlay gpb-fl--selection-overlay (field-beginning) (field-end)))


(defvar gpb-fl--minibuffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map "\C-m" 'gpb-fl--return-selected-item)
    (define-key map [remap previous-line] 'gpb-fl--goto-previous-item)
    (define-key map [remap next-line] 'gpb-fl--goto-next-item)
    (define-key map [(meta p)] 'gpb-fl--goto-previous-item)
    (define-key map [(meta n)] 'gpb-fl--goto-next-item)
    (define-key map [(control p)] 'gpb-fl--goto-previous-item)
    (define-key map [(control n)] 'gpb-fl--goto-next-item)
    (define-key map [(meta p)] 'gpb-fl--goto-previous-item)
    (define-key map [(meta n)] 'gpb-fl--goto-next-item)
    (define-key map [remap find-file] 'gpb-fl--find-file)
    map)
  "The keymap used in the minibuffer during filtered list interactions.")


(defun gpb-fl--minibuffer-after-change-hook (&rest args)
  "Update the choices buffer when we edit the minibuffer."
  (gpb-log-forms 'gpb-fl--minibuffer-after-change-hook 1)
  (let ((minibuffer-window (active-minibuffer-window))
        text)
    (when minibuffer-window
      (with-current-buffer (window-buffer minibuffer-window)
        (setq text (buffer-substring-no-properties
                    (minibuffer-prompt-end) (point-max)))

        (gpb-log-forms 'gpb-fl--minibuffer-after-change-hook
                       'gpb-fl--item-buffer
                       'text)

        ;; `gpb-fl--item-buffer' is let bound during `gpb-fl--read-choice'
        (gpb-fl--filter-choice-buffer text gpb-fl--item-buffer)
        (gpb-fl--schedule-echo-timer)))))


(defun gpb-fl--read-choice (items &optional prompt buf-name map
                                  use-one-window init)
  "Display the PROMPT to ask user for a selection from ITEMS.

The items is shown in a completions buffer and is filtered as the user
types.  ITEMS may be a items of strings or an alist of (display-string
. result) cons cells."
  (save-window-excursion
    (when use-one-window (delete-other-windows))
    ;; We let bind these variables in an effort to allow recursive edits.
    (let* ((prompt (or prompt "Choice: "))
           (buf-name (generate-new-buffer-name (or buf-name "*Choices*")))
           (gpb-fl--item-buffer (gpb-fl--make-choice-buffer
                                 items buf-name init)))

      (set-window-buffer (selected-window) gpb-fl--item-buffer)

      (with-current-buffer gpb-fl--item-buffer
        ;; Cancel the current minibuffer edit if we kill display buffer.
        (add-hook 'kill-buffer-hook 'gpb-fl--try-abort-recursive-edit nil t))

      ;; We add hooks in the minibuffer to filter the buffer showing
      ;; choices as the user types.
      (add-hook 'minibuffer-setup-hook 'gpb-fl--setup-minibuffer-hook)
      (add-hook 'minibuffer-exit-hook 'gpb-fl--exit-minibuffer-hook)

      (catch 'gpb-fl--selection
        (unwind-protect
            (progn
              (gpb-fl--schedule-echo-timer)
              (read-from-minibuffer prompt nil (or map gpb-fl--minibuffer-map))
              nil)

          ;; Remove hooks and kill buffer
          (ignore-errors (cancel-timer gpb-fl--timer))
          (remove-hook 'minibuffer-setup-hook 'gpb-fl--setup-minibuffer-hook)
          (remove-hook 'minibuffer-exit-hook 'gpb-fl--exit-minibuffer-hook)
          (with-current-buffer gpb-fl--item-buffer
            (remove-hook 'kill-buffer-hook 'gpb-fl--try-abort-recursive-edit t))
          (kill-buffer gpb-fl--item-buffer))))))

(defun gpb-fl--setup-minibuffer-hook ()
  "Setup the minibuffer to update the filtered list."
  (gpb-log-forms 'gpb-fl--setup-minibuffer-hook 'gpb-fl--item-buffer)
  (add-hook 'after-change-functions 'gpb-fl--minibuffer-after-change-hook nil t))

(defun gpb-fl--exit-minibuffer-hook ()
  "Clean up the minibuffer and kill the list buffer."
  (gpb-log-forms 'gpb-fl--exit-minibuffer-hook 'gpb-fl--item-buffer)
  (remove-hook 'after-change-functions 'gpb-fl--minibuffer-after-change-hook t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Better recent file selection
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar gpb-fl--filtered-recentf-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map gpb-fl--minibuffer-map)
    (define-key map "\C-x\C-f" 'gpb-fl--read-filename)
    map)
  "The keymap used in the minibuffer during filtered list interactions.")

(defun gpb-fl--get-recentf-items (&optional max-width)
  (let ((path-list (delq nil (delete-dups
                              (mapcan (lambda (file)
                                        (list (file-name-directory file) file))
                                      recentf-list)))))
    (mapcar (lambda (path)
              `(,path
                :display ,(gpb-fl--decorate-file-path path max-width)
                :echo ,path))
            path-list)))

(defun gpb-fl--read-filename ()
  (interactive)
  (let ((filename (gpb-fl--get-selected-item)))
    (throw 'gpb-fl--selection
           (read-file-name "File: " (file-name-directory filename)
                           nil nil (file-name-nondirectory filename)))))

(defun gpb-recentf-open-files-filtered ()
  (interactive)
  (let* ((max-width (window-width))
         (items (gpb-fl--get-recentf-items max-width))
         file)
    (setq filename (gpb-fl--read-choice items "Recent File: " "*Recent Files*"
                                        gpb-fl--filtered-recentf-map))
    (when filename
      (find-file filename))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Better buffer switching
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun gpb-fl--get-buffer-items (&optional include-hidden max-width)
  "Generate a list of items describing the current set of buffers.

When `include-hidden' is true, we include the hidden buffers
whose names begin with space."
  (interactive "P")
  (let (buf bufs hidden-bufs buf-name buf-file-name)
    ;; We `push' items below, so we reverse the buffer list to start.
    (dolist (x (reverse (buffer-list)))
      ;; We copy these strings so that we can add properties without
      ;; affecting the string.
      (setq buf-name (substring (buffer-name x) 0)
            buf-file-name (or (ignore-errors (substring (buffer-file-name x) 0))
                              (with-current-buffer x
                                (and (derived-mode-p 'dired-mode)
                                     default-directory))))
      (cond
       ;; Push hidden buffers onto `hidden-bufs' unless a prefix argument
       ;; was given.
       ((or (string-match "^ " buf-name)
            (string-match "\\*epc con [0-9]+\\*" buf-name))
        (let ((buf-name (substring-no-properties buf-name)))
          (add-text-properties 0 (length buf-name)
                               '(face ((foreground-color . "gray61")))
                               buf-name)
          (push `(,x :display ,buf-name
                     :echo ,(substring-no-properties buf-name)
                     :matcher ,buf-name)
                hidden-bufs)))

       ;; show filename
       (buf-file-name
        (push `(,x :display ,(gpb-fl--decorate-file-path
                              buf-file-name max-width)
                   :echo ,buf-file-name
                   :matcher ,(format "%s %s" buf-name buf-file-name))
              bufs))

       ;; use buffer name
       (t
        (push `(,x :display ,buf-name
                   :echo ,buf-name
                   :matcher ,buf-name)
              bufs))))

    (if include-hidden
        (nconc bufs hidden-bufs)
      bufs)))


(defun gpb-switch-buffer-filtered (arg)
  "Switch buffers using a filter list.

When called with an argument (see `universal-argument'), we show
hidden the buffers whose names begin with space."
  (interactive "P")
  (let ((items (gpb-fl--get-buffer-items arg (window-width))))
    (setq buf (gpb-fl--read-choice items "Buffer: " "*Buffers*"))
    (switch-to-buffer buf)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Better comint history
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun gpb-fl--get-comint-history-items (&optional buf)
  (let* ((buf (or (and buf (get-buffer buf)) (current-buffer)))
         history)

    (with-current-buffer buf
      ;; Pull the items from the history ring
      (dotimes (index (or (ring-length comint-input-ring) 0))
        (push (ring-ref comint-input-ring index) history)))

    (reverse (delq nil (delete-dups (reverse history))))))


(defun gpb-read-comint-history-filtered (&optional buf)
  "Search comint history in a filtered list.

Falls back to global key binding if called when the point is
before the process mark."
  (interactive)
  ;; See `comint-dynamic-list-input-ring'
  (let* ((buf (or buf (current-buffer)))
         (proc (get-buffer-process buf))
         (history (gpb-fl--get-comint-history-items buf))
         selected-item)

    (setq selected-item (gpb-fl--read-choice
                         history "History Item: " "*Input History*"
                         nil nil -1))
    (when selected-item
      (goto-char (process-mark proc))
      (delete-region (point) (point-max))
      (insert selected-item))))


;;
;; Various utility functions
;;

(defun gpb-fl-get-item-matcher (item)
  (or (plist-get (cdr-safe item) :matcher)
      (plist-get (cdr-safe item) :display)
      (car-safe item)
      item))

(defun gpb-fl-get-item-display (item)
  (or (plist-get (cdr-safe item) :display)
      (car-safe item)
      item))

(defun gpb-fl--schedule-echo-timer (&rest args)
  "Schedule the echo timer to run soon."
  (ignore-errors (cancel-timer gpb-fl--timer))
  (setq gpb-fl--timer
        (run-at-time gpb-fl-echo-delay nil #'gpb-fl--echo-item)))

(defun gpb-fl--decorate-file-path (path &optional max-width)
  "Make directory prefix of `path' light grey.

If `max-width' is provided, we also attempt to trim the string to
at most width."
  (let* ((file-name (directory-file-name path))
         (tramp-prefix (or (file-remote-p file-name) ""))
         (dir (or (file-name-directory (file-local-name file-name)) ""))
         (basename (file-name-nondirectory file-name))
         ;; Make the tramp prefix and directory info grey.
         (dir-properties '(face ((foreground-color . "gray67"))))
         n)

    (when (directory-name-p path)
      (setq basename (file-name-as-directory basename)))

    (when (and max-width (> (length path) max-width))
      ;; n is how much we need to truncate `dir'
      (setq n (- (length path) (- max-width 3))
            dir (concat "..." (substring dir (min (length dir) n)))))

    (setq dir (concat tramp-prefix dir))
    (add-text-properties 0 (length dir) dir-properties  dir)
    (concat dir basename)))


(defmacro gpb-fl--define-command (name &rest body)
  "Define a command that is executed in the buffer showing the filtered list"
  (declare (indent 1))
  `(defun ,name ()
     (interactive)
     ;; If we are in a choice buffer, operate there.  Otherwise operate in
     ;; `gpb-fl--item-buffer'.
     (let* ((buf (or (and (derived-mode-p 'gpb-choice-buffer-mode)
                          (current-buffer))
                     gpb-fl--item-buffer)))
       (unless buf (error "No choice buffer available"))
       (with-current-buffer buf
         ,@body
         ;; Update the point in all windows showing the choice list.
         (dolist (win (get-buffer-window-list buf))
           (set-window-point win (overlay-start gpb-fl--selection-overlay)))
         (gpb-fl--echo-item-1)))))

(gpb-fl--define-command gpb-fl--goto-next-item
  (gpb-log-forms 'gpb-fl--goto-next-item
                 '(current-buffer)
                 'gpb-fl--selection-overlay)
  (goto-char (overlay-end gpb-fl--selection-overlay))
  (when (eobp) (backward-char))
  (move-overlay gpb-fl--selection-overlay (field-beginning) (field-end)))

(gpb-fl--define-command gpb-fl--goto-previous-item
  (goto-char (overlay-start gpb-fl--selection-overlay))
  (unless (bobp) (forward-line -1))
  (move-overlay gpb-fl--selection-overlay (field-beginning) (field-end)))


(defun gpb-fl--get-selected-item ()
  (interactive)
  (let ((buf (if (derived-mode-p 'gpb-choice-buffer-mode)
                 (current-buffer)
               gpb-fl--item-buffer))
        item)
    (unless buf (error "No choice buffer available"))
    (with-current-buffer buf
      (setq item (and gpb-fl--selection-overlay
                      (get-text-property
                       (overlay-start gpb-fl--selection-overlay) 'item)))
      (or (car-safe item) item))))


(defun gpb-fl--echo-item ()
  (gpb-log-forms 'gpb-fl--echo-item 'gpb-fl--item-buffer)

  (when (and gpb-fl--item-buffer (buffer-live-p gpb-fl--item-buffer))
    (with-current-buffer gpb-fl--item-buffer
      (gpb-fl--echo-item-1))))

(defun gpb-fl--echo-item-1 ()
  (let ((item (and gpb-fl--selection-overlay
                   (get-text-property
                    (overlay-start gpb-fl--selection-overlay) 'item)))
        (message-log-max nil))
    (message "%s" (and item (or (plist-get (cdr-safe item) :echo)
                                (car-safe item)
                                item)))))

(defun gpb-fl--try-abort-recursive-edit (&rest args)
  (ignore-errors (abort-recursive-edit)))


(provide 'gpb-filtered-list)
