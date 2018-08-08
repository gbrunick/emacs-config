;; Customization of company mode.

(require 'company)

;;(setq company-backends nil) ; No global backends
(setq company-backends (delq 'company-ropemacs company-backends))

(setq company-idle-delay nil) ; Don't auto-complete
(setq company-begin-commands nil) ; Don't auto-complete
(define-key company-active-map "\t" 'gpb-company-tab-command)
(define-key company-active-map [(tab)] 'gpb-company-tab-command)
(define-key company-active-map (kbd "C-k") 'company-select-previous)
(define-key company-active-map (kbd "C-j") 'company-select-next)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-f") 'company-filter-candidates)
(define-key company-active-map [(escape)] 'company-abort)
(define-key company-active-map [remap isearch-backward]
  'show-isearch-warning)
(define-key company-active-map [remap isearch-forward]
  'show-isearch-warning)

;; Don't do special case visualization for the one completion case.
(setq company-frontends '(company-pseudo-tooltip-frontend
                          company-echo-metadata-frontend))

;; (set-face-attribute 'company-tooltip nil :background "grey85")
;; (set-face-attribute 'company-tooltip-selection nil :background "#a0b0f0")

(defun show-isearch-warning ()
  (interactive)
  (message "You can't start an isearch when company mode is active."))


(defun gpb-company-try-to-complete-common ()
  (interactive)
  (unless (looking-back "[ \t\n]")
    (setq company-prefix nil
          company-candidates nil)
    (company-complete-common)
    (when company-candidates
      (delete-region (point) (save-excursion (skip-syntax-forward "w_") (point)))
      t)))


(defun gpb-company-switch-to-completion-buffer ()
  (interactive)
  (let ((beg (- (point) (length company-prefix)) )
        (end (point))
        (completions company-candidates))
    (company-abort)
    (completion-in-region beg end completions)
    (let ((win (get-buffer-window "*Completions*")))
      (when win
        (select-window win)
        (next-completion 1)))))

(defun gpb-company-tab-command (arg)
  (interactive "P")
  (cond
   (arg
    (gpb-company-switch-to-completion-buffer))
   (t
    (company-complete-common))))


;; (defun gpb-company-self-insert-and-then-complete ()
;;   "Electic keys."
;;   (interactive)
;;   (insert-char last-command-event 1)
;;   (company-manual-begin))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The following implements a backspace feature for incremental
;; searching of completions in company mode.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define-key company-search-map [backspace]
;;   'gpb-company-search-remove-char)

;; (defun gpb-company-search-remove-char ()
;;   "Remove one letter from the search string.  If there is are no
;; letters in the search string, exit search."
;;   (interactive)
;;   (company-search-assert-enabled)
;;   (let ((len (length company-search-string))
;;         pos)
;;     (if (= len 0)
;;         (company-search-abort)
;;       ;; The rest is taken from company-search-printing-char
;;       (setq company-search-string
;;             (substring company-search-string 0 (1- len))
;;             company-search-lighter
;;             (concat " Search: \"" company-search-string "\"")
;;             pos (company-search company-search-string
;;                                 (nthcdr company-selection
;;                                         company-candidates)))
;;         (if (null pos)
;;             (ding)
;;           (company-set-selection (+ company-selection pos) t)))))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; The following implements search for preceding word functionality in
;; ;; company mode.
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; We add company-search-kill-others to company-search-map as an
;; ;; (fake) event that calls itself.  We do this so that we can push
;; ;; this event onto the end of unread-command-events and have the
;; ;; functional called after all the previous input events have been
;; ;; handled.  It is slightly simpler to just push the key strokes onto
;; ;; the event stack that are already bound to the function that we want
;; ;; to call, but this would break mysteriously if the user changes the
;; ;; keybindings.  The fake event route below seems more robust.
;; (define-key company-search-map [company-search-kill-others]
;;   'gpb-company--call-event-as-function)

;; (defun gpb-company--call-event-as-function ()
;;   "Bind this to a fake event which is a function to call the
;; function when the event is processed."
;;   (interactive)
;;   ;;(message "Entering gpb-company--call-event-as-function-callback...")
;;   ;;(message "last-command-event: %S" last-command-event)
;;   (assert (fboundp last-command-event))
;;   ;;(message "Calling %S with no arguments" last-command-event)
;;   (funcall last-command-event))

;; (defun gpb-company-search-and-replace-word ()
;;   "Starts company mode by removing the the word before point,
;; starting completion with an empty prefix, and then immediately
;; filtering out the results which do not contain the word.

;; If the point is not following a word, just open completion in
;; filter mode.

;; The terminology should probably be changed here.  Maybe
;; filter-previous-word?"
;;   (interactive)
;;   (if (not (looking-back "[[:word:]]" (1- (point))))
;;       (progn
;;         (company-manual-begin)
;;         (company-filter-candidates))
;;     (let* ((beg (save-excursion (backward-word) (point)))
;;            (end (point)))
;;       (set (make-variable-buffer-local 'gpb-company-searching-for)
;;            (buffer-substring-no-properties beg end))
;;       (delete-region beg end)
;;       ;; If the user aborts the completion, we put back the word that we
;;       ;; removed from the buffer.
;;       (add-hook 'company-completion-cancelled-hook
;;                 'gpb-company--search-cancelled-callback
;;                 nil t) ; buffer local hook
;;       (company-manual-begin)
;;       (company-search-candidates)
;;       (setq unread-command-events
;;             (nconc unread-command-events
;;                    ;; The append below converts the string
;;                    ;; gpb-company-searching-for to a list of characters.
;;                    ;; We add each character to unread-command-events
;;                    ;; individually to simulate user input.  Don't remove
;;                    ;; the nil or things break.
;;                    (append gpb-company-searching-for nil)
;;                    ;; Then we simulate the user calling
;;                    ;; company-search-kill-others
;;                    (list 'company-search-kill-others)
;;                    ))
;;       (message "gpb-company-search-and-replace-word")
;;       (message "  pending unread-command-events: %S"
;;                unread-command-events))))

;; (defun gpb-company--search-cancelled-callback (explicit-action)
;;   (when gpb-company-searching-for
;;     (insert gpb-company-searching-for)
;;     (setq gpb-company-searching-for nil)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;;  Convenience for pop-ing up a completion list
;; ;;
;; ;; (defun on-success (arg) (insert (format "Success: %S" arg)))
;; ;; (defun on-failure (arg) (insert (format "Failure: %S" arg)))
;; ;; (gpb-show-completion-list '("eq:1" "eq:2" "eq:3")
;; ;;                           'on-success 'on-failure)
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defvar gpb-company-list--initial-point
;;   nil
;;   "Saves point where list was opened.")
;; (make-variable-buffer-local 'gpb-company-list--initial-point)

;; (defvar gpb-company-list--current-items
;;   nil
;;   "Saves list being shown.")
;; (make-variable-buffer-local 'gpb-company-list--current-items)

;; (defvar gpb-company-list--finished-callback
;;   nil)
;; (make-variable-buffer-local 'gpb-company-list--finished-callback)

;; (defvar gpb-company-list--cancelled-callback
;;   nil)
;; (make-variable-buffer-local 'gpb-company-list--cancelled-callback)

;; (defun gpb-show-completion-list (items &optional on-success-callback
;;                                        on-failure-callback initial-item
;;                                        dont-complete-common)
;;   (setq gpb-company-list--initial-point (point)
;;         gpb-company-list--current-items items)

;;   (setq gpb-company-list--finished-callback on-success-callback)
;;   (add-hook 'company-completion-finished-hook
;;             'gpb-company-list--call-finished-callback)

;;   (setq gpb-company-list--cancelled-callback on-failure-callback)
;;   (add-hook 'company-completion-cancelled-hook
;;             'gpb-company-list--call-cancelled-callback)

;;   (company-begin-backend 'gpb-company-list--show-list-backend)
;;   (when initial-item
;;     (when (not (integerp initial-item))
;;       (setq initial-item (position initial-item items)))
;;     (company-set-selection initial-item))
;;   (unless dont-complete-common
;;     (company-complete-common)))

;; (defun gpb-company-list--call-finished-callback (arg)
;;   (when gpb-company-list--finished-callback
;;     (funcall gpb-company-list--finished-callback arg))
;;   (gpb-company-list--remove-callbacks))

;;   ;; (remove-hook 'company-completion-finished-hook
;;   ;;              'gpb-company-list--call-finished-callback)
;;   ;; (remove-hook 'company-completion-cancelled-hook
;;   ;;              'gpb-company-list--call-cancelled-callback)
;;   ;; (setq gpb-company-list--initial-point nil
;;   ;;       gpb-company-list--current-items nil
;;   ;;       gpb-company-list--finished-callback nil
;;   ;;       gpb-company-list--cancelled-callback nil))

;; (defun gpb-company-list--call-cancelled-callback (arg)
;;   (when gpb-company-list--initial-point
;;     (delete-region gpb-company-list--initial-point (point)))
;;   (when gpb-company-list--cancelled-callback
;;     (funcall gpb-company-list--cancelled-callback arg))
;;   (gpb-company-list--remove-callbacks))

;; (defun gpb-company-list--remove-callbacks ()
;;   (remove-hook 'company-completion-finished-hook
;;                'gpb-company-list--call-finished-callback)
;;   (remove-hook 'company-completion-cancelled-hook
;;                'gpb-company-list--call-cancelled-callback)
;;   (setq gpb-company-list--initial-point nil
;;         gpb-company-list--current-items nil
;;         gpb-company-list--finished-callback nil
;;         gpb-company-list--cancelled-callback nil))

;; (defun gpb-company-list--show-list-backend (command &optional arg
;;                                                     &rest ignored)
;;   (interactive) ;;(list 'interactive))
;;   (case command
;;     ('prefix (buffer-substring-no-properties
;;               gpb-company-list--initial-point (point)))
;;     ('candidates gpb-company-list--current-items)
;;     ('meta (aget gpb-company-list--current-items arg))
;;     ('require-match t)
;;     ('sorted t)))

;; (provide 'gpb-company)
