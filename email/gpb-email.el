(require 'notmuch)

(eval-after-load "notmuch"
  '(progn
     (setq notmuch-search-oldest-first nil)
     (define-key notmuch-search-mode-map "" 'gpb-show-thread)
     (setq notmuch-search-result-format
           `(("date" . "%s ")
             ("subject" . "%s\n")
             ("count" . "%12s ")
             ("authors" . "%-100s")
             ;;("tags" . "(%s)")
             ))))


(defun gpb-show-thread ()
  "Display the currently selected thread."
  (interactive)
  (let ((thread-id (notmuch-search-find-thread-id))
        ;; (subject (notmuch-search-find-subject))
        ;; (dir (make-temp-file nil t))
        ;; files
        )

    (browse-url (format "http://localhost:5001/notmuch/thread?id=%s"
                        (replace-regexp-in-string "^thread:" "" thread-id)))))

  ;;   (when (> (length thread-id) 0)
  ;;     (cd dir)
  ;;     (save-window-excursion
  ;;       (shell-command
  ;;        (format "notmuch show --format=mbox %s > %s/thread.mbox" thread-id dir)
  ;;        "*format-mbox*")
  ;;       (shell-command
  ;;        (format "mhonarc -rcfile ~/src/emacs/mhonarc.rc %s/thread.mbox" dir)
  ;;        "*format-mbox*")
  ;;       (shell-command (format "~/src/emacs/concat-msgs %s" dir) "*format-mbox*")
  ;;       ;; (shell-command (format "show-mbox %s/thread.mbox" dir))
  ;;       ;; (switch-to-buffer "*Shell Command Output*"))
  ;;       (browse-url (format "file:///%s/messages.html" dir ))))
  ;;   (message "dir: %s" dir))
  ;; )

(defun gpb-show-thread-old ()
  "Display the currently selected thread."
  (interactive)
  (let ((thread-id (notmuch-search-find-thread-id))
        (subject (notmuch-search-find-subject))
        (dir (make-temp-file nil t))
        files)
    (when (> (length thread-id) 0)
      (cd dir)
      (shell-command
       (format "notmuch show --format=mbox %s | hypermail -i -d \"%s\" -l \"%s\""
               thread-id dir subject))
      (setq files (directory-files dir nil "[0-9][0-9][0-9][0-9].html"))
      ;; (dolist (f (sort files (lambda (x y) (not (string-lessp x y)))))
      ;;   (browse-url (format "file:///%s/%s" dir f))
      ;;   (sit-for 0.1))
      ;; The follow sed script replaces
      ;;    <dfn>From</dfn>
      ;; with
      ;;    <hr /><dfn>From<\\/dfn>
      ;; to produce a horizontal line between messages.
      (shell-command
       (concat "sed \"s/<dfn>From<\\/dfn>/<hr \\><dfn>From<\\/dfn>/\" "
               "< thread_body1.html > thread_body1a.html"))

      (browse-url (format "file:///%s/thread_body1a.html" dir ))
      (message "Dir: %s" dir))))

  ;;       (notmuch-show thread-id
  ;;                     (current-buffer)
  ;;                     notmuch-search-query-string
  ;;                     ;; name the buffer based on notmuch-search-find-subject
  ;;                     (if (string-match "^[ \t]*$" subject)
  ;;                         "[No Subject]"
  ;;                       (truncate-string-to-width
  ;;                        (concat "*"
  ;;                                (truncate-string-to-width subject 32 nil nil t)
  ;;                                "*")
  ;;                        32 nil nil t))
  ;;                     crypto-switch)
  ;;     (error "End of search results"))))
  ;; (interactive)
  ;; (let ((thread (aget (text-properties-at (point)) 'notmuch-search-thread-id)))
  ;;   (message thread)))


;; notmuch show --format=mbox thread:000000000000332a | hypermail -i
(defun gpb-color-search-results ()
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((ov (make-overlay (point)
                              (save-excursion
                                (forward-line 2)
                                (point)))))
        (overlay-put ov 'face `((background-color
                                 . "#daf1ff")))
        (forward-line 4)))))

(defun search-email (arg)
  "Search mail using notmuch"
  (interactive "MSearch email for (default last six months): ")
  (if (string-equal arg "")
      (let* ((time (current-time))
             ;; combine high and low bits of (current-time)
             (time2 (+ (* (car time) (expt 2 16)) (cadr time)))
             (secs-in-6months (* 60 60 24 30 6)))
        (notmuch-search (format "%s..%s"
                                (- time2 secs-in-6months)
                                (+ time2 secs-in-6months) nil)))
    (notmuch-search arg notmuch-search-oldest-first)
    (set-process-sentinel (get-buffer-process (current-buffer))
                          'gpb-notmuch-search-process-sentinel)
    ;;(run-at-time 2 nil 'gpb-color-search-results)
    (hl-line-mode -1)
    (local-set-key [remap previous-line]
                   (lambda () (interactive)
                     (previous-line 2)))
    (local-set-key [remap next-line]
                   (lambda () (interactive)
                     (next-line 2)))))

(defun gpb-notmuch-search-process-sentinel (proc msg)
  (let ((buffer (process-buffer proc))
        (status (process-status proc))
        (exit-status (process-exit-status proc)))
    (when (memq status '(exit signal))
      (gpb-color-search-results)))
  (notmuch-search-process-sentinel proc msg)
  (message "Search completed"))

(defvar gpb-download-email--buffer-name "*Downloading email*")
(defvar gpb-download-email--callback nil)

(defun gpb-download-email--sentinel (proc state)
  (when (string-equal state "finished\n")
    (with-selected-window (get-buffer-window gpb-download-email--buffer-name)
      (funcall gpb-download-email--callback))))
;; (select-window cur-win)))

(defun gpb-download-email (&optional callback)
  (interactive)
  (pop-to-buffer gpb-download-email--buffer-name)
  (with-current-buffer gpb-download-email--buffer-name
    (erase-buffer)
    (insert "Checking for new email..\n"))
  (let ((proc (start-process-shell-command "*download-email*"
                                           gpb-download-email--buffer-name
                                           "download-email")))
    (when callback
        (setq gpb-download-email--callback callback)
        (set-process-sentinel proc 'gpb-download-email--sentinel))))

(defun inbox ()
  (interactive)
  (if (y-or-n-p-with-timeout "Download new email? " 2 t)
      (gpb-download-email 'notmuch)
    (notmuch)))

(provide 'gpb-email)
