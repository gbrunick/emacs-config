;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Customize dired
;;

;; Put the following stub in .emacs
;; (eval-after-load "dired" '(require 'gpb-dired))

(require 'dired)
(require 'dired-x)
(require 'dired-single)

(autoload 'wdired-change-to-wdired-mode "wdired")

;;(message "Loading gpb-dired...")

(setq dired-listing-switches "-ahGl"
      ;; original value "^\\.?#\\|^\\.$\\|^\\.\\.$")
      dired-omit-files "^\\.?#")

(add-hook 'dired-mode-hook 'gpb-dired-mode-hook)

(defun gpb-dired-mode-hook ()
  (dired-omit-mode 1)
  (if gpb-dired-default-omit-mode
      (dired-omit-mode 1)
    (dired-omit-mode -1))
  (hl-line-mode 1))


(defun gpb-dired-spellcheck-filename ()
  (interactive)
  (wdired-change-to-wdired-mode)
  (ispell-word)
  (wdired-finish-edit))

(define-key dired-mode-map [return] 'gpb-dired-visit-file)
(define-key dired-mode-map [mouse-1] nil)
(define-key dired-mode-map [double-mouse-1] 'dired-single-buffer-mouse)
(define-key dired-mode-map [(r)] 'dired-toggle-read-only)
(define-key dired-mode-map [(meta $)] 'gpb-dired-spellcheck-filename)
(define-key dired-mode-map "h" 'dired-omit-mode)
(define-key dired-mode-map "j" 'dired-next-line)
(define-key dired-mode-map "k" 'dired-previous-line)
(define-key dired-mode-map [(control o)] nil)
;; (define-key dired-mode-map [remap keyboard-quit] 'quit-window)

(defun gpb-dired-visit-file ()
  (interactive)
  (let* ((filename (dired-get-filename nil t))
         (ext (file-name-extension filename)))
    (cond
     ((and (string-equal ext "pdf")
           (string-equal system-type "gnu/linux"))
      (shell-command (format "xdg-open \"%s\"" filename)))
     (t
      (dired-single-buffer)))))

(define-key dired-mode-map "^"
  (defun gpb-dired-goto-parent ()
    (interactive)
    (dired-single-buffer "..")))

(defvar gpb-dired-default-omit-mode t
  "Should some files be hiden when dired is opened?")

(define-key dired-mode-map [(h)]
  (defun toggle-omit-mode ()
    (interactive)
    (dired-omit-mode)
    (setq gpb-dired-default-omit-mode dired-omit-mode)))
  ;; (setq gpb-hide-hidden-files (not gpb-hide-hidden-files))
  ;; (make-local-variable 'files-hidden)
  ;; (dired-mark-files-regexp "\\..*")
  ;; (dired-do-kill-lines)
  ;; )

(defun gpb-dired-xdg-open  (&optional file-list)
  (interactive
   (list (dired-get-marked-files t current-prefix-arg)))
  (apply 'call-process "xdg-open" nil 0 nil file-list))

(defun gpb-dired-cut-files  (&optional file-list)
  (interactive
   (list (dired-get-marked-files t current-prefix-arg)))
  (apply 'call-process-shell-command "clip-files" nil 0 nil
         (cons "--cut" file-list)))

(defun gpb-dired-copy-files  (&optional file-list)
  (interactive
   (list (dired-get-marked-files t current-prefix-arg)))
  (apply 'call-process-shell-command "clip-files" nil 0 nil
         (cons "--copy" file-list)))

(defun gpb-dired-open-all-marked-files ()
  (interactive)
  (let ((dired-buffer (current-buffer)))
    (dolist (filename (reverse (dired-get-marked-files)))
      (find-file filename))
    (with-current-buffer dired-buffer
      (when (derived-mode-p 'dired-mode)
        (dired-unmark-all-marks)))))

(defun gpd-dired-generate-context-menu ()
  (let* ((marked-files (dired-get-marked-files t))
         (len (length marked-files))
         (str (if (> len 1) (format "%i marked files" len)
                (car marked-files)))
         (plural (if (> len 1) "s" "")))
    `((,str nil :disabled :centered)
      ("Edit with emacs" gpb-dired-open-all-marked-files)
      ("Open with xdg-open" gpb-dired-xdg-open)
      (,(format "Cut file%s" plural) gpb-dired-cut-files)
      (,(format "Copy file%s" plural) gpb-dired-copy-files)
      ("Spell check name" gpb-dired-spellcheck-filename)
      ("Buffer Settings" nil :disabled :centered)
      ,(when (eq major-mode 'dired-mode)
             `("Begin renaming files" dired-toggle-read-only))
      ,(when (eq major-mode 'wdired-mode)
             `("Commit changeds" (wdired-s change-to-wdired-mode)))
      ,(if dired-omit-mode
           `("Unhide files" dired-omit-mode)
         `("Hide files" dired-omit-mode)))))

(provide 'gpb-dired)
