;; Code to integrate emacs with linux recent files list

(defvar gpb-rf-recent-file-list nil
  "A list of recently open files.")

(defvar gpb-rf-enable-record-file-on-visit t
  "Should we attempt to update the system recent file list when
emacs visits a file?")

(defvar gpb-rf-ignore-extensions '("cache" "ede" "recentf")
  "If the extension of a visited file is in this list, then it is
not added to the recent file list.")

;; (defvar gpb-rf-system-type
;;   (cond
;;    ((file-exists-p (expand-file-name "~/.recently-used.xbel"))
;;     'gnome)
;;    (t
;;     'not-implemented))
;;   "Which recent file system should you use.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  The main function
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (gpb-rf-refresh-list)
(defun gpb-rf-refresh-list ()
  "Update the variable `gpb-rf-recent-file-list'"
  (with-current-buffer (get-buffer-create "*Recent Documents*")
    (erase-buffer)
    (setq gpb-rf-recent-file-list nil)
    (when (= (call-process-shell-command "list-recent-documents" nil t) 0)
      (goto-char (point-max))
      (while (not (bobp))
        (setq gpb-rf-recent-file-list
              (cons (buffer-substring-no-properties
                     (progn (forward-line -1) (point))
                     (save-excursion (forward-line 1) (backward-char) (point)))
                    gpb-rf-recent-file-list))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Keep the local and system list updated as emacs visits files.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gpb-rf-add-file-to-recent-list (&optional filename)
  (let* ((filename (or filename (buffer-file-name)))
         (ext (file-name-extension filename)))
    (unless (or (not gpb-rf-enable-record-file-on-visit)
                (member ext gpb-rf-ignore-extensions)
                (string-match "/tmp/hg-editor.*" filename)
                (and (not (file-exists-p filename))
                     (message "File %s doesn't exist" filename)))
      (setq gpb-rf-recent-file-list
            (cons filename (remove filename gpb-rf-recent-file-list)))
      ;;(message "Adding %s to recent documents" (buffer-file-name))
      (start-process-shell-command
       "add-recent-file" "*add-recent-file output*"
       (format "add-recent-file %s %s %s '%s'"
               "--mime-type 'text/plain'"
               "--app-name emacs"
               "--app-exec emacs-edit"
               (buffer-file-name))))))

;; (setq find-file-hook nil after-save-hook nil)
(add-hook 'find-file-hook 'gpb-rf-add-file-to-recent-list)
(add-hook 'after-save-hook 'gpb-rf-add-file-to-recent-list)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Code for interacting with the list
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defvar gpb-rf-list-files-ignore-regex '(".*\\.pdf$")
;;   "If a filename matches one of these regular expressions, it is not listed
;; as on option by gpb-rf-ido-find-recent-file and gpb-rf-list-files.")

;; (defun gpb-rf--pruned-list ()
;;   ;; TODO

;; (gpb-rf-ido-find-recent-file)
(defun gpb-rf-ido-find-recent-file ()
  "Find a recent file using Ido."
  (interactive)
  (let* ((alist (mapcar (lambda (x) (cons (file-name-nondirectory x) x))
                        gpb-rf-recent-file-list))
         (basename (ido-completing-read "File name: "
                                     (mapcar 'car alist) nil nil)))
    (find-file (aget alist basename))))

(defvar gpb-rf-debug-flag nil)
(defun gpb-rf-toggle-debug ()
  (interactive)
  (setq gpb-rf-debug-flag
        (not gpb-rf-debug-flag))
  (gpb-rf-list))


(defvar gpb-rf-wide-flag nil)
(defun gpb-rf-toggle-wide ()
  (interactive)
  (setq gpb-rf-wide-flag
        (not gpb-rf-wide-flag))
  (gpb-rf-list-files))

(defun gpb-rf-open-file ()
  (interactive)
  (let ((filename (get-text-property (point) 'filename))
        (this-buffer (current-buffer)))
    (when filename
      (if (not (file-exists-p filename))
          (error "File no longer exists.")
        (kill-buffer)
        (find-file filename)))))

(defun gpb-rf-make-gray (str)
  (add-text-properties 0 (length str)
                       `(face ((foreground-color . "gray60"))) str)
  str)

;; (gpb-rf-list-files)
(defun gpb-rf-list-files(&optional wide)
  (interactive)
  (switch-to-buffer (get-buffer-create "*Recent Files*"))
  (toggle-read-only -1)
  (erase-buffer)
  (insert "RET to open, w toggle width, q to quit.\n\n")
  ;; (when gpb-rf-recent-file-list
  ;;   (insert (car gpb-rf-recent-file-list)))
  (let ((first t) path base)
    (dolist (file-name gpb-rf-recent-file-list)
      ;;(insert filename)
      (setq file-name (replace-regexp-in-string
                       (concat "^" (expand-file-name "~"))  "~"
                       file-name))
      (setq file-name (concat (gpb-rf-make-gray (file-name-directory file-name))
                              (file-name-nondirectory file-name)))
      (when (and ;; If you lose the connection to a gnome virtual
             ;; filesystem but don't unmount, then file-exists-p
             ;; hangs when you check file on the virtual
             ;; filesystem.
             ;; (not (gpb-util-starts-with file-name "~/.gvfs"))
             ;; (not (gpb-util-starts-with file-name "smb://"))
             ;; (not (gpb-util-starts-with file-name "ftp://"))
             ;; (not (gpb-util-starts-with file-name "~/nav-storage"))
             (file-exists-p file-name))
        (if first (setq first nil) (insert "\n"))
        (insert (concat
                 (propertize
                  (if (not gpb-rf-wide-flag)
                      (gpb-util-truncate-string file-name 'center 70)
                    file-name)
                  'filename file-name)))))
    (goto-char (point-min))
    (forward-line 2)
    ;; (save-excursion
    ;;   (replace-regexp (concat "^" (expand-file-name "~"))  "~"))
    ;; (while (not (eobp))
    ;;   (let ((beg (point)) filename)
    ;;     (forward-line 1)
    ;;     (setq file-name (buffer-substring beg (1- (point))))
    ;;     (delete-region beg (point))
    (toggle-read-only 1)
    ;; (goto-char (point-min))
    ;; (forward-line 2)
    (local-set-key "" 'gpb-rf-open-file)
    (local-set-key "q" (lambda () (interactive) (kill-buffer)))
    (local-set-key "w" 'gpb-rf-toggle-wide)
    (local-set-key "r" 'gpb-rf-refresh-file-list)
    (local-set-key "g" 'gpb-rf-refresh-file-list)
    ;;(local-set-key "d" 'gpb-rf-toggle-debug)
    ))

(defun gpb-list-files(&optional wide)
  (interactive)
  ;; (when gpb-rf-recent-file-list
  ;;   (insert (car gpb-rf-recent-file-list)))
  (let ((first t) (dir default-directory) path base)
    (switch-to-buffer (get-buffer-create "*Recent Files*"))
    (toggle-read-only -1)
    (erase-buffer)
    (insert "RET to open, w toggle width, q to quit.\n\n")
    (dolist (file-name (append
                        (when default-directory
                          (directory-files default-directory t))
                        gpb-rf-recent-file-list))
      ;;(insert filename)
      (setq file-name (replace-regexp-in-string
                       (concat "^" (expand-file-name "~"))  "~"
                       file-name))
      (setq file-name (concat (gpb-rf-make-gray (file-name-directory file-name))
                              (file-name-nondirectory file-name)))
      (when (and ;; If you lose the connection to a gnome virtual
             ;; filesystem but don't unmount, then file-exists-p
             ;; hangs when you check file on the virtual
             ;; filesystem.
             ;; (not (gpb-util-starts-with file-name "~/.gvfs"))
             ;; (not (gpb-util-starts-with file-name "smb://"))
             ;; (not (gpb-util-starts-with file-name "ftp://"))
             ;; (not (gpb-util-starts-with file-name "~/nav-storage"))
             (file-exists-p file-name))
        (if first (setq first nil) (insert "\n"))
        (insert (concat
                 (propertize
                  (if (not gpb-rf-wide-flag)
                      (gpb-util-truncate-string file-name 'center 70)
                    file-name)
                  'filename file-name)))))
    (goto-char (point-min))
    (forward-line 2)
    ;; (save-excursion
    ;;   (replace-regexp (concat "^" (expand-file-name "~"))  "~"))
    ;; (while (not (eobp))
    ;;   (let ((beg (point)) filename)
    ;;     (forward-line 1)
    ;;     (setq file-name (buffer-substring beg (1- (point))))
    ;;     (delete-region beg (point))
    (toggle-read-only 1)
    ;; (goto-char (point-min))
    ;; (forward-line 2)
    (local-set-key "" 'gpb-rf-open-file)
    (local-set-key "q" (lambda () (interactive) (kill-buffer)))
    (local-set-key "w" 'gpb-rf-toggle-wide)
    ;;(local-set-key "r" 'gpb-rf-refresh-file-list)
    ;;(local-set-key "g" 'gpb-rf-refresh-file-list)
    ;;(local-set-key "d" 'gpb-rf-toggle-debug)
    ))
;;(switch-to-buffer "*Recent Files*"))

(defun gpb-rf-refresh-file-list ()
  (interactive)
  (gpb-rf-refresh-list)
  (gpb-rf-list-files))

(gpb-rf-refresh-list)
(provide 'gpb-recent-files)
