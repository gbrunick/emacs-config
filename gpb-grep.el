;;
;; grep customization
;;
;; Make it easier to exclude diretories and file patterns.
;;


(defcustom gpb-grep-exclude-dirs
  '("\\.git")
  "A list of globs.  Matching directories are excluded by `gpb-grep'."
  :type '(repeat string))

(defcustom gpb-grep-exclude-files
  '("TAGS" "*~" "#*#")
  "List of globs.  Maching filenames are exclude by `gpb-grep'."
  :type '(repeat string))

(defun gpb-grep (dir regex)
  (interactive (list
                (read-directory-name "Directory: ")
                (read-from-minibuffer "Regex: "
                                      (and mark-active
                                           (buffer-substring-no-properties
                                            (region-beginning) (region-end)))
                                      nil
                                      nil
                                      'grep-history)))

  (let* ((compilation-ask-about-save nil)
         (default-directory dir)
         (make-args (lambda (argname globs)
                      (mapconcat (lambda (x)
                                   (format "%s=\"%s\" " argname x))
                                 globs
                                 "")))
         (exclude-dirs (funcall make-args "--exclude-dir" gpb-grep-exclude-dirs))
         (exclude-files (funcall make-args "--exclude" gpb-grep-exclude-files))
         (cmd (cl-case (window-system)
                (w32
                 (concat "grep -RinHI -E "
                         (format "\"%s\" " regex)
                         "--line-buffered "
                         exclude-dirs
                         exclude-files
                         "."))
                (otherwise
                 (concat "grep -RinH -E "
                         (format "\"%s\" " regex)
                         exclude-dirs
                         exclude-files
                         ".")))))
    (grep cmd)))


(provide 'gpb-grep)
