;; gpb-packages.el
;;
;; Tools to install and manage packages in a local version controlled
;; package archive.  The list of packages is recorded in the version
;; controlled file external-packages/selected-packages.txt.
;;
;; Run `gpb-ensure-packages-installed' in your init file to ensure that
;; exactly the packages listed in `selected-packages' are installed from
;; the tar files in external-packages/.
;;
;; To upgrade or install new packages, use `list-packages' in the usual
;; way, run `gpb-update-local-elpa' to update the local package archive and
;; then commmit the changes to version control.
;;

(require 'package)

;; We store all required packages in a local package repository for
;; reproducibility.
(setq gpb-local-elpa (expand-file-name "external-packages"
                                       (file-name-directory
                                        (or load-file-name (buffer-file-name))))
      gpb-selected-packages-file (expand-file-name "selected-packages.txt"
                                                   gpb-local-elpa)
      gpb-selected-packages (read (with-temp-buffer
                                    (insert-file-contents
                                     gpb-selected-packages-file)
                                    (buffer-string))))


(defun gpb-ensure-packages-installed ()
  "Ensure that all packages in the local package archive are installed."
  ;; If have any missing packages, we install them all.
  (let ((package-archives `(("gpb-local-elpa" . ,gpb-local-elpa)))
        (uninstalled-packages (cl-remove-if #'package-installed-p
                                            gpb-selected-packages)))
    (when uninstalled-packages
      (package-refresh-contents)
      (dolist (pkg gpb-selected-packages)
        (package-install pkg)))))


(defun gpb-update-local-elpa (&optional arg)
  "Store the current set of external packages.

With a prefix argument, we recreate the mirror directory."
  (interactive "P")
  (elpamr-create-mirror-for-installed gpb-local-elpa arg)
  (with-temp-buffer
    (insert (pp-to-string (cl-sort package-selected-packages #'string<)))
    (write-file gpb-selected-packages-file)))


(provide 'gpb-packages)
