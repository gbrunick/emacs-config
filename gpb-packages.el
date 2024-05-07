;; gpb-packages.el
;;
;; Tools to manage and install all files in a local package archive.
;;

(require 'package)

;; We store all required packages in a local package repository for
;; reproducibility.
(setq gpb-local-elpa (expand-file-name "external-packages"
                                       (file-name-directory
                                        (or load-file-name (buffer-file-name))))
      package-archives `(("gpb-local-elpa" . ,gpb-local-elpa))
      gpb-selected-packages (list 'evil 'evil-collection 'evil-goggles
                                  'ess 'poly-R
                                  'wgrep
                                  'elpa-mirror))


(defun gpb-update-local-elpa ()
  "Store the current set of external packages."
  (interactive)
  (unless (package-installed-p 'elpa-mirror)
    (let ((package-archives `(("gpb-local-elpa" . ,gpb-local-elpa))))
      (package-refresh-contents)
      (package-install "elpa-mirror")))
  (require 'elpa-mirror)
  (elpamr-create-mirror-for-installed gpb-local-elpa t))


(defun gpb-manage-packages ()
  "Use ELPA and MELPA to install new packages"
  (interactive)
  (setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                           ("melpa" . "https://melpa.org/packages/")))
  (package-refresh-contents)
  (list-packages))


(defun gpb-ensure-packages-installed ()
  "Ensure that all packages in the local package archive are installed."
  (dolist (pkg gpb-selected-packages)
    (unless (package-installed-p pkg)
      (package-install pkg t)))
  (package--save-selected-packages gpb-selected-packages))

(provide 'gpb-packages)
