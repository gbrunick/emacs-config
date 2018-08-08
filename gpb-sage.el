(setq gpb-sage-path "/usr/local/share/sage-4.8")

;;      (concat "/home/gbrunick/src/"
;;             "sage-4.5-linux-64bit-ubuntu_10.04_lts-x86_64-Linux"

(add-to-list 'load-path (concat gpb-sage-path "/data/emacs"))
(setq sage-command (concat gpb-sage-path "/sage"))

(load "sage")
(load "sage-view")
(load "gpb-python")

;; Override the fact that sage makes sage-buffer an alias for python-buffer
(setq sage-buffer nil)

;; Add more space between lines of math.
(setq sage-view-latex-head
      (with-temp-buffer
        (insert sage-view-latex-head)
        (goto-char 0)
        (replace-string
         "\\begin{document}"
         (concat
          "\\usepackage{setspace}\n\\onehalfspacing\n"
          "\\setlength{\\PreviewBorder}{1ex}"
          "\\begin{document}\n"))
        (buffer-string)))

(defun gpb-sage-save-and-send ()
  (interactive)
  (save-buffer)
  (sage-send-buffer))

(define-key sage-mode-map [(control c) (control c)]
  'gpb-sage-save-and-send)

(defun gpb-inferior-sage-mode-init ()
  (interactive)
  ;; (run-sage nil nil t)
  ;; (switch-to-buffer sage-buffer)
  (gpb-comint-make-previous-output-readonly))

(add-hook 'inferior-sage-mode-hook 'gpb-inferior-sage-mode-init)

(add-hook 'sage-startup-after-prompt-hook 'gpb-on-sage-startup-after-prompt)
(defun gpb-on-sage-startup-after-prompt ()
  (message "Running gpb-on-sage-startup-after-prompt...")
  (sage-view)
  (python-send-receive-multiline "use_emacs_display_hook();")
  ;;(python-send-receive-multiline "colors LightBG")
  (setq sage-view-scale 1.75)
  (sage-view-disable-inline-plots)
  (font-lock-add-keywords
   nil '(("^ *out:" . 'font-lock-type-face)))
  (rename-buffer (generate-new-buffer-name "*sage*"))
)
