;;
;; Emacs rectangular mark mode with hjkl movement keys.
;;
;; `gpb-enter-rectangle-mark-mode' to enter mode.
;; h,j,k,l to move around.  y/c/d to VIM yank/change/delete.
;; Also accepts C-c and C-x for yank and delete.
;;

(defun gpb-rmm-wrap (f &optional continue)
  "Wrap commands to manage the overriding map and clean up.

If continue is t we leave the keymap activated."
  `(lambda ()
     (interactive)
     (call-interactively #',f)
     (if ,continue
         (set-temporary-overlay-map gpb-rectangle-mark-mode-map)

      ;; Done. 
       (rectangle-mark-mode -1)
       (deactivate-mark)
       (when evil-mode
         (evil-normal-state)
         (when (> (current-column) 0)
           (backward-char)))

       ;; You have to do this at a a delay or it is immediately hidden by a
       ;; message about the mark being deactivated.
       (cond
        ((eq #',f #'copy-rectangle-as-kill)
         (gpb-rmm-new-kill)
         (run-at-time 0.1 nil (lambda () (message "Copied region"))))
        ((eq #',f #'kill-rectangle)
         (gpb-rmm-new-kill)
         (run-at-time 0.1 nil (lambda () (message "Cut region"))))))))


(defun gpb-rmm-new-kill (&optional rect)
  "Add rectangle RECT to `kill-ring'.

RECT defaults to `killed-rectangle'."
  (let* ((rect (or rect killed-rectangle))
         (str (mapconcat 'identity rect "\n"))
         (handler (list #'insert-rectangle rect)))
    (kill-new (propertize str 'yank-handler handler))))

    
(defvar gpb-rectangle-mark-mode-map
  (let ((keymap (make-sparse-keymap)))
    ;; Movement
    (define-key keymap "h" (gpb-rmm-wrap #'rectangle-left-char t))
    (define-key keymap "j" (gpb-rmm-wrap #'rectangle-next-line t))
    (define-key keymap "k" (gpb-rmm-wrap #'rectangle-previous-line t))
    (define-key keymap "l" (gpb-rmm-wrap #'rectangle-right-char t))

    ;; Actions
    (define-key keymap "i"    (gpb-rmm-wrap #'string-rectangle))
    (define-key keymap "c"    (gpb-rmm-wrap #'string-rectangle))
    (define-key keymap "y"    (gpb-rmm-wrap #'copy-rectangle-as-kill))
    (define-key keymap "\C-c" (gpb-rmm-wrap #'copy-rectangle-as-kill))
    (define-key keymap "d"    (gpb-rmm-wrap #'kill-rectangle))
    (define-key keymap "\C-x" (gpb-rmm-wrap #'kill-rectangle))
    (define-key keymap " "    (gpb-rmm-wrap #'clear-rectangle))
    (define-key keymap (kbd "C-g") #'exit-recursive-edit)

    ;; Default stays in the keymap
    (define-key keymap [t] #'(lambda () (interactive)
                               (message "Exit rectangle mode with CTRL-G")
                               ;; Keep the keymap active.
                               (set-temporary-overlay-map
                                gpb-rectangle-mark-mode-map)))
    keymap))


(defun gpb-enter-rectangle-mark-mode ()
  "Enter rectangle mark mode with VIM hjkl movement"
  (interactive)
  (let ((evil-mode (boundp 'evil-mode))) 
    (when evil-mode (evil-emacs-state))
    (rectangle-mark-mode 1)
    (set-temporary-overlay-map gpb-rectangle-mark-mode-map)
    (setq-local command-message nil)
    (message "Select rectangle: ")))
      

(provide 'gpb-rectangle-mark-mode)
