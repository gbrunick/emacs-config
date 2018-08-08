(defun gpb-display-buffer-p (buffer action)
  (cond
   ((null action) t)
   (t nil)))

(defun gpb-display-buffer (buffer action)
  (or
   (display-buffer-reuse-window buffer (append '((reusable-frames . visible)
                                                 (inhibit-switch-frame . t)) action))
   (display-buffer-use-some-window buffer (append '((inhibit-same-window . t)
                                                    (reusable-frames . visible)
                                                    (inhibit-switch-frame . t)) action))))


(setq display-buffer-alist '((gpb-display-buffer-p gpb-display-buffer)))
