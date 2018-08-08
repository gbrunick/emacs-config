;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; auto-complete.el and popup.el customization
;;
;;

(require 'auto-complete)

(setq ac-auto-start nil
      ac-trigger-key nil
      ac-expand-on-auto-complete t
      ac-show-menu-immediately-on-auto-complete t)

(define-key ac-completing-map "\C-f" 'ac-isearch)
(define-key ac-completing-map "\C-j" 'ac-next)
(define-key ac-completing-map "\C-k" 'ac-previous)
(define-key ac-completing-map [(backtab)] 'ac-previous)

;; (define-key ac-completing-map "\C-f" 'popup-isearch)
;; (define-key ac-completing-map [remap popup-next] 'gpb-popup-next)
;; (define-key ac-completing-map [remap popup-previous] 'gpb-popup-previous)

;; Monkey patching to prevent scrolling from top to bottom.

(defun popup-next (popup)
  (let ((height (popup-height popup))
        (cursor (1+ (popup-cursor popup)))
        (scroll-top (popup-scroll-top popup))
        (length (length (popup-list popup))))
    (when (< cursor length)
      (cond
       ((>= cursor length)
        ;; Back to first page
        (setq cursor 0
              scroll-top 0))
       ((= cursor (+ scroll-top height))
        ;; Go to next page
        (setq scroll-top (min (1+ scroll-top) (max (- length height) 0)))))
      (setf (popup-cursor popup) cursor
            (popup-scroll-top popup) scroll-top)
      (popup-draw popup))))

(defun popup-previous (popup)
  (let ((height (popup-height popup))
        (cursor (1- (popup-cursor popup)))
        (scroll-top (popup-scroll-top popup))
        (length (length (popup-list popup))))
    (when (>= cursor 0)
      (cond
       ((< cursor 0)
        ;; Go to last page
        (setq cursor (1- length)
              scroll-top (max (- length height) 0)))
       ((= cursor (1- scroll-top))
        ;; Go to previous page
        (decf scroll-top)))
      (setf (popup-cursor popup) cursor
            (popup-scroll-top popup) scroll-top)
      (popup-draw popup))))
