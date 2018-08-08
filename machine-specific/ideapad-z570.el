;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Machine specific initialization
;;

(defun gpb-add-tree-to-load-path (root)
  "Add path and all subdirs of path to load-path"
  ;; (byte-recompile-directory root)
  (add-to-list 'load-path root)
  (let ((default-directory (expand-file-name root)))
    (normal-top-level-add-subdirs-to-load-path)))

(gpb-add-tree-to-load-path (expand-file-name "~/src/emacs"))

;; Version specific initialization
(cond
 ((eq emacs-major-version 23)
  (setq find-function-C-source-directory
        "/usr/local/src/emacs23-23.3+1/src"))
  ;;       byte-compile-dest-file-function
  ;;       (lambda (x) (format "~/lib/common/emacs23-elc/%sc"
  ;;                           (file-name-nondirectory x))))
  ;; (add-to-list 'load-path "~/lib/common/emacs23-elc"))

 ((eq emacs-major-version 24)
  (setq find-function-C-source-directory
        "/usr/local/src/emacs-snapshot-20121122/src"))
  ;;       byte-compile-dest-file-function
  ;;       (lambda (x) (format "~/lib/common/emacs24-elc/%sc"
  ;;                           (file-name-nondirectory x))))
  ;; (add-to-list 'load-path "~/lib/common/emacs24-elc"))
 )

;; Add directories to load path
;; (add-to-list 'load-path (gpb-get-dir "src/gpb-ipc"))
;; (let ((lib-dir (expand-file-name "~/lib/common/emacs-lisp"))
;;       (debug-on-error debug-on-error))
;;   ;; (byte-recompile-directory lib-dir 0)
;;   (gpb-add-tree-to-load-path lib-dir))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t :family "DejaVu Sans Mono" :height 96)))
 '(company-tooltip ((t (:background "grey85" :foreground "black"))))
 '(company-tooltip-selection ((t (:inherit company-tooltip :background "#a0b0f0"))))
 '(completions-common-part ((t (:inherit default :foreground "grey55"))))
 '(completions-first-difference ((t (:inherit default))))
 '(font-lock-builtin-face ((t (:foreground "dark slate blue"))))
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-comment-face ((t (:foreground "grey35" :slant normal))))
 '(font-lock-function-name-face ((t (:foreground "blue"))))
 '(font-lock-keyword-face ((t (:foreground "SeaGreen4"))))
 '(font-lock-string-face ((t (:foreground "sienna"))))
 '(highlight ((t (:background "gainsboro"))))
 '(region ((t (:background "pale goldenrod"))))
 '(success ((t (:foreground "forest green" :weight bold)))))
 ;; '(mode-line ((t (:inherit default :background "#2B2B2B" :foreground "#8FB28F" :box (:line-width -1 :color "#5F7F5F" :style released-button)))))
 ;; '(mode-line-highlight ((t nil)))
 ;; '(mode-line-inactive ((t (:box (:line-width -1 :color nil :style released-button) :foreground "#5F7F5F" :background "#383838" :inherit (default))))))
 ;; '(company-tooltip ((t (:background "grey85" :foreground "black"))))
 ;; '(company-tooltip-selection ((t (:inherit company-tooltip :background "#a0b0f0"))))
 ;; '(completions-common-part ((t (:inherit default :foreground "grey55"))))
 ;; '(completions-first-difference ((t (:inherit default))))
 ;; '(cursor ((t (:background "yellow2" :foreground "#DCDCCC"))))
 ;; '(mode-line ((t (:inherit default))))
 ;; '(mode-line-inactive ((t (:inherit default))))
 ;; '(region ((t (:background "tan3")))))

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:background "grey98" :foreground "black"
;;                 :family "DejaVu Sans Mono"
;;                 :height 96))))
;;  '(company-tooltip ((t (:background "grey85" :foreground "black"))))
;;  '(company-tooltip-selection ((t (:inherit company-tooltip :background "#a0b0f0"))))
;;  '(completions-common-part ((t (:inherit default :foreground "grey55"))))
;;  '(completions-first-difference ((t (:inherit default))))
;;  '(font-lock-builtin-face ((t (:foreground "dark slate blue"))))
;;  '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
;;  '(font-lock-comment-face ((t (:foreground "grey35"))))
;;  '(font-lock-function-name-face ((t (:foreground "Blue"))))
;;  '(font-lock-keyword-face ((t (:foreground "SeaGreen4"))))
;;  '(font-lock-string-face ((t (:foreground "sienna"))))
;;  '(highlight ((t (:background "gainsboro"))))
;;  '(region ((t (:background "pale goldenrod")))))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(compilation-message-face (quote default))
 '(custom-enabled-themes (quote (gpb-light)))
 '(custom-safe-themes
   (quote
    ("476473d9da096c65b9581f954927ef343918a027291da7d17a6c62c4a60ee54e" "a0044e375b8e2f62764d1cdf8beb07930de581f1e806de8488b3bcab015ad790" "3a6e10ffcb21d31f1ef277a1c939462fb6ef49890f5e49a76838b88a44be100a" "764777857ef24b4ef1041be725960172ac40964b9f23a75894a578759ba6652f" "9bae3dec09d6cb4511779a2f5a06d7b14ba49543ea4541dfa61675751e1432bd" "1fe2150695ddca371f34d8509250bdbd96b84b4002ad46175a3d40ca2290fd13" "ebb48666234fb8cd889220233964a98f823d69c2913d4c106c537e2a8c5a8766" "aeba93042c2b55c0d5998b74d565139d57d8f55443d7ac8ac1131f5ca8f31618" "99608087eb019bc707b0aca54b380eefc100212f875f77f75bf99dff858100c6" "150302f2806b68ea3639d12629fe5982986dd8b1e7b7cd5227e8d89eee5f2e10" "7b8fd6fec6b2fe543f6cc29d501c64d239044911b0b3d4c72cf957d4932505dd" "9b6cd409e283acce85fe1cb34aada88407ae06381687b95472f180cc57007de2" "388014d815c5a4881817fce4d2f8423b2779ab0305fa078d5ca02c18a96e26bb" "f8901de0da4cca955e8f93f89408947acee7b899a48836db7c11593522ba6449" "636a241b02597d37908ca345b587ed9c2c758d5afb517c00d8b74abfe3e4cd74" "3848496a1c9d665797b0d54493773b2fed91ba27f7dce3800be84d6964de69b0" "5c5b2ee417e01cff9f633e07894e2a44c49cb31a5b218e5049afefc1adb2c0bd" "0e1f896f7580cf167d1f9d3f2e36e788f04bf771789a9e8be9933c78987a0de4" "34da35596f7d12fab2cceedd460649d57f005d3858cd151569edd753f14852e1" "6ed6260b1f493c3f1161fcce1a4b7713024a5d1b3c2da7948d4bd2aea787f422" "054d3557789f21eb5671f4f91079d8d015d821ea47be0e9017cd867c6f9c53b4" "fc6e906a0e6ead5747ab2e7c5838166f7350b958d82e410257aeeb2820e8a07a" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" default)))
 '(debug-on-quit nil)
 '(fci-rule-color "#073642")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(syslog-debug-face
   (quote
    ((t :background unspecified :foreground "#2aa198" :weight bold))))
 '(syslog-error-face
   (quote
    ((t :background unspecified :foreground "#dc322f" :weight bold))))
 '(syslog-hour-face (quote ((t :background unspecified :foreground "#859900"))))
 '(syslog-info-face
   (quote
    ((t :background unspecified :foreground "#268bd2" :weight bold))))
 '(syslog-ip-face (quote ((t :background unspecified :foreground "#b58900"))))
 '(syslog-su-face (quote ((t :background unspecified :foreground "#d33682"))))
 '(syslog-warn-face
   (quote
    ((t :background unspecified :foreground "#cb4b16" :weight bold))))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#cb4b16")
     (60 . "#b58900")
     (80 . "#859900")
     (100 . "#2aa198")
     (120 . "#268bd2")
     (140 . "#d33682")
     (160 . "#6c71c4")
     (180 . "#dc322f")
     (200 . "#cb4b16")
     (220 . "#b58900")
     (240 . "#859900")
     (260 . "#2aa198")
     (280 . "#268bd2")
     (300 . "#d33682")
     (320 . "#6c71c4")
     (340 . "#dc322f")
     (360 . "#cb4b16"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83"))))

(require 'gpb-recent-files)
(setq gpb-fl--recent-file-list-symbol 'gpb-rf-recent-file-list)
