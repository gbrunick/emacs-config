;; Some useful S-exp's for testing

(prat-get-server-buf "/plinkx:WSL:~")

(prat-shell-command "ls -la" nil nil "/plinkx:WSL:~/")

(prat-shell-command "sleep 2" nil nil "/plinkx:WSL:~/")

(prat-shell-command "dir" nil nil "c:/Users/gbrun/Code/emacs-config/")

(prat-shell-command "echo $X and $Y" nil (list "X=this" "Y=that")
                      "/plinkx:WSL:~/")

(prat-shell-command "echo %X% and %Y%" nil (list "X=this" "Y=that"))

(prat-get-editor-script)

(let ((default-directory  "/plinkx:WSL:~/"))
  (prat-get-editor-script))


(autoload 'prat-user-command-prefix-keymap "prat" nil nil 'keymap)
(global-set-key "\C-c\C-v" 'prat-user-command-prefix-keymap)
(prat-reload-all)

