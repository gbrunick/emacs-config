;; Some useful S-exp's for testing

(shpool-get-server-buf "/plinkx:WSL:~")

(shpool-shell-command "ls -la" nil nil "/plinkx:WSL:~/")

(shpool-shell-command "sleep 2" nil nil "/plinkx:WSL:~/")

(shpool-shell-command "dir" nil nil "c:/Users/gbrun/Code/emacs-config/")

(shpool-shell-command "echo $X and $Y" nil (list "X=this" "Y=that")
                      "/plinkx:WSL:~/")

(shpool-shell-command "echo %X% and %Y%" nil (list "X=this" "Y=that"))

(prat-get-editor-script)

(let ((default-directory  "/plinkx:WSL:~/"))
  (prat-get-editor-script))
