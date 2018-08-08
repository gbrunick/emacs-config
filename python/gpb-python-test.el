(ert-deftest gpb-py:escape-string-for-python-test ()
  (let ((file-name (make-temp-file "test-" nil ".py")))
    (dolist (str '("This"
                   "'abc'\n\"def\"\n\"\"\"ghi\"\"\"\t\t\t"))
      (with-temp-buffer
        (insert str)
        (write-region (point-min) (point-max) file-name))
      (should (string-equal
               (gpb-py:escape-string-for-python str)
               (with-temp-buffer
                 ;; Insert initial triple quote and escaped newline
                 (insert "'''\\\n")
                 ;; Get repr(str) without quotes
                 (shell-command
                  (format (concat "python -c \"print "
                                  "repr(open('''%s''','rw').read())[1:-1]\"")
                          file-name)
                  t)
                 ;; Removing trailing newline from print statement
                 (goto-char (point-max))
                 (delete-backward-char 1)
                 ;; Add trailing triple quote
                 (insert "'''")
                 (goto-char (point-min))
                 (while (search-forward "\\n" nil t)
                   (replace-match "\n" nil t))
                 (while (search-forward "\\t" nil t)
                   (replace-match "\t" nil t))
                 (buffer-substring-no-properties (point-min) (point-max))))))))

(ert-run-tests-interactively "gpb-py:*")
