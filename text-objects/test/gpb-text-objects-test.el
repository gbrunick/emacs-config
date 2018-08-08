(require 'ert)
(require 'gpb-text-objects)

(defun gpb-tobj--test-move (dir)
  "This is a test movement.
A \"b\" followed by a number and an \"e\" followed by a number
begin and end text objects.  The number gives the number of text
objects that share that boundary."
  (case dir
    (1 (re-search-forward "e[0-9]+")
       (when (looking-back "e\\([0-9]+\\)")
         (signal 'multiple-boundaries (string-to-int (match-string 1)))))
    (-1 (re-search-backward "b")
        (when (looking-at "b\\([0-9]+\\)")
          (signal 'multiple-boundaries (string-to-int (match-string 1)))))
    (t (error "Runtime error"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Test gpb-tobj--move-to-boundary-of-nested-text-object
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest gpb-tobj--move-to-boundary-of-nested-text-object--test-1 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects--test-words.txt")
    (goto-char 142)
    (gpb-tobj--move-to-boundary-of-nested-text-object 1 'gpb-tobj--forward-word)
    (should (equal (point) 145))))

(ert-deftest gpb-tobj--move-to-boundary-of-nested-text-object--test-2 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects--test-words.txt")
    (goto-char 142)
    (gpb-tobj--move-to-boundary-of-nested-text-object -1 'gpb-tobj--forward-word)
    (should (equal (point) 139))))

(ert-deftest gpb-tobj--move-to-boundary-of-nested-text-object--test-3 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects--test-words.txt")
    (goto-char 142)
    (gpb-tobj--move-to-boundary-of-nested-text-object 1 'gpb-tobj--forward-word t)
    (should (equal (point) 145))))

(ert-deftest gpb-tobj--move-to-boundary-of-nested-text-object--test-4 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects--test-words.txt")
    (goto-char 142)
    (gpb-tobj--move-to-boundary-of-nested-text-object -1 'gpb-tobj--forward-word t)
    (should (equal (point) 139))))

(ert-deftest gpb-tobj--move-to-boundary-of-nested-text-object--test-5 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects--test-words.txt")
    (goto-char 54)
    (should-error (gpb-tobj--move-to-boundary-of-nested-text-object 1 'gpb-tobj--forward-word)
                  :type 'search-failed)))

(ert-deftest gpb-tobj--move-to-boundary-of-nested-text-object--test-6 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects--test-words.txt")
    (goto-char 55)
    (should-error (gpb-tobj--move-to-boundary-of-nested-text-object 1 'gpb-tobj--forward-word t)
                  :type 'search-failed)))

(ert-deftest gpb-tobj--move-to-boundary-of-nested-text-object--test-7 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects--test-words.txt")
    (goto-char 60)
    (should-error (gpb-tobj--move-to-boundary-of-nested-text-object -1 'gpb-tobj--forward-word)
                  :type 'search-failed)))

(ert-deftest gpb-tobj--move-to-boundary-of-nested-text-object--test-8 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects--test-words.txt")
    (goto-char 59)
    (should-error (gpb-tobj--move-to-boundary-of-nested-text-object -1 'gpb-tobj--forward-word t)
                  :type 'search-failed)))

(ert-deftest gpb-tobj--move-to-boundary-of-nested-text-object--test-101 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects-test.txt")
    (goto-char 1136)
    (gpb-tobj--move-to-boundary-of-nested-text-object -1 'gpb-tobj--forward-indented-text-block)
    (should (equal (point) 1020))))

(ert-deftest gpb-tobj--move-to-boundary-of-nested-text-object--test-102 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects-test.txt")
    (goto-char 1320)
    (should-error (gpb-tobj--move-to-boundary-of-nested-text-object
                   -1 'gpb-tobj--forward-indented-text-block)
                  :type 'search-failed)))

(ert-deftest gpb-tobj--move-to-boundary-of-nested-text-object--test-103 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects-test.txt")
    (goto-char 1895)
    (gpb-tobj--move-to-boundary-of-nested-text-object 1 'gpb-tobj--forward-indented-text-block t)
    (should (equal (point) 2743))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Test gpb-tobj--forward-indented-text-block
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest gpb-tobj--forward-indented-text-block--test-1 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects-test.txt")
    (should (equal  '(977 1 1019)
                  (list (goto-char 977)
                        (catch 'multiple-boundaries
                          (gpb-tobj--forward-indented-text-block 1) 1)
                        (point))))))

(ert-deftest gpb-tobj--forward-indented-text-block--test-2 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects-test.txt")
    (goto-char 1224)
    (should (equal  '(1 1301 2 1319)
                    (list (catch 'multiple-boundaries
                            (gpb-tobj--forward-indented-text-block 1) 1)
                          (point)
                          (catch 'multiple-boundaries
                            (gpb-tobj--forward-indented-text-block 1) 1)
                          (point))))))

(ert-deftest gpb-tobj--forward-indented-text-block--test-3 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects-test.txt")
    (should (equal  '(1355 1 1457 3 1498)
                  (list (goto-char 1355)
                        (catch 'multiple-boundaries
                          (gpb-tobj--forward-indented-text-block 1) 1)
                        (point)
                        (catch 'multiple-boundaries
                          (gpb-tobj--forward-indented-text-block 1) 1)
                        (point))))))

(ert-deftest gpb-tobj--forward-indented-text-block--test-4 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects-test.txt")
    (goto-char 1447)
    (should (equal  '(1 1409)
                    (list (catch 'multiple-boundaries
                            (gpb-tobj--forward-indented-text-block -1) 1)
                          (point))))))

(ert-deftest gpb-tobj--forward-indented-text-block--test-5 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects-test.txt")
    (goto-char 1409)
    (should (equal  '(1 1342)
                  (list (catch 'multiple-boundaries
                          (gpb-tobj--forward-indented-text-block -1) 1)
                        (point))))))

(ert-deftest gpb-tobj--forward-indented-text-block--test-6 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects-test.txt")
    (goto-char 1342)
    (should (equal  '(1 1320)
                  (list (catch 'multiple-boundaries
                          (gpb-tobj--forward-indented-text-block -1) 1)
                        (point))))))

(ert-deftest gpb-tobj--forward-indented-text-block--test-7 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects-test.txt")
    (goto-char 1320)
    (should (equal  '(1 1210)
                  (list (catch 'multiple-boundaries
                          (gpb-tobj--forward-indented-text-block -1) 1)
                        (point))))))

(ert-deftest gpb-tobj--forward-indented-text-block--test-8 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects-test.txt")
    (goto-char 1019)
    (catch 'multiple-boundaries (gpb-tobj--forward-indented-text-block 1))
    (should (equal (point) 1136))))

(ert-deftest gpb-tobj--forward-indented-text-block--test-9 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects-test.txt")
    (goto-char 34)
    (should-error (catch 'multiple-boundaries (gpb-tobj--forward-indented-text-block -1) 1))))

(ert-deftest gpb-tobj--forward-indented-text-block--test-10 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects-test.txt")
    (goto-char 1506)
    (catch 'multiple-boundaries (gpb-tobj--forward-indented-text-block 1) 1)
    (should (equal (point) 1639))))

(ert-deftest gpb-tobj--forward-indented-text-block--test-11 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects-test.txt")
    (goto-char 1896)
    (catch 'multiple-boundaries (gpb-tobj--forward-indented-text-block 1) 1)
    (should (equal (point) 2034))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Test gpb-tobj--expand-nested-text-object
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest gpb-tobj--expand-nested-text-object--test-1 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects-test.txt")
    (should (equal
             (gpb-tobj--expand-nested-text-object
              443 462 0
              (lambda (arg) (gpb-tobj--forward-regex arg "(" ")")))
             '(399 488 0)))))

(ert-deftest gpb-tobj--expand-nested-text-object--test-2 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects-test.txt")
    (should (equal
             (gpb-tobj--expand-nested-text-object
              399 488 0
              (lambda (arg) (gpb-tobj--forward-regex arg "(" ")")))
             '(300 763 0)))))

(ert-deftest gpb-tobj--expand-nested-text-object--test-3 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects-test.txt")
    (should (equal
             (gpb-tobj--expand-nested-text-object
              300 763 0
              (lambda (arg) (gpb-tobj--forward-regex arg "(" ")")))
             '(201 764 0)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  gpb-tobj--find-minimal-nested-text-object
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun gpb-tobj--find-minimal-nested-text-object-tester (beg end forward-func
                                                &optional strictly-larger bound)
  "Find the smallest nested text object that contains the region BEG to END.

Returns a list of the form (TOBJ-BEG TOBJ-END) with
TOBJ-BEG <= BEG and END <= TOBJ-END.  If STRICTLY-LARGER is
non-nil, the bounds of the text object are always strictly larger
than the region BEG END.  In other words, TOBJ-BEG < BEG or END <
TOBJ-END.  Returns on nil if there is no outer text object."
  (let ((pred `(lambda (beg end)
                 (or (and (not strictly-larger)
                          (<= beg ,beg)
                          (<= ,end end))
                     (and (< beg ,beg)
                          (<= ,end end))
                     (and (<= beg ,beg)
                          (< ,end end))))))
    (condition-case exc
        (let ((x (gpb-tobj--find-minimal-nested-text-object
                  beg 1 forward-func pred)))
          (car x))
      (search-failed nil))))



(ert-deftest gpb-tobj--find-minimal-nested-text-object--test-01 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects-test.txt")
    (should (equal
             (gpb-tobj--find-minimal-nested-text-object-tester
              450 451
              (lambda (arg) (gpb-tobj--forward-regex arg "(" ")"))
              nil)
             '(443 462)))))

(ert-deftest gpb-tobj--find-minimal-nested-text-object--test-02 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects-test.txt")
    (should (equal
             (gpb-tobj--find-minimal-nested-text-object-tester
              568 669
              (lambda (arg) (gpb-tobj--forward-regex arg "(" ")"))
              nil)
             '(568 669)))))

(ert-deftest gpb-tobj--find-minimal-nested-text-object--test-03 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects-test.txt")
    (should (equal
             (gpb-tobj--find-minimal-nested-text-object-tester
              568 669
              (lambda (arg) (gpb-tobj--forward-regex arg "(" ")"))
              t)
             '(518 670)))))

(ert-deftest gpb-tobj--find-minimal-nested-text-object--test-04 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects-test.txt")
    (should (equal
             (gpb-tobj--find-minimal-nested-text-object-tester
              450 451
              (lambda (arg) (gpb-tobj--forward-regex arg "(" ")"))
              t)
             '(443 462)))))

(ert-deftest gpb-tobj--find-minimal-nested-text-object--test-05 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects-test.txt")
    (should (equal
             (gpb-tobj--find-minimal-nested-text-object-tester
              493 762
              (lambda (arg) (gpb-tobj--forward-regex arg "(" ")"))
              nil)
             '(493 762)))))

(ert-deftest gpb-tobj--find-minimal-nested-text-object--test-06 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects-test.txt")
    (should (equal
             (gpb-tobj--find-minimal-nested-text-object-tester
              493 762
              (lambda (arg) (gpb-tobj--forward-regex arg "(" ")"))
              t)
             '(300 763)))))

(ert-deftest gpb-tobj--find-minimal-nested-text-object--test-07 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects-test.txt")
    (should (equal
             (gpb-tobj--find-minimal-nested-text-object-tester
              490 527
              (lambda (arg) (gpb-tobj--forward-regex arg "(" ")"))
              t)
             '(300 763)))))

(ert-deftest gpb-tobj--find-minimal-nested-text-object--test-08 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects-test.txt")
    (should (equal '(1020 1136)
                   (gpb-tobj--find-minimal-nested-text-object-tester
                    1057 1058 'gpb-tobj--forward-indented-text-block nil)))))

(ert-deftest gpb-tobj--find-minimal-nested-text-object--test-09 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects-test.txt")
    (should (equal '(1020 1136)
                   (gpb-tobj--find-minimal-nested-text-object-tester
                    1020 1021 'gpb-tobj--forward-indented-text-block nil)))))

(ert-deftest gpb-tobj--find-minimal-nested-text-object--test-10 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects-test.txt")
    (should (equal (gpb-tobj--find-minimal-nested-text-object-tester
                    1020 1136 'gpb-tobj--forward-indented-text-block t)
                   '(886 1319)))))

(ert-deftest gpb-tobj--find-minimal-nested-text-object--test-11 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects-test.txt")
    (should (equal (gpb-tobj--find-minimal-nested-text-object-tester
                    1320 1498 'gpb-tobj--forward-indented-text-block t)
                   nil))))

(ert-deftest gpb-tobj--find-minimal-nested-text-object--test-12 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects-test.txt")
    (should (equal (gpb-tobj--find-minimal-nested-text-object-tester
                    1895 1896 'gpb-tobj--forward-indented-text-block)
                   (list 1763 2743)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  gpb-tobj--find-maximal-nested-text-object
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(ert-deftest gpb-tobj--find-maximal-nested-text-object--test-001 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects-test.txt")
    (should (equal
             (gpb-tobj--find-maximal-nested-text-object
              450 1
              (lambda (arg) (gpb-tobj--forward-regex arg "(" ")"))
              (lambda (beg end) (and (<= beg 450) (<= 450 end))))
             '((201 764) (300 763) (399 488) (443 462))))))



(defun gpb-tobj--find-maximal-nested-text-object-tester
 (pos dir forward-func)
  "Find the next text object that begins/ends at or after/before POS.

If DIR is 1 we search forward, if DIR is -1, then we search
backwards.  Returns a list of the form (beg end) or signals
'search-failed."
  (save-excursion
    (goto-char pos)
    (let ((pred (if (eq dir 1) `(lambda (beg end) (>= beg ,pos))
                  `(lambda (beg end) (<= end ,pos))))
          boundary-list)
      (catch 'result
        (while t
          (let ((boundary-list (gpb-tobj--find-maximal-nested-text-object
                                (point) dir forward-func pred)))
            (if boundary-list (throw 'result (car boundary-list))
              (funcall forward-func dir))))))))


(ert-deftest gpb-tobj--find-maximal-nested-text-object--test-101 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects-test.txt")
    (should (equal (gpb-tobj--find-maximal-nested-text-object-tester
                    224 -1 'forward-word)
                   '(217 223)))))

(ert-deftest gpb-tobj--find-maximal-nested-text-object--test-102 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects-test.txt")
    (should (equal (gpb-tobj--find-maximal-nested-text-object-tester
                    2174 1 'gpb-tobj--forward-indented-text-block)
                   '(2253 2442)))))

(ert-deftest gpb-tobj--find-maximal-nested-text-object--test-103 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects-test.txt")
    (should (equal (gpb-tobj--find-maximal-nested-text-object-tester
                    2174 -1 'gpb-tobj--forward-indented-text-block)
                   '(2084 2128)))))

(ert-deftest gpb-tobj--find-maximal-nested-text-object--test-104 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects-test.txt")
    (should (equal (gpb-tobj--find-maximal-nested-text-object-tester
                    2128 -1 'gpb-tobj--forward-indented-text-block)
                   '(2084 2128)))))

(ert-deftest gpb-tobj--find-maximal-nested-text-object--test-105 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects-test.txt")
    (should (equal (gpb-tobj--find-maximal-nested-text-object-tester
                    2107 -1 'gpb-tobj--forward-indented-text-block)
                   '(2034 2084)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  gpb-tobj--first-line-of-indented-text-block-p
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest gpb-tobj--first-line-of-indented-text-block-p--test-1 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects-test.txt")
    (should (equal (gpb-tobj--first-line-of-indented-text-block-p 1320) t))))

(ert-deftest gpb-tobj--first-line-of-indented-text-block-p--test-2 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects-test.txt")
    (should (equal (gpb-tobj--first-line-of-indented-text-block-p 1342) t))))

(ert-deftest gpb-tobj--first-line-of-indented-text-block-p--test-3 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects-test.txt")
    (should (equal (gpb-tobj--first-line-of-indented-text-block-p 1382) nil))))

(ert-deftest gpb-tobj--first-line-of-indented-text-block-p--test-4 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects-test.txt")
    (should (equal (gpb-tobj--first-line-of-indented-text-block-p 1499) t))))

(ert-deftest gpb-tobj--first-line-of-indented-text-block-p--test-5 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects-test.txt")
    (should (equal (gpb-tobj--first-line-of-indented-text-block-p 1530) t))))

(ert-deftest gpb-tobj--first-line-of-indented-text-block-p--test-6 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects-test.txt")
    (should (equal (gpb-tobj--first-line-of-indented-text-block-p 1571) nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  gpb-tobj--count-indented-text-blocks-ending-at
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest gpb-tobj--count-indented-text-blocks-ending-at--test-1 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects-test.txt")
    (should (equal (gpb-tobj--count-indented-text-blocks-ending-at 978) 0))))

(ert-deftest gpb-tobj--count-indented-text-blocks-ending-at--test-2 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects-test.txt")
    (should (equal (gpb-tobj--count-indented-text-blocks-ending-at 1003) 1))))

(ert-deftest gpb-tobj--count-indented-text-blocks-ending-at--test-3 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects-test.txt")
    (should (equal (gpb-tobj--count-indented-text-blocks-ending-at 1101) 0))))

(ert-deftest gpb-tobj--count-indented-text-blocks-ending-at--test-4 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects-test.txt")
    (should (equal (gpb-tobj--count-indented-text-blocks-ending-at 1313) 2))))

(ert-deftest gpb-tobj--count-indented-text-blocks-ending-at--test-5 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects-test.txt")
    (should (equal (gpb-tobj--count-indented-text-blocks-ending-at 1485) 3))))

(ert-deftest gpb-tobj--count-indented-text-blocks-ending-at--test-6 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects-test.txt")
    (should (equal (gpb-tobj--count-indented-text-blocks-ending-at 1669) 3))))

(ert-deftest gpb-tobj--count-indented-text-blocks-ending-at--test-7 ()
  (with-current-buffer (find-file-noselect "gpb-text-objects-test.txt")
    (should (equal (gpb-tobj--count-indented-text-blocks-ending-at 2322) 0))))


(ert-run-tests-interactively "gpb-tobj--*")
