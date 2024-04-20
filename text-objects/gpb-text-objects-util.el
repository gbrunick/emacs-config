;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Utility functions for dealing with text objects.
;;
;;  ffto is short for forward function-based text object.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'gpb-text-objects-base)


(defvar gpb-tobj--forward-function-definition nil
  "A forward function is a convenient way to define a class of text objects.

A forward function accepts a single argument which should be 1 or
-1.  If the argument is 1 (resp. -1), then the forward function
should move the point forward (resp. backwards) to the first
buffer position strictly after (resp. before) the current point
which is the end (resp. start) of a text object.

If no such objects exists, the forward function should (signal
'search-failed \"reason\") and leave the point alone.

If multiple nested text objects begin or end at the new position
of the point, then the forward-function should indicate this by
concluding with (throw 'multiple-boundaries COUNT) where COUNT is
the number of text objects that share the common boundary
position.  When moving forward (resp. backwards), COUNT should be
equal to the number of text objects that end (resp. begin) at the
new position of the point.")


(defvar gpb-tobj--indented-text-block-definition nil
  "A collection of lines of text is considered an \"indented
block of text\" if all of the following conditions hold:

1. The first line of text is not ignored.

2. Each line of text after the first line of text is either
   ignored or indented strictly more than the first line.

3. The last line of text is not ignored.

4. The last line of text is followed by zero or more ignored
   lines and then the end of the buffer or a nonignored line that
   is indented at a level that is equal to or less than the
   indentation of the first line of text in the block.

Lines consisting only of whitespace and lines whose first
character is contained in a string are always ignored.  The user
may set the variable `gpb-tobj--is-ignored-line-function' to a
function to ignore additional kinds of lines.  A typical usage
for a custom function would be to ignore comment only lines in
programming modes.

Example:

1 def f(x, y):
2     print x
3     print y
4
5 def g(x, y):
6     print x
7     for i in range(10):
8         print i

Lines 1-3 are an indented block, lines 5-8 are an indented block,
and lines 7-8 are an indented block, so line 8 is the last line
of two disitinct indented blocks.  In particular, a line can only
be the first line of single indented block, but a line can be the
last line of a multiple blocks.")


(defvar gpb-tobj--is-ignored-line-function nil
  "Set this variable to a function to customize the definition of
an indented text block.  This function should return t if the
point is on a line that should be ignored when calculating
indented text blocks.  See `gpb-tobj--indented-text-block-definition'
for more information.")


(defun gpb-tobj--apply-predicate (pred boundary-list &optional count)
  (cl-assert (>= (length boundary-list) 2))
  (if count
      (let ((true-count 0))
        ;; Count the number of text object in boundary-list
        ;; that strictly contain the region delimited by beg
        ;; and end.
        (while boundary-list
          (when (funcall pred (pop boundary-list) (pop boundary-list))
            (cl-incf true-count)))
        (>= true-count count))
    (condition-case exc
        (funcall pred (car boundary-list) (cadr boundary-list))
      (wrong-number-of-arguments (funcall pred boundary-list)))))


(defun gpb-tobj--count-indented-text-blocks-ending-at (&optional pos)
  "Count the number of indented blocks that end at the line containing POS.

If POS is not provided, then the function uses the current point."
  (save-excursion
    (when pos (goto-char pos))
    ;; An ignored line is never the end of a block
    (if (gpb-tobj--is-ignored-line-p)
        0
      ;; Otherwise, we start counting
      (let* ((count 0)
             (indent (current-indentation))
             ;; Find the level of indentation of the next non-ignored line
             (next-indent (save-excursion
                            (forward-line 1)
                            (while (and (not (eobp))
                                        (gpb-tobj--is-ignored-line-p))
                              (forward-line 1))
                            (if (eobp) 0 (current-indentation)))))
        ;; We now begin stepping backwards until we reach a line that
        ;; is not indented strictly more than the first nonignored
        ;; line following POS.
        (while (and (not (bobp)) (> (current-indentation) next-indent))
          (forward-line -1)
          (while (and (not (bobp)) (gpb-tobj--is-ignored-line-p))
            (forward-line -1))
          (when (and (<= next-indent (current-indentation))
                     (< (current-indentation) indent))
            (cl-incf count)
            (setq indent (current-indentation))))
        count))))


(defun gpb-tobj--expand-nested-text-object (beg end common-boundaries
                                            forward-func)
  "Expand the text object bounded by BEG END.

BEG END must delimit a nested text object.  COMMON-BOUNDARIES is
an integer and is equal to zero unless there are strictly larger
text objects that share a common boundary with the object
delimited by BEG and END.  If COMMON-BOUNDARIES is positive, then
there are COMMON-BOUNDARIES text objects that start at BEG and
end strictly after END.  If COMMON-BOUNDARIES is negative, then
there are (abs COMMON-BOUNDARIES) text objects that begin
strictly before BEG and end at END."
  (cond
   ((> common-boundaries 0)
    (save-excursion
      (goto-char end)
      (decf common-boundaries (gpb-tobj--move-to-boundary-of-nested-text-object 1 forward-func t))
      (list beg (point) common-boundaries)))
   ((< common-boundaries 0)
    (save-excursion
      (goto-char beg)
      (cl-incf common-boundaries (gpb-tobj--move-to-boundary-of-nested-text-object -1 forward-func t))
      (list (point) end common-boundaries)))
   ((= common-boundaries 0)
    (save-excursion
      (goto-char beg)
      (cl-incf common-boundaries (gpb-tobj--move-to-boundary-of-nested-text-object -1 forward-func t))
      (setq beg (point))
      (goto-char end)
      (decf common-boundaries (gpb-tobj--move-to-boundary-of-nested-text-object 1 forward-func t))
      (list beg (point) common-boundaries)))
   (t
    (error "Runtime error"))))


(defun gpb-tobj--first-line-of-indented-text-block-p (&optional pos)
  "Returns t if the line containing POS is the first line of an indented block.

See `gpb-tobj--indented-text-block-definition' for the definition
of an indented block of text."
  (save-excursion
    (when pos (goto-char pos))
    (forward-line 0)
    (and (not (eobp))
         (not (gpb-tobj--is-ignored-line-p))
         (< (current-indentation)
            (progn
              (forward-line 1)
              (while (and (not (eobp)) (gpb-tobj--is-ignored-line-p))
                (forward-line 1))
              (current-indentation))))))


;; (defun gpb-tobj--find-nested-text-objects (pos dir forward-func &optional pred)
;;   "Returns a list of nested text objects."
;;   (cl-assert (member dir '(1 -1)))
;;   (save-excursion
;;     (let (boundary1 boundary2 balance result)
;;       (goto-char pos)
;;       (setq balance (- (catch 'multiple-boundaries (funcall forward-func dir) 1))
;;             boundary1 (point))
;;       (cl-incf balance (gpb-tobj--move-to-boundary-of-nested-text-object
;;                      (- dir) forward-func))
;;       (setq boundary2 (point))
;;       (let ((beg (min boundary1 boundary2))
;;             (end (max boundary1 boundary2)))
;;         (ignore-errors
;;           (while (or (null pred) (funcall pred beg end))
;;             (setq obj-list (append obj-list `((,beg ,end))))
;;             (cl-multiple-value-bind (beg end balance)
;;                 (gpb-tobj--expand-nested-text-object
;;                  beg end balance forward-func))))
;;         obj-list))))


(defun gpb-tobj--find-maximal-nested-text-object (pos dir forward-func pred)
  "Expand the text object until PRED fails.

The initial, innermost, text object is specified by giving an
initial buffer POS and DIR which are passes to `forward-func'.

The result is a list of the form (b_1 e_1 b_2 e_2 ... b_n e_n)
where each (b_i, e_i) pair denotes the boundaries of a text
object that satisfies PRED.  The bounds (b_1, e_1) correspond to
the outermost text object and the remaining boundaries are
nested.  That is: b_i <= b_{i+1}, e_{i+1} <= e_i, and at least on
of these inequalities is strict.


If the innermost text object does not satisfy the PRED, the
function return nil.

The function PRED is first passed the beg end of the outer most
text object.  If this fails due to the error condition
`wrong-number-of-arguments', then PRED is passed the entire list
of boundaries described above as a single argument.  See
`gpb-tobj--apply-predicate' for the details."
  (cl-assert (member dir '(1 -1)))
  (save-excursion
    (let (boundary1 boundary2 balance)
      (goto-char pos)
      (setq balance (- (catch 'multiple-boundaries (funcall forward-func dir) 1))
            boundary1 (point))
      (cl-incf balance (gpb-tobj--move-to-boundary-of-nested-text-object
                     (- dir) forward-func))
      (setq boundary2 (point))
      (let ((beg (min boundary1 boundary2))
            (end (max boundary1 boundary2)))
        (when (funcall pred beg end)
          (gpb-tobj--find-maximal-nested-text-object-1
           `((,beg ,end)) (* dir balance) forward-func pred))))))

          ;; (signal 'search-failed "First object fails predicate"))))))


(defun gpb-tobj--find-maximal-nested-text-object-1 (boundary-list balance
                                                    forward-func pred)
  (condition-case exc
      (cl-multiple-value-bind (next-beg next-end next-balance)
          (gpb-tobj--expand-nested-text-object (first (car boundary-list))
                                               (second (car boundary-list))
                                               balance forward-func)
        (let ((next-list (cons (list next-beg next-end) boundary-list)))
          (if (funcall pred next-beg next-end)
              ;; If the predicte is true, we keep trying to expand the text
              ;; object.
              (gpb-tobj--find-maximal-nested-text-object-1
               next-list next-balance forward-func pred)
            ;; Otherwise we have found the maximal list of objects.
            boundary-list)))
    ;; If `gpb-tobj--expand-nested-text-object' fails, then current list is
    ;; the maximal list
    (search-failed boundary-list)))


(defun gpb-tobj--find-minimal-nested-text-object (pos dir forward-func pred
                                                  &optional count)
  "Expand the text object until PRED is true.

The initial, innermost, text object is specified by giving an
initial buffer POS and DIR which are passes to `forward-func'.

The result is a list of the form ((b_1 e_1) (b_2 e_2) ... (b_n e_n))
where each (b_i, e_i) pair denotes the boundaries of a text
object.  The bounds (b_1, e_1) correspond to the outermost text
object (the only text object that satisfies PRED) and the
remaining boundaries are nested.  That is: b_i <= b_{i+1},
e_{i+1} <= e_i, and at least on of these inequalities is strict.

If there are not text objects which satisfy the predicate, this
function signals `search-failed'.

The function PRED is first passed the beg end of the outermost
text object.  If this fails due to the error condition
`wrong-number-of-arguments', then PRED is passed the entire list
of boundaries described above as a single argument.  See
`gpb-tobj--apply-predicate' for the details."
  (cl-assert (member dir '(1 -1)))
  (save-excursion
    (let (boundary1 boundary2 balance)
      (goto-char pos)
      (setq balance (- (catch 'multiple-boundaries
                         (funcall forward-func dir) 1))
            boundary1 (point))
      (cl-incf balance (gpb-tobj--move-to-boundary-of-nested-text-object
                     (- dir) forward-func))
      (setq boundary2 (point))
      (let ((beg (min boundary1 boundary2))
            (end (max boundary1 boundary2)))
        (if (funcall pred beg end)
            ;; We are done
            `((,beg ,end))
          ;; Keep looking
          (gpb-tobj--find-minimal-nested-text-object-1 `((,beg ,end))
                                                       (* dir balance)
                                                       forward-func pred
                                                       count))))))

(defun gpb-tobj--find-minimal-nested-text-object-1 (boundary-list bal ffunc pred
                                                    &optional count)
  (cl-multiple-value-bind (next-beg next-end next-bal)
      (gpb-tobj--expand-nested-text-object
       (first (car boundary-list)) (second (car boundary-list)) bal ffunc)
    (let ((next-list (append `((,next-beg ,next-end)) boundary-list)))
      (if (funcall pred next-beg next-end)
          ;; If the predicate is true, we have found our list.
          next-list
        ;; Otherwise we keep looking.
        (gpb-tobj--find-minimal-nested-text-object-1 next-list next-bal
                                                     ffunc pred count)))))


(defun gpb-tobj--flash-region (beg end)
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'face 'region)
    (overlay-put ov 'window (selected-window))
    (sit-for 0.1)
    (delete-overlay ov)))


(defun gpb-tobj--forward-indented-text-block (arg)
  "Move to the beginning or end of a block of text."
  (interactive "^p")
  (cond
   ;; Move backwards to the first line which is the beginning of an
   ;; indented text block
   ((= arg -1)
    ;; Always back up at least one character
    (when (bobp) (signal 'search-failed "Beginning of buffer"))
    (if (bolp) (forward-line -1) (forward-line 0))
    ;; Now backup until you find the first line of an indented text block.
    (while (and (not (bobp))
                (not (gpb-tobj--first-line-of-indented-text-block-p)))
      (forward-line -1))
    (unless (gpb-tobj--first-line-of-indented-text-block-p)
      (signal 'search-failed "Beginning of buffer")))
   ;; Move forward to the beginning of line which follows a line that
   ;; closes an indented text block.
   ((= arg 1)
    (when (eobp) (signal 'search-failed "End buffer"))
    (let ((count (gpb-tobj--count-indented-text-blocks-ending-at)))
      (while (and (not (eobp)) (<= count 0))
        (forward-line 1)
        (setq count (gpb-tobj--count-indented-text-blocks-ending-at)))
      (unless (> count 0) (signal 'search-failed "End of buffer"))
      ;; We leave the point after the line that closes the indented text
      ;; block.
      (forward-line 1)
      (when (> count 1) (throw 'multiple-boundaries count))))
   ;; Only handle -1 and 1
   (t
    (error "Runtime Error"))))


(defun gpb-tobj--in-comment-p ()
  (nth 4 (syntax-ppss)))


(defun gpb-tobj--move-to-boundary-of-nested-text-object (dir forward-func
                                                         &optional interior-p
                                                         bound)
  "Move to the boundary of the smallest nested text object containing the point.

If DIR is 1, we move to the end.  If DIR is -1 we move to the
start.  The point always moves.  If INTERIOR-P is true, then we
restrict attention to text objects that strictly contain the
point.  Signals 'search-failed when the search for a boundary
fails.  Returns the number of unmatched boundaries at the point.

To clarify the previous description, suppose that the buffer contains the
text \" abc def \", \"abc\" is a text object, \"b\" is a text
object, and \"def\" is text object.  Then we have the following
cases:

1. If the point sits before \"a\", then the sexp
   (gpb-tobj--move-to-boundary-of-nested-text-object 1 ...)
   leaves the point after \"c\".

2. If the point sits before \"a\", then the sexp
   (gpb-tobj--move-to-boundary-of-nested-text-object 1 ... t)
   signals 'search-failed.

3. If the point sits between \"a\" and \"b\", then the sexp
   (gpb-tobj--move-to-boundary-of-nested-text-object 1 ...)
   places the point between \"b\" and \"c\".

4. If the point sits between \"a\" and \"b\", then the sexp
   (gpb-tobj--move-to-boundary-of-nested-text-object 1 ... t)
   places the point after \"c\".

5. If the point sits after \"c\", then the sexp
   (gpb-tobj--move-to-boundary-of-nested-text-object 1 ...)
   and
   (gpb-tobj--move-to-boundary-of-nested-text-object 1 ... t)
   both signal 'search-failed."
  ;; Remark: all the variables and comments below are given for the
  ;; case where DIR is 1, but the code also handles the case where DIR
  ;; is -1.
  (cl-assert (member dir '(-1 1)))
  (setq bound (or bound (if (eq dir 1) (point-max) (point-min))))
  (let ((unmatched-ends 0) (initial-point (point)) pos)
    (while (and (<= unmatched-ends 0) (>= (* dir (- bound (point))) 0))
      (setq pos (point))
      ;; If FORWARD-FUNC throws an error, then this function should fail.
      (condition-case e
          (cl-incf unmatched-ends (catch 'multiple-boundaries
                                 (funcall forward-func dir)
                                 1))
        ('error (signal 'search-failed
                        "Cannot move forward to next object boundary")))
      (condition-case e
          (save-excursion
            ;; Now update `unmatched-ends' to reflect all of the
            ;; begins that we skipped over in the previous
            ;; FORWARD-FUNC move.
            (let ((begins (catch 'multiple-boundaries
                            (funcall forward-func (- dir))
                            1)))
              (while (and (>= (* dir (- (point) pos)) 0)
                          (or interior-p (/= (point) initial-point)))
                (decf unmatched-ends begins)
                (setq begins (catch 'multiple-boundaries
                               (funcall forward-func (- dir))
                               1)))))
        ('search-failed)))
    (when (> (* dir (- (point) bound)) 0)
      (signal 'search-failed "Bound exceeded"))
    unmatched-ends))


(defun gpb-tobj--forward-regex (arg begin-regex end-regex
                                    &optional predicate ignore-comment)
  "Does not support regular expressions that span newlines.

If PREDICATE is provided, this a function that is called to
determine if a match should count.  For example, this function
could test to see if the point is contained in a comment."
  (let ((pt (point))
        (begin-match 0)
        (end-match 0)
        (initial-point-in-comment (gpb-tobj--in-comment-p)))
    (when (listp begin-regex)
      (setq begin-match (cdr begin-regex)
            begin-regex (car begin-regex)))
    (when (listp end-regex)
      (setq end-match (cdr end-regex)
            end-regex (car end-regex)))
    (cond
     ((= arg 1)
      (forward-line 0)
      (re-search-forward end-regex)
      (while (or (<= (match-end end-match) pt)
                 (and predicate (funcall predicate)))
        (re-search-forward end-regex))
      (goto-char (match-end end-match))
      (and (gpb-tobj--in-comment-p)
           (or (not initial-point-in-comment) ignore-comment)
           (gpb-tobj--forward-regex arg begin-regex end-regex predicate t))
      1)

     ((= arg -1)
      (end-of-line)
      (re-search-backward begin-regex)
      (while (or (>= (match-beginning begin-match) pt)
                 (and predicate (funcall predicate)))
        (re-search-backward begin-regex))
      (goto-char (match-beginning begin-match))
      (and (gpb-tobj--in-comment-p)
           (or (not initial-point-in-comment) ignore-comment)
           (gpb-tobj--forward-regex arg begin-regex end-regex predicate t))
      1)

     (t (error "Runtime error")))))


(defun gpb-tobj--in-string-or-comment-p ()
  "Is the point in a string or a comment"
  (let ((parse-info (syntax-ppss)))
    (or (elt parse-info 3) (elt parse-info 4) (elt parse-info 7))))


(defun gpb-tobj--is-ignored-line-p ()
  "Should the line be ignored in an indented block?

See `gpb-tobj--indented-text-block-definition' for the definition
of an indented block of text."
  (save-excursion
    (forward-line 0)
    (or (looking-at "^[ \t]*$")
        (looking-at "^[[:space:]]*$")
        (in-string-p)
        (when gpb-tobj--is-ignored-line-function
          (funcall gpb-tobj--is-ignored-line-function)))))


(defun gpb-tobj--region-string (beg end)
  (truncate-string-to-width
   (replace-regexp-in-string "\n" " " (buffer-substring-no-properties beg end))
   60 nil nil "..."))


(defmacro gpb-tobj--remove-keyword-arg (kwargs-symbol key &optional required)
  (catch 'result
    (let ((kwargs (symbol-value kwargs-symbol)) save-args)
      (while (keywordp (car-safe kwargs))
        (let ((k (pop kwargs))
              (v (pop kwargs)))
          (when (eq k key)
            (throw 'result
                   `(progn
                      (setq ,kwargs-symbol ',(nconc save-args kwargs))
                      ',v)))
          (setq save-args (nconc  save-args (list k v)))))
      (when required
        (error "Missing required option: %S" key)))))


(provide 'gpb-text-objects-util)
