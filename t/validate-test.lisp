(in-package #:webhax-test)

(in-suite webhax-test)

(defparameter result nil)

(defun val1 ()
  (normalize-fieldspec-body '((:pickone :options (3 5 6 18) :notnull t))))

(defun val2 ()
  (normalize-fieldspec-body '((:yesno :notnull t))))

(defun val3 ()
  (normalize-fieldspec-body '((:pickone :options ("x" "y" "z")))))

(defun val4 ()
  (normalize-fieldspec-body '(:date)))

(defun callval (valfunc input)
  (funcall
   (getf (funcall valfunc) :compiled-validator)
   input))

(test validate
  (is-false (nth-value 1 (callval #'val1 "4")))
  (is-true (nth-value 1 (callval #'val1 "5")))
  (is (= 5 (callval #'val1 "5")))
  (signals type-error (callval #'val1 5))
  (is-false (nth-value 1 (callval #'val1 "")))

  (is-true (nth-value 1 (callval #'val2 "true")))
  (is-true (nth-value 1 (callval #'val2 "1")))
  (is-true (nth-value 1 (callval #'val2 "TRUE")))
  (is-true (nth-value 1 (callval #'val2 "t")))
  (is-true (nth-value 1 (callval #'val2 "false")))
  (is-true (nth-value 1 (callval #'val2 "0")))
  (is-true (nth-value 1 (callval #'val2 "NIL")))
  (is-false (nth-value 1 (callval #'val2 "")))
  (is-false (callval #'val2 "yes"))
  (is-false (callval #'val2 "false"))
  (is-false (callval #'val2 "0"))
  (is-false (callval #'val2 "NIL"))

  )



