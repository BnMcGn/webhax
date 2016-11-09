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

  (is (equal "x" (callval #'val3 "x")))
  (is-false (nth-value 1 (callval #'val3 "a")))
  (is (null (callval #'val3 "")))

  (is (null (callval #'val4 "")))
  (is (eq 'local-time:timestamp (type-of (callval #'val4 "2000-01-01")))))

(defun batch-val ()
  (list
   :one
   (normalize-fieldspec-body '((:picksome :options (1 3 5))))
   :two
   (normalize-fieldspec-body '((:string)))
   :three
   (normalize-fieldspec-body '((:yesno)))))

(test validate-batch
  (multiple-value-bind (results sig)
      (validate-batch
       '((:one . "1") (:two . "qwerzx") (:three . "true") (:one . "5"))
       (batch-val))
    (is-true sig)
    (is (listp (gethash :one results)))
    (is (equal "qwerzx" (gethash :two results)))
    (is (gethash :three results))
    (is (= 2 (length (gethash :one results)))))
  (multiple-value-bind (results sig)
      (validate-batch
       '((:one . "1") (:two . "qwerzx") (:three . "true") (:one . "2"))
       (batch-val))
    (is-false sig)
    (is (= 1 (length (alexandria:hash-table-keys results))))
    (is (equal '(:one) (alexandria:hash-table-keys results)))))

(test validate-batch-translate
  (let ((data (hu:plist->hash '(:two "yyzz" :three nil)))
        (table (hu:plist->hash '(:one :x :two :y :three :z))))
    (multiple-value-bind (results sig)
        (validate-batch
         '((:x . "1") (:z . "true") (:x . "5"))
         (batch-val)
         :existing-hash data :translation-table table :edit t)
      (is-true sig)
      (is-true (gethash :three results))
      (is (listp (gethash :one results)))
      (is (equal "yyzz" (gethash :two results))))))



