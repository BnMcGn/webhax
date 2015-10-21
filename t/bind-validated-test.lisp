(in-package #:webhax-test)

(in-suite webhax-test)

(defvar input
  '(("one" "2" "3")
    ((:a . "thing") ("B" . "an@email.addr") (:b . "snk@ack.r")
     (c . "2001-02-30"))))

(defmacro twrap (&body body)
  `(destructuring-bind (webhax::*regular-web-input* webhax::*key-web-input*)
       input
     ,@body))

(defun testfunc1 ()
  (twrap
    (bind-validated-input
  ((item1 (ratify-wrapper :overlength))
   (item2 (ratify-wrapper :integer))
   (item3 (ratify-wrapper :integer) :optional t)
   (a (ratify-wrapper :overlength) :key t)
   (b (ratify-wrapper :email) :key t :multiple t)
   ((d 5) (ratify-wrapper :integer) :key t))
      (list item2 item3 d item1 b a))))

(defun testfunc2 ()
  (twrap
    (bind-validated-input
  ((item1 (ratify-wrapper :overlength))
   (item2 (ratify-wrapper :integer) :rest t))
      (values item1 (reduce #'+ item2)))))

(defun testfunc3 ()
  (twrap
    (bind-validated-input
  ((item1 (ratify-wrapper :overlength))
   (item2 (ratify-wrapper :integer))
   (item3 (ratify-wrapper :integer)))
      (values (+ item2 item3) item1))))

(defun testfunc4 ()
  (twrap
    (bind-validated-input
        ((item1 (ratify-wrapper :overlength) :rest t)
         (a (ratify-wrapper :overlength) :key t :required t))
      (declare (ignore item1))
      a)))

(defun testfunc5 ()
  (twrap
    (bind-validated-input
        ((item1 (ratify-wrapper :overlength) :rest t)
         (d (ratify-wrapper :overlength) :key t :required t))
      (declare (ignore item1))
      d)))

(defun testfunc6()
  (twrap
    (bind-validated-input
        ((item1 (lambda (x) (values x t)) :rest t))
      item1)))

(defun testfunc7()
  (twrap
    (let ((webhax::*regular-web-input* '("one")))
      (bind-validated-input
         ((item1 (lambda (x) (values x t)) :rest t))
       item1))))

(test bind-validated-input
  (is (= 10
   (reduce #'+ (remove-if-not #'integerp (testfunc1)))))
  (is (= 2 (length (fifth (testfunc1)))))
  (is (every #'stringp (fifth (testfunc1))))
  (is (= 5 (nth-value 1 (testfunc2))))
  (is (string-equal "one" (testfunc2)))
  (is (= 5 (testfunc3)))
  (is (string-equal "thing" (testfunc4)))
  (signals simple-error (testfunc5))
  (is (equal '("one" "2" "3") (testfunc6)))
  (is (equal '("one") (testfunc7))))
