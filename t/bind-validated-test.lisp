
(in-package #:webhax-test)

(in-suite webhax-test)

(defvar input
  '(("one" "2" "3")
    ((:a . "thing") ("B" . "an@email.addr") (:b . "snk@ack.r") 
     (c . "2001-02-30"))))

(defun test-input-normalize (input)
  (values (first input) (second input)))

(defmacro twrap (&body body)
  `(let ((webhax::*input-normalize* #'test-input-normalize))
     ,@body))

(defun testfunc1 ()
  (twrap
    (bind-validated-input 
	(input
	 (item1 (ratify-wrapper :overlength))
	 (item2 (ratify-wrapper :integer))
	 (item3 (ratify-wrapper :integer) :optional t)
	 (a (ratify-wrapper :overlength) :key t)
	 (b (ratify-wrapper :email) :key t :multiple t)
	 ((d 5) (ratify-wrapper :integer) :key t))
      (list item2 item3 d item1 b a))))

(test bind-validated-input1
  (is (= 10
	 (reduce #'+ (remove-if-not #'integerp (testfunc1)))))
  (is (= 2 (length (fifth (testfunc1)))))
  (is (every #'stringp (fifth (testfunc1)))))