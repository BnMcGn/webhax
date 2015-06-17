
(def-package 

(in-package #:webhax-test)


(defvar input
  '(("one" "2" "3")
    ((:a . "thing") ("B" . "an@email.addr") (:b . "snk@ack.r") 
     (c . "2001-02-30"))))

(defun test-input-normalize (input)
  (values (first input) (second input)))

(defmacro twrap (&body body)
  `(let ((webhax::*input-normalize* #'test-input-normalize))
     ,@body))

(twrap
  (bind-validated-input 
      (input
       (item1 (ratify-wrapper :overlength))
       (item2 (ratify-wrapper :integer))
       (item3 (ratify-wrapper :integer) :optional t)
       (a (ratify-wrapper :overlength) :key t)
       (b (ratify-wrapper :email) :key t :multiple t)
       ((d 5) (ratify-wrapper :integer) :key t))
    (list item1 item3 item2 d b a)))