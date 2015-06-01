;;;; webhax.lisp

(in-package #:webhax)

;;; "webhax" goes here. Hacks and glory await!

(defvar *session*)

(defparameter *webhax-output* *standard-output*)

(defmacro html-out (&body body)
  "Outputs HTML to standard out."
  `(with-html-output (*webhax-output* nil :indent t) ,@body))

(defpsmacro do-keyvalue ((key val obj) &body body)
  (let ((obj-v (gensym)))
    `(let ((,obj-v ,obj))
       (for-in (,key ,obj-v)
	       (if (chain ,obj-v (has-own-property ,key))
		   (let ((,val (getprop ,obj-v ,key)))
		     ,@body))))))

(defpsmacro collecting-string (&body body)
  (let ((res (gensym)))
    `(let ((,res ""))
       (labels ((collect (itm)
		  (setf ,res (+ ,res itm))))
	 ,@body)
       ,res)))

(defpsmacro collecting (&body body)
  (let ((res (gensym)))
    `(let ((,res (array)))
       (labels ((collect (itm)
		  (chain ,res (push itm))))
	 ,@body)
       ,res)))


(defun ps-gadgets ()
  (ps
    (defun say (thing)
      (chain console (log thing)))
    )); End ps-gadgets


  