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

    (defun member (item list)
      (>= (chain list (index-of item)) 0))

    )); End ps-gadgets

(defparameter *input-normalize* nil)
(defparameter *output-to-string?* nil)
(defparameter *set-content-type* nil)
(defparameter *webhax-host* nil)

(defun route-handler-wrapper (handler &key content-type)
  (lambda (input)
    (print *input-normalize*)
    (when content-type
      (funcall *set-content-type* content-type))
    (bind-webspecials (nth-value 1 (funcall *input-normalize* input))
      (if *output-to-string?*
	  (with-output-to-string (*webhax-output*)
	    (funcall handler input))
	  (funcall handler input)))))

(defun initialize (host-type &key host)
  (setf *webhax-host* host)
  (let ((hostpack (find-package (symb 'webhax- host-type))))
    (unless hostpack
      (error (format nil "Package ~a not found" (symb 'webhax- host-type))))
    (let ((*package* (find-package :webhax)))
      (use-package hostpack))
    (dolist (sym '(*input-normalize* *set-content-type* *output-to-string?*))
      (setf (symbol-value sym) (symbol-value 
				(symbolize sym :package hostpack))))))
