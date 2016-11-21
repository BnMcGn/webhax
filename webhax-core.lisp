;;;; webhax.lisp

(in-package #:webhax-core)

;;; "webhax" goes here. Hacks and glory await!

(defparameter *webhax-output* *standard-output*)

(defmacro html-out (&body body)
  "Outputs HTML to standard out."
  `(with-html-output (*webhax-output* nil :indent t) ,@body))

(defmacro html-out-str (&body body)
  `(with-html-output-to-string (*webhax-output* nil :indent t) ,@body))

(defvar *regular-web-input*)
(defvar *key-web-input*)
(defvar *session* nil)
(defvar *request* nil)
(defvar *response* nil)
(defvar *web-env*)

(defun output-string (string)
  (princ string *webhax-output*))

(defun multiple-key-p (stritem)
  (ends-with-subseq "[]" stritem))

;;;Web-fail: Return an http error response code with the lisp condition system.

(define-condition web-fail (error)
  ((text :initarg :text :reader text)
   (response :initarg :response :reader response)))

(defmacro handle-web-fail (&body body)
  `(handler-case
       ,@body
     (web-fail (fail)
       (with-slots (text response) fail
         `(,response nil (,text))))))

