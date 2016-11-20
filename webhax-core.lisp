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

