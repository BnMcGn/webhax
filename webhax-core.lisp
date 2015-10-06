;;;; webhax.lisp

(in-package #:webhax-core)

;;; "webhax" goes here. Hacks and glory await!

(defparameter *webhax-output* *standard-output*)

(defmacro html-out (&body body)
  "Outputs HTML to standard out."
  `(with-html-output (*webhax-output* nil :indent t) ,@body))

(defvar *regular-web-input*)
(defvar *key-web-input*)

(defun output-string (string)
  (princ string *webhax-output*))

;;For things that send multiple items with "[]" appended to the var name.
(defun eq-symb-multiple (a b)
  (or (eq-symb a b)
      (and (= (length (mkstr a)) (+ 2 (length (mkstr b))))
     (eq-symb a (symb b '[])))
      (and (= (+ 2 (length (mkstr a))) (length (mkstr b)))
     (eq-symb (symb a '[]) b))))

