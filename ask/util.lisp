;;;; util.lisp

(in-package #:webhax)

(defvar *ask-control-url* "/ask-data/")

(defvar *ask-formname* nil)
(defvar *ask-prefills* nil)

(let ((counter 0))
  (defun create-numbered-name (name)
    (when (> counter (- most-positive-fixnum 2))
      (setf counter 0))
    (mkstr name (incf counter))))

;;;Validation

(defun %q-validator (q)
  (webhax-validate:normalize-fieldspec-body
   (cddr q)))

(defun %q-real-name (q)
  (second q))





