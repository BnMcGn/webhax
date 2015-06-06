;;;; util.lisp

(in-package #:webhax)


(defparameter *ask-control-types*
  '(:yesno :string :pickone :picksome :integer :date :month :datetime 
    :datetime-local))

(defvar *ask-control-url*)

(let ((counter 0))
  (defun create-numbered-name (name)
    (when (> counter (- most-positive-fixnum 2))
      (setf counter 0))
    (mkstr name (incf counter))))

(defun make-q-label (q)
  (if (stringp (third q))
      (third q)
      (thing-labels:thing-label (second q))))

(defun get-q-type (q)
    (aif (first-match *ask-control-types* (lambda (x) (member x (cddr q))))
       it
       (error "Control type not found")))

;;;Validation


(let ((ratify-matches
       '(:email :number :date :month :datetime :datetime-local :integer)))
  (defun %ratify-sym-for-type (tsym)
    (cond ((member tsym ratify-matches) tsym)
	  ((eq tsym :yesno) :boolean)
	  (t nil))))

(defun %default-validator (q)
  (aif (%ratify-sym-for-type (get-q-type q))
       (ratify-wrapper it)
       (case (get-q-type q)
	 (:pickone 
	  (mkparse-in-list (fetch-keyword :source q)))
	 (:picksome
	  (mkparse-in-list (fetch-keyword :source q)))
	 (otherwise ;Default - limits input length
	  (ratify-wrapper :overlength))))) 

(defun %q-validator (q)
  "Validator spec: function that returns (values <adjusted val> t) if good, or (values <error message> nil) if bad."
  (%default-validator q));FIXME: add user validator support



