


(in-package :webhax-validate)

      
(defun ratify-wrapper (basename)
  (let ((*package* (find-package 'webhax-validate)))
    (let* ((p-name (mkstr 'parse- basename))
	   (test (or ;Not all ratify tests have a parse- form
		  (and (find-symbol p-name)
		       (fboundp (symbolize p-name))
		       (symbol-function 
			(symbolize p-name)))
		  (symbol-function 
		   (symbolize (mkstr 'test- basename))))))
      (lambda (x)
	(handler-case
	    (values (funcall test x) t)
	  (t (e) (values (message e) nil)))))))

(defmethod message ((condition condition))
  (princ-to-string condition))

(defvar *webhax-input-limit* 200)

(define-test overlength (item)
  (if (< (length item) *webhax-input-limit*)
    item
    (ratification-error item 
      (format nil "Field is longer than system limit of ~a chars."
	      *webhax-default-input-limit*))))

(defun mkparse-in-list (items)
  (lambda (item)
    (aif2only (first-match items (lambda (x) (eq-symb x item)))
	      (values it t)
	      (values "Value not in list of options" nil))))

