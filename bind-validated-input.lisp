;;;; bind-validated-input.lisp

(in-package #:webhax)


(defun %%make-regular-params-fetcher (valspecs)
  (let ((min-vals 0)
	(max-vals 0)
	(found-optional nil)
	(found-rest nil)
	(vlength (length valspecs)))
    (dolist (itm valspecs)
      (when found-rest (error "No regular parameters allowed after :rest"))
      (if (fetch-keyword :optional itm)
	  (progn 
	    (when (fetch-keyword :rest itm)
	      (error ":rest and :optional not allowed in same spec."))
	    (incf max-vals)
	    (setf found-optional t))
	  (if (fetch-keyword :rest itm)
	      (setf found-rest t)
	      (progn (incf min-vals) (incf max-vals)))))
    `(lambda (input)
       (let ((l-in (length input)))
	 (when (< l-in ,min-vals)
	   (error 
	    (format nil "~a parameters found, ~a required" l-in ,min-vals)))
	 (when (and (not ,found-rest) (> l-in ,max-vals))
	   (error
	    (format nil "Too many parameters: ~a found, ~a specified."
		    l-in ,max-vals)))
	 (if (< l-in ,vlength)
	     (values
	      (concatenate 'list input (make-list (- ,vlength l-in)))
	      (concatenate 'list (make-list l-in :initial-element t)
			   (make-list (- ,vlength l-in))))
	     (values 
	      input
	      (make-list ,vlength :initial-element t)))))))

(defun %spec-name (valspec)
  (car (ensure-list (car valspec))))

(defun %%make-key-param-fetcher (keyspec input)
  (bind-extracted-keywords (keyspec other :multiple :required)
    (with-gensyms (value)
      `(let ((,value ,(if multiple 
			 `(assoc-all ',(%spec-name other) 
				     ,input :test #'eq-symb)
			 `(assoc ',(%spec-name other)
				 ,input :test #'eq-symb))))
	 ,@(when required
		 `(unless ,value
		    (error 
		     (format nil 
			     "No value found for required keyword parameter ~a"
			     ,(%spec-name other)))))
	 (if ,value
	     (values ,(if multiple value `(cdr ,value)) t)
	     (values nil nil))))))

(defun %%default-decider (valspec inputform foundvar)
  (let ((filledp? (and (listp (car valspec)) 
		       (third (car valspec)) 
		       (symbolp (third (car valspec)))))
	(multiple (fetch-keyword :multiple valspec)))
    (with-gensyms (item found vitem valid) 
      (collecting
	(collect
	    (list (%spec-name valspec)
		  `(multiple-value-bind (,item ,found) ,inputform
		     ,@(when filledp?
			    `((setf ,foundvar ,found)))
		     (if ,found
			 (multiple-value-bind 
			       (,vitem ,valid)
			     ,(if multiple
				  `(funcall
				    (mkparse-all-members ,(second valspec))
				    ,item)
				  `(funcall-in-macro 
				    ,(second valspec) ,item))
			   (if ,valid
			       ,vitem
			       (error ,vitem)))
			 ,(if (listp (car valspec))
			      (second (car valspec))
			      nil)))))
	(when filledp?
	  (collect
	      (list (third (car valspec)) foundvar)))))))
	  
;valspec:
;(name -or- (name default filled-p)
;   validator 
;  &keys key (required multiple) -or- &keys (key nil) (rest optional))


(defmacro bind-validated-input ((input &rest valspecs) &body body)
  (multiple-value-bind (keys regular) 
      (splitfilter valspecs
		   (lambda (x)
		     (fetch-keyword :key x)))
    (with-gensyms (foundp regvals regfill reg-input key-input)
	`(multiple-value-bind (,reg-input ,key-input)
	     (funcall *input-normalize* ,input)
	   (multiple-value-bind (,regvals ,regfill)
	       (funcall ,(%%make-regular-params-fetcher regular) ,reg-input)
	     (let ((,foundp nil))
	       ,foundp ;whine-stopper
	       (let ,(apply #'concatenate 
		      'list
		      (loop for i from 0
			 for regspec in regular
			 append
			   (%%default-decider regspec `(values 
							(elt ,regvals ,i)
							(elt ,regfill ,i))
					      foundp))
		      (collecting
			(dolist (kspec keys)
			  (collect 
			      (%%default-decider 
			       kspec (%%make-key-param-fetcher kspec key-input)
			       foundp)))))
		 ,@body)))))))
    
