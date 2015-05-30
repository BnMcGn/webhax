;;;; client.lisp

(in-package #:webhax)

;;; Ask clientside/javascript stuff goes here.

(defun get-q-functype-symbol (q)
  (symb 'client- (get-q-type q)))

(defun generate-q-data (q)
  `(create
    :function ,(get-q-functype-symbol q)
    :label ,(make-q-label q)
    ,@(awhen2 (fetch-keyword :source q)
	      (list :source it))
    ,@(awhen2 (fetch-keyword :source-url q)
	      (list :source-url it))
    ,@(aif2 (fetch-keyword :default q)
	      (list :default it) (list :default nil))))

(defun generate-client-data (symbols qs)
  `(create
     ,@(collecting
	(loop for s in symbols
	     for q in qs
	     do (progn 
		  (collect s) 
		  (collect (generate-q-data q)))))))

(defun ps-control-lib ()
  (ps
    (defun display-specified-controls (target commands data)
      (setf (getprop data :_curr-disp) (chain commands :next))
      (setf (chain target inner-h-t-m-l)
	    (collecting-string
	     (dolist (k (chain -object (keys (chain commands :next))))
	       (collect (make-control k (getprop data k)))))))

    (defun update-form-data (stor data)
      (do-keyvalue (k v (chain data :next))
	(do-keyvalue (kk vv v)
	  (setf (getprop stor k kk) vv))))

    (defun get-control-parent-form (control)
      (if (equal (@ control tag-name) "FORM")
	  (@ control id)
	  (get-control-parent-form (@ control parent-element))))

    (defun control-updated (name control value) ;FIXME: doesn't have auto
      (let ((form (get-control-parent-form control)))
	(setf (getprop (chain document askdata) form name :current) value)))

    (defun post-ask-update (form &optional (url (lisp *ask-control-url*)))
      (let ((data (-object))
	    (currkeys
	     (chain -object (keys 
	       (getprop document 'askdata form :_curr-disp)))))
	(dolist (k currkeys)
	  (let ((v (getprop document 'askdata form k)))
	    (if (chain v (has-own-property :current))
		(progn (setf (getprop data k) (@ v current))
		       (setf (@ v current-saved) (@ v current))
		       (delete (@ v current)))
		(if (chain v (has-own-property :default))
		    (setf (getprop data k) (@ v default))))))
	(chain $ (get-j-s-o-n (+ url form) data
		   (lambda (x)
		     (update-form-data 
		      (getprop (chain document askdata) form) x)
		     (display-specified-controls
		      (chain document (get-element-by-id form) 
			     first-element-child)
		      x
		      (getprop (chain document askdata) form)))))))))		

(defun ps-widget-lib ()
  (ps
    ;FIXME: should decide between current and original default for fill.
    (defun make-control (name data)
      (who-ps-html
       (:div (:span (getprop data :label)) 
	     (:span (funcall (getprop data :function) name data)))))

   (defmacro updatecode ()
      '(ps-inline (control-updated name this (@ this value))))
    
    ;FIXME: default needs better handling.
    (defun client-yesno (name params)
      (ps-html
       ((:input :type "radio" :name name :value "true" 
		:onchange (updatecode)
		(getprop params :default) :checked "checked") "Yes")
       ((:input :type "radio" :name name :value "false" 
		:onchange (updatecode)
		(not (getprop params :default)) :checked "checked") "No")))

    (defun client-pickone (name params)
      (collecting-string
       (dolist (x (getprop params :source))
	 (collect 
	   (ps-html
	    ((:input :type "radio" :name name :value x
	       :onchange (updatecode)
	       (eq (getprop params :default) x) :checked "checked") x))))))

    (defmacro make-simple-client-control (name type)
      `(defun ,(lisp (symb 'client- name)) (name params)
	 (ps-html
	  ((:input :type ,(lisp (string-downcase (mkstr type))) :name name
		   :value (getprop params :default)
		   :onchange (updatecode))))))
    
    (make-simple-client-control string text)
    (make-simple-client-control email email)
    (make-simple-client-control number number)
    (make-simple-client-control date date)
    (make-simple-client-control month month)
    (make-simple-client-control datetime datetime)
    (make-simple-client-control datetime-local datetime-local)))

(defun ps-ask-lib ()
  (concatenate
   'string
   (ps-gadgets)
   (ps-control-lib)
   (ps-widget-lib)))


(defun ask-page-insert (nbody qs names)
  "The part of an Ask that gets stuck into the web page, including the 
HTML form."
  `(let* ((formname (register-ask-manager
		     ,(create-ask-manager nbody qs names)))
	  (initial-display (call-ask-manager formname nil)))
     (html-out 
       (:form 
	:id formname
	(:div)
	(:input :type "button"
		:value "Next"
		:onclick 
		(let ((*js-string-delimiter* #\"))
		  (ps-inline (post-ask-update (lisp formname))))))
       (:script
	:type "text/javascript"
	(str (ps
	  (let ((formname (lisp formname))
		(formdata ,(generate-client-data names qs))
		(initstate 
		 (lisp-raw (json:encode-json-alist-to-string initial-display))))
	    (unless (chain document (has-own-property "askdata"))
	      (setf (chain document askdata) (create)))
	    (setf (getprop (chain document askdata) formname) formdata)
	    (display-specified-controls 
	     (chain document (get-element-by-id formname) first-element-child)
	     initstate
	     (getprop (chain document askdata) formname)))))))))
