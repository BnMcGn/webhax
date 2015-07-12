
(in-package :webhax)

(defvar *defined-pagemods* (make-hash-table))

(defun ps-page-mod ()
  (apply #'concatenate 
   'string
   (ps
     (defun page-mod (commands)
       (dolist (itm commands)
	 (when (and (chain itm (has-own-property 'command))
		    (member (getprop itm 'command) '("setValue" "replaceWith")))
	   (with-slots (target value) itm
	     (case (getprop itm 'command)
	       ("setValue"
		(let ((tmp value))
		  (eval (+ target " = tmp"))))
	       ("replaceWith"
		(chain ($ target) (replace-with value))))))))
     
     (defun mod-test ()
       (chain $ (get-j-s-o-n "/page-mod/asdf" nil
		  (lambda (x)
		    (page-mod x))))))

   (hash-table-values *defined-pagemods*)))


(defun create-page-mod (&rest things)
  things)

(defmacro set-value (locspec value)
  "Locspec is a parenscript variable expression ie. chain, getprop, @. Value is 
anything that can be converted to JSON by the json:encode-json function."
  `(list
    (cons :command :set-value)
    (cons :target (string-right-trim '(#\;) (ps ,locspec))) 
    (cons :value ,value)))

(defmacro replace-with (locspec value)
  "Locspec is a specifier for the JQuery $ function. Value is a string. Html-out/cl-who output can also be used in the value position and will be captured to a string."
  `(let* ((other nil)
	  (htstring (with-output-to-string (*webhax-output*)
		      (setf other ,value))))
     (list (cons :command :replace-with)
	   (cons :target ,locspec)
	   (cons :value (if htstring htstring other)))))

(defun set-href (location)
  (set-value (@ window location href) location))

(create-simple-route 
 :page-mod
 (lambda ()
   (let ((*regular-web-input* (cdr *regular-web-input*))
	 (funcname (aif (match-a-symbol 
			 (car *regular-web-input*)
			 (hash-table-keys *defined-pagemods*))
			it
			(error "Page mod not found"))))
     (json:encode-json-to-string 
      (funcall (symbol-function funcname)))))
 :content-type "text/json")
    
(defun page-mod-url ()
  "/page-mod/")

(defmacro define-page-mod (name (&rest validation-list) &body body)
  `(progn
     (setf (gethash ,name *defined-pagemods*)
	   (ps 
	     (defun ,name (&rest params)
	       (let ((url (+ ,(concatenate 'string (page-mod-url) name)
			     "/"
			     (chain params 
				    (slice 0 (- (getprop params length) 1))
				    (join "/")))))
		 (chain $ (get-j-s-o-n 
			   url
			   (chain params (- (getprop params length) 1))
			   (lambda (x)
			     (page-mod x))))))))
     (defun ,name ()
       (bind-validated-input ,validation-list
	 ,@body))))
