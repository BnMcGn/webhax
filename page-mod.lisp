
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

(defvar *page-mod-store* :toplevel)

(defun assemble-page-mod (&rest things)
  (let ((convert (if (eq *page-mod-store* :toplevel) 
		     #'json:encode-json-to-string #'identity))
	(*page-mod-store* (if (eq *page-mod-store* :toplevel) 
			      nil *page-mod-store*)))
    (dolist (itm (nreverse things))
      (if (functionp itm)
	  (funcall itm)
	  (push itm *page-mod-store*)))
    (print (funcall convert (nreverse *page-mod-store*)))))

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

(defun page-mod-url ()
  "/page-mod/")

(defmacro define-page-mod (name (&rest validation-list) &body body)
  (with-gensyms (input)
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
       (defun ,name (,input)
	 (bind-validated-input (,input ,@validation-list)
	   ,@body))
       