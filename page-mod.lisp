
(in-package :webhax)

(defun ps-page-mod ()
  (ps
    (defun page-mod (commands)
      (for-in (itm commands)
	(when (and (chain itm (has-own-property command))
		   (member (getprop itm command) '(:set-value :replace-with)))
	  (with-slots (target value) itm
	    (case cmd
	      (:set-value
	       (let ((tmp value))
		 (eval (+ target " = value"))))
	      (:replace-with
	       (chain ($ target) (replace-with value))))))))))

(defvar *page-mod-store* :toplevel)

(defun assemble-page-mod (&rest things)
  (let ((convert (if (eq *page-mod-store* :toplevel) 
		     #'json:encode-json #'identity))
	(*page-mod-store* (if (eq *page-mod-store* :toplevel) 
			      nil *page-mod-store*)))
    (dolist (itm (nreverse things))
      (if (functionp itm)
	  (funcall itm)
	  (push itm *page-mod-store*)))
    (funcall convert (nreverse *page-mod-store*))))

(defmacro set-value (locspec value)
  "Locspec is a parenscript variable expression ie. chain, getprop, @. Value is 
anything that can be converted to JSON by the json:encode-json function."
  `((:command . :set-value) (:target . (ps ,locspec)) (:value . ,value)))

(defmacro replace-with (locspec value)
  "Locspec is a specifier for the JQuery $ function. Value is a string. Html-out/cl-who output can also be used in the value position and will be captured to a string."
  `(let* ((other nil)
	  (htstring (with-output-to-string (*webhax-output*)
		      (setf other ,value))))
     (list (cons :command :replace-with)
	   (cons :target ,locspec)
	   (cons :value (if htstring htstring other)))))
