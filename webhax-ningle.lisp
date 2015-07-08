
(in-package :cl-user)
(defpackage #:webhax-ningle
  (:use #:cl
	#:ningle
	#:clack.request
	#:gadgets)
  (:shadowing-import-from #:ningle.context #:make-request)
  (:export ;Begin borrowed from clack.request
	   #:content-length
	   #:content-type
	   #:parameter
	   #:query-string
	   #:referer
	   #:remote-addr
	   #:remote-port
	   #:request-method
	   #:request-uri
	   #:script-name
	   #:server-protocol
	   #:user-agent
	   ;End borrowed from clack.request
	   #:make-request

	   #:output-to-string?
	   #:set-content-type
	   #:input-normalize))

(in-package :webhax-ningle)

; Webhax, like clack, uses a subset of hunchentoot's *request* object api.

(defun input-normalize (input)
  (values (awhen (assoc :splat input)
	    ;FIXME: Should probably only remove zero lengths in last pos.
	    (remove-if (lambda (x) 
			 (and (stringp x) (= 0 (length x))))
		       (split-sequence #\/ (second it))))
	  (remove-if (lambda (x) (and (consp x) (eq :splat (car x))))
		     input)))

(defun set-content-type (ctype)
  (setf (clack.response:headers *response* :content-type) ctype))

(defun activate-routes (routes host-object)
  (maphash 
   (lambda (k v)
     (let ((route 
	    (if (second v)
		(second v)
		(format nil "/~a/*" (string-downcase (mkstr k))))))
       (setf (ningle:route host-object route) (car v))))
   routes))

(defparameter *input-normalize* #'input-normalize)
(defparameter *set-content-type* #'set-content-type)
(defparameter *output-to-string?* t)
(defparameter *activate-routes* #'activate-routes)



;;Debugging



'(ningle.app::mapper books::*app*) 
'(ningle.app::call books::*app* nil)
'(myway:dispatch (ningle.app::mapper books::*app*) "/tester/")
'(ningle:route books::*app* "/tester/")
'books::*session-store*
'(clack.middleware.session::store books::*session-store*)
'(clack.session.store::stash stor)

(defun get-session-container (session-store)
  (clack.session.store::stash 
   (clack.middleware.session::store session-store)))

(defun get-session1 (session-store)
  (first (alexandria:hash-table-values (get-session-container session-store))))