
(in-package :cl-user)
(defpackage #:webhax-ningle
  (:use #:cl
	#:ningle
	#:clack.request
	#:gadgets)
  (:shadowing-import-from #:ningle.context #:make-request)
  (:export #:*request* ;Begin borrowed from clack.request
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

	   #:*session* ;ningle's *session*; a hash table.
	   #:output-to-string?
	   #:set-content-type
	   #:input-normalize))

(in-package :webhax-ningle)

; Webhax, like clack, uses a subset of hunchentoot's *request* object api.

(defun input-normalize (input)
  (values (awhen (assoc :splat input) (split-sequence #\/ (cdr it)))
	   (remove-if (lambda (x) (and (consp x) (eq :splat (car x))))
		      input)))

(defun set-content-type (ctype)
  (setf (clack.response:headers *response* :content-type) ctype))

(defparameter *input-normalize* #'input-normalize)
(defparameter *set-content-type* #'set-content-type)
(defparameter *output-to-string?* t)

