;;;; webhax.lisp

(in-package #:webhax)

;;; "webhax" goes here. Hacks and glory await!

(defparameter *webhax-output* *standard-output*)

(defmacro html-out (&body body)
  "Outputs HTML to standard out."
  `(with-html-output (*webhax-output* nil :indent t) ,@body))

(defpsmacro do-keyvalue ((key val obj) &body body)
  (let ((obj-v (gensym)))
    `(let ((,obj-v ,obj))
       (for-in (,key ,obj-v)
	       (if (chain ,obj-v (has-own-property ,key))
		   (let ((,val (getprop ,obj-v ,key)))
		     ,@body))))))

(defpsmacro collecting-string (&body body)
  (let ((res (gensym)))
    `(let ((,res ""))
       (labels ((collect (itm)
		  (setf ,res (+ ,res itm))))
	 ,@body)
       ,res)))

(defpsmacro collecting (&body body)
  (let ((res (gensym)))
    `(let ((,res (array)))
       (labels ((collect (itm)
		  (chain ,res (push itm))))
	 ,@body)
       ,res)))


(defun ps-gadgets ()
  (ps
    (defun say (thing)
      (chain console (log thing)))

    (defun member (item list)
      (>= (chain list (index-of item)) 0))

    )); End ps-gadgets

(defparameter *input-normalize* nil)
(defparameter *output-to-string?* nil)
(defparameter *set-content-type* nil)
(defparameter *activate-routes* nil)
(defparameter *host-object* nil)
(defparameter *host-package* nil)
(defvar *regular-web-input*)
(defvar *key-web-input*)
(defvar *request*)
(defvar *session*)

(defun initialize (host-type &key host)
  (setf *host-object* host)
  (let ((hostpack (find-package (symb 'webhax- host-type))))
    (unless hostpack
      (error (format nil "Package ~a not found" (symb 'webhax- host-type))))
    (setf *host-package* hostpack)
    (let ((*package* (find-package :webhax)))
      (use-package hostpack))
    (dolist (sym '(*input-normalize* *set-content-type* *output-to-string?*
		   *activate-routes*))
      (setf (symbol-value sym) (symbol-value 
				(symbolize sym :package hostpack))))
    (funcall *activate-routes* *registered-routes* host)))

;FIXME: Might be better to rely on clack - once it settles down - as an abstraction layer, rather than making yet another.

(defun input-function-wrapper (handler &key content-type)
  (lambda (input)
    (when content-type
      (funcall *set-content-type* content-type))
    (let ((*request* (symbol-value 
		      (symbolize '*request* :package *host-package*)))
	  (*session* (symbol-value 
		      (symbolize '*session* :package *host-package*))))
      (multiple-value-bind (*regular-web-input* *key-web-input*)
	  (funcall *input-normalize* input)
	(bind-webspecials (nth-value 1 (funcall *input-normalize* input))
	  (if *output-to-string?*
	      (with-output-to-string (*webhax-output*)
		(funcall handler))
	      (funcall handler)))))))

(defvar *registered-routes* (make-hash-table))

(defun create-simple-route (name function &key route-spec content-type)
  (setf (gethash name *registered-routes*) 
	(list (input-function-wrapper function :content-type content-type)
	      route-spec)))

(defmacro create-route ((name &key route-spec content-type) 
			(&rest valspecs) 
			&body body)
  `(setf (gethash ,name *registered-routes*)
	 (list
	  (input-function-wrapper
	   (lambda ()
	     (bind-validated-input ,valspecs ,@body))
	   :content-type ,content-type)
	  ,route-spec)))

(defun output-string (string)
  (princ string *webhax-output*))

(defun alist->ps-object-code (alist &key (wrap t))
  (let ((res
	 (collecting
	     (dolist (item alist)
	       (collect (car item))
	       (collect (cdr item))))))
    (if wrap (cons 'ps:create res) res)))

;;For things that send multiple items with "[]" appended to the var name.
(defun eq-symb-multiple (a b)
  (or (eq-symb a b)
      (and (= (length (mkstr a)) (+ 2 (length (mkstr b))))
	   (eq-symb a (symb b '[])))
      (and (= (+ 2 (length (mkstr a))) (length (mkstr b)))
	   (eq-symb (symb a '[]) b))))


;;;;;
; Menu stuff
;;;;;

;items format: (('mainitem <'subitems...> urlstring), more items...)
;active format: match itemspec above, omitting urlstring
(defparameter *menu-items* nil)
(defvar *menu-active* nil)