;;;; webhax-core.lisp

(in-package #:cl-user)

(defpackage #:webhax-core
  (:use #:cl #:gadgets #:alexandria #:clack #:lack.request #:lack.response
        #:cl-who)
  (:export
   #:*webhax-output*
   #:html-out
   #:*regular-web-input*
   #:*key-web-input*
   #:output-string
   #:eq-symb-multiple
   #:logged-in-p
   #:clack-tool
   #:function-wrapper
   #:execute
   #:multiple-key-p
   #:start-test-app
   #:*session*
   #:get-user-name
   #:get-display-name
   #:*web-env*
   #:*response*
   #:*request*
   #:html-out-str
   #:web-fail
   #:handle-web-fail
   #:env-from-url))

(in-package #:webhax-core)

;;; "webhax" goes here. Hacks and glory await!

(defparameter *webhax-output* *standard-output*)

(defmacro html-out (&body body)
  "Outputs HTML to standard out."
  `(with-html-output (*webhax-output* nil :indent t) ,@body))

(defmacro html-out-str (&body body)
  `(with-html-output-to-string (*webhax-output* nil :indent t) ,@body))

(defvar *regular-web-input*)
(defvar *key-web-input*)
(defvar *session* nil)
(defvar *request* nil)
(defvar *response* nil)
(defvar *web-env*)

(defun output-string (string)
  (princ string *webhax-output*))

(defun multiple-key-p (stritem)
  (ends-with-subseq "[]" stritem))

;;;Web-fail: Return an http error response code with the lisp condition system.

(define-condition web-fail (error)
  ((text :initarg :text :reader text)
   (response :initarg :response :reader response)))

(defmacro handle-web-fail (&body body)
  `(handler-case
       ,@body
     (web-fail (fail)
       (with-slots (text response) fail
         `(,response nil (,text))))))

(defun env-from-url (url)
  "Create a minimal clack-style env from a url. Mostly for quick testing."
  (let ((uri (quri:uri url)))
    (list
     :query-parameters (quri:uri-query-params uri)
     :request-uri url
     :path-info (quri:uri-path uri)
     :query-string (quri:uri-query uri)
     :server-name (quri:uri-host uri)
     :url-scheme (quri:uri-scheme uri))))
