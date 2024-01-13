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
   #:string-equal-multiple
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
   #:env-from-url
   #:input-function-wrapper
   #:normalize-input
   #:under-path-p
   #:repath-clack-env
   #:url-from-env
   #:session-from-env
   #:*default-content-type*
   #:*clack-app*
   #:with-content-type
   #:as-html
   #:as-json
   #:*menu-items*
   #:*menu-active*
   #:write-html-file
   #:web-fail-404
   #:web-fail-400))

(in-package #:webhax-core)

;;; "webhax" goes here. Hacks and glory await!

(defparameter *webhax-output* *standard-output*)

(defmacro html-out (&body body)
  "Outputs HTML to standard out."
  `(with-html-output (*webhax-output* nil :indent t) ,@body))

(defmacro html-out-str (&body body)
  `(with-html-output-to-string (*webhax-output* nil :indent t) ,@body))

(defvar *regular-web-input* nil)
(defvar *key-web-input* nil)
(defvar *session* nil)
(defvar *request* nil)
(defvar *response* nil)
(defvar *web-env*)
(defvar *clack-app* nil)

(defparameter *default-content-type* "text/html")

(defmacro write-html-file (name &body body)
  `(with-output-to-file (*webhax-output* ,name)
     ,@body))

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
       (progn ,@body)
     (web-fail (fail)
       (with-slots (text response) fail
         `(,response nil (,text))))))

(defun web-fail-404 ()
  (error 'web-fail :response 404 :text "Page not found"))

(defun web-fail-400 (&optional message)
  (error 'web-fail :response 400 :text (or message "Invalid Request")))

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

(defun under-path-p (path testpath)
  (let ((len (length path)))
    (cond
      ((string= path testpath) "/")
      ((and (< len (length testpath))
            (string= testpath path :end1 len)
            (char= (aref testpath len) #\/))
       (subseq testpath len))
      (t nil))))

(defun repath-clack-env (env newpath)
  (mapcan-by-2
   (lambda (k v)
     (if (eq :path-info k)
         (list :path-info newpath)
         (list k v)))
   env))

(defun url-from-env (env)
  "Extract the current request url from a clack environment."
  (strcat
   (format nil "~a://" (string-downcase (mkstr (or (getf env :url-scheme)
                                                   (getf env :uri-scheme)))))
   (getf env :server-name)
   (awhen (getf env :server-port)
     (unless (= 80 it)
       (format nil ":~d" it)))
   (getf env :request-uri)))

(defun session-from-env (env)
  (getf env :lack.session))

(defun normalize-input (&optional input)
  (declare (ignore input))
  (error "No input handler activated. This code shouldn't be reached."))

(defun input-function-wrapper (handler &key (content-type "text/html") headers)
  (lambda (&optional input)
    ;; FIXME: This the right place? Feels like a crutch for ningle.
    (handle-web-fail
      (list 200 (list* :content-type content-type headers)
            (list
             (multiple-value-bind
                   (*regular-web-input* *key-web-input* *session*)
                 (normalize-input input)
               (with-output-to-string (*webhax-output*)
                 (funcall handler))))))))

(eval-always
  (defmacro with-content-type (ctype &body body)
    `(progn
       (setf (getf (lack.response:response-headers *response*) :content-type)
             ,ctype)
       ,@body)))

(defmacro as-html (&body body)
  `(with-content-type "text/html" ,@body))

(defmacro as-json (&body body)
  `(with-content-type "application/json" ,@body))

;;;;;
;; Menu stuff
;;;;;

;;items format: (('mainitem <'subitems...> urlstring), more items...)
;;active format: match itemspec above, omitting urlstring
(defparameter *menu-items* nil)
(defvar *menu-active* nil)

