(in-package #:webhax-core)

;;;
;;; Base class for making webhax services as clack middleware components.
;;;

(defun logged-in-p (env)
  (getf env :remote-user))

(defun url-splitter (url base)
  (split-sequence #\/ (subseq url (length base)) :remove-empty-subseqs t))

(defclass clack-tool (clack.middleware:<middleware>)
  ((base-url :type string
             :initarg :base-url
             :initform "")
   (login-p :initarg :login-p
            :initform t)
   (default-content-type :type string
                         :initarg :default-content-type
                         :initform "text/json")
   (function :type function
                     :initarg :function
                     :initform (lambda () ""))))

(defmethod call ((this clack-tool) env)
  (with-slots (base-url login-p) this
    (if (starts-with-subseq base-url (getf env :request-uri))
        (if (and login-p (not (logged-in-p env)))
            '(403 nil ("This service not available without login."))
            (function-wrapper this env))
        (call-next this env))))

(defgeneric function-wrapper (obj env)
  (:method ((this clack-tool) env)
    (with-slots (default-content-type function base-url) this
      (let* ((*request* (clack.request:make-request env))
             (*response* (clack.response:make-response 200))
             (*key-web-input* (clack.request:parameter *request*))
             (*regular-web-input*
               (url-splitter (clack.request:path-info *request*) base-url)))
        (setf (clack.response:headers *response* :content-type)
              default-content-type)
        (setf (clack.response:body *response*)
              (list (execute this)))
        (clack.response:finalize *response*)))))

(defgeneric execute (obj)
  (:method ((this clack-tool))
    (with-slots (function) this
      (when function
        (funcall function)))))

;;;FIXME: Implement url producer?
