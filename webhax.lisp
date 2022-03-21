;;;; webhax.lisp

(in-package #:webhax)

;;; "webhax" goes here. Hacks and glory await!

(defparameter *set-content-type* nil)

(defun set-route (app route func)
  "Thin wrapper around setf ningle:route in case we stop using ningle."
  (setf (ningle:route app route) func))

(defun normalize-ningle-input (input)
  (if ningle:*session*
      (values (when-let ((inp (assoc :splat input)))
                ;;FIXME: Should probably only remove zero lengths in last pos.
                (remove-if (lambda (x)
                             (and (stringp x) (= 0 (length x))))
                           (cl-utilities:split-sequence #\/ (second inp))))
              (remove-if (lambda (x) (and (consp x) (eq :splat (car x))))
                         input)
              ningle:*session*)
      (values (when (boundp '*regular-web-input*) *regular-web-input*)
              (when (boundp '*key-web-input*) *key-web-input*)
              (or (when (boundp '*session*) *session*)
                  (when (boundp '*web-env*) (session-from-env *web-env*))))))

;;;Replace stub in webhax-core
(setf (symbol-function 'webhax-core:normalize-input) #'normalize-ningle-input)

(defun set-content-type (ctype)
  ;;FIXME: Don't know why this broke
  ;;(setf (lack.response:response-headers ningle:*response* :content-type) ctype))
  (declare (ignore ctype))
  (error "Needs reimplementation"))

;;;FIXME: Deprecate?
(defun create-simple-route (app route-spec function &key content-type)
  (set-route app route-spec
             (input-function-wrapper function :content-type content-type)))

;;;FIXME: Deprecate?
(defmacro create-route ((app route-spec &key content-type)
                        (&rest valspecs)
                        &body body)
  `(set-route ,app ,route-spec
              (input-function-wrapper
               (lambda ()
                 (bind-validated-input ,valspecs ,@body))
               :content-type ,content-type)))

(defmacro mount-component ((component-name &key mount-id) &body parameters)
  "Produces html output to mount a named react component in place, creating a named div element, then creating a script element that renders the component in the div. Parameters are alternating keys and values, sent to the component as initial props. Values, therefore, are parenscript. Lisp values must be wrapped in (lisp ...).

Mount-id, when specified, causes the component to be mounted to the element named by mount-id, instead of in place."
  (let ((tagid (gensym "tagid")))
    `(let ((,tagid ,(or mount-id (mkstr (gensym (mkstr "mount-" component-name #\-))))))
       (html-out
         (when ,tagid (htm (:div :id ,tagid)))
         (:script
          :type "text/javascript"
          (str
           (ps
             (react:render
              (react:create-element ,component-name
                                    (create ,@parameters))
              (chain document
                     (get-element-by-id (lisp ,tagid)))))))))))

(defmacro mount-cljs-component ((initial-event &key mount-id) &body parameters)
  "  "
  (let ((tagid (gensym "tagid")))
    `(let ((,tagid ,(or mount-id (mkstr (gensym "mount-cljs-component-")))))
       (html-out
         (when ,tagid (htm (:div :id ,tagid)))
         (:script
          :type "text/javascript"
          (str
           (ps
             (let ((params
                     (create "mount-point" (lisp ,tagid) "entry-point" ,initial-event
                             ,@parameters)))
               (chain
                window
                (add-event-listener
                 "load"
                 (lambda (ev)
                   (chain flaglib2 init (server_side_setup params)))))))))))))

;;;FIXME: Assumes that ReactTestUtils is loaded
(defmacro test-component ((component-name func-name) &body parameters)
  "Defines a javascript function named func-name. The function will return a
detached DOM node containing the specified component. Meant as a testing equivalent
to mount-component."
  `(html-out
     (:script
      :type "text/javascript"
      (str
       (ps
         (defun ,func-name ()
           ((@ -react-test-utils render-into-document)
            (react:create-element ,component-name
                                  (create ,@parameters)))))))))


;;;;;;;;


(defmacro clack-server-manager (handler-var app &rest clackup-params)
  `(progn
     (defvar ,handler-var nil)
     (when ,handler-var
         (clack:stop ,handler-var))
     (setf ,handler-var
           (clack:clackup
            ,app
            ,@clackup-params))))

;;;;;;;;;;;;;;;;;;;
;;; Link collection
;;;;;;;;;;;;;;;;;;;

(defparameter *webhax-link-collection* (make-hash-table :test #'eq))

(defun register-link (key link &key (label ""))
  (setf (gethash key *webhax-link-collection*) (list link label)))

;;;;;;;;;;;;;;;;;;;
;;; Named text
;;;;;;;;;;;;;;;;;;;

(defparameter *named-text-locations* nil)

(defun named-text (name)
  (let ((fname
         (loop
            for dir in *named-text-locations*
            for file = (make-pathname :directory (pathname-directory dir)
                                       :type "md"
                                       :name (string-downcase (mkstr name)))
            if (probe-file file)
            return file
            finally (error "Couldn't find named text"))))
    (3bmd:parse-and-print-to-stream fname *webhax-output*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Javascript resource collector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun write-js-resources (path &rest function-symbols-and-strings)
  (with-open-file
      (fh path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (dolist (itm function-symbols-and-strings)
      (write-string (princ-to-string
                     (typecase itm
                       (symbol (funcall (symbol-function itm)))
                       (string itm)))
             fh))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Header setting middleware
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun header-adder (pattern headers)
  "A simple middleware to add specified headers to the response from any urls that start with pattern."
  (lambda (app)
    (lambda (env)
     (if (starts-with-subseq pattern (getf env :path-info))
         (let ((response (funcall app env)))
           (list* (car response)
                  (concatenate 'list (second response) headers)
                  (cddr response)))
         (funcall app env)))))
