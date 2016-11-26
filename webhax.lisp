;;;; webhax.lisp

(in-package #:webhax)

;;; "webhax" goes here. Hacks and glory await!

(defparameter *set-content-type* nil)

(defun set-route (app route func)
  "Thin wrapper around setf ningle:route in case we stop using ningle."
  (setf (ningle:route app route) func))

(defun input-normalize (input)
  (values (awhen (assoc :splat input)
                 ;;FIXME: Should probably only remove zero lengths in last pos.
                 (remove-if (lambda (x)
                              (and (stringp x) (= 0 (length x))))
                            (split-sequence #\/ (second it))))
          (remove-if (lambda (x) (and (consp x) (eq :splat (car x))))
                     input)))

(defun set-content-type (ctype)
  ;;FIXME: Don't know why this broke
  ;;(setf (lack.response:response-headers ningle:*response* :content-type) ctype))
  (declare (ignore ctype))
  (error "Needs reimplementation"))
 
(defun input-function-wrapper (handler &key (content-type "text/html"))
  (lambda (input)
    (list 200 (list :content-type content-type)
          (list
           (multiple-value-bind (*regular-web-input* *key-web-input*)
               (input-normalize input)
             (let ((*session* ningle:*session*))
               (bind-webspecials (nth-value 1 (input-normalize input))
                 (with-output-to-string (*webhax-output*)
                   (funcall handler)))))))))

;;;FIXME: *webhax-output* rebind to string is not taking effect.
(defmacro quick-page (&rest parts-and-main)
  (let ((parts (butlast parts-and-main))
        (main (last-car parts-and-main)))
    `(input-function-wrapper
      (define-page nil
          (,*metaplate-default-parts*
           ,@parts
           ,@(when main `((add-part :@main-content ,main))))
        (,*metaplate-default-layout*)))))

(defun create-simple-route (app route-spec function &key content-type)
  (set-route app route-spec
             (input-function-wrapper function :content-type content-type)))

(defmacro create-route ((app route-spec &key content-type)
                        (&rest valspecs)
                        &body body)
  `(set-route ,app ,route-spec
              (input-function-wrapper
               (lambda ()
                 (bind-validated-input ,valspecs ,@body))
               :content-type ,content-type)))

(defun alist->ps-object-code (alist &key (wrap t))
  (let ((res
   (collecting
       (dolist (item alist)
         (collect (car item))
         (collect (cdr item))))))
    (if wrap (cons 'ps:create res) res)))

;;;;;
; Menu stuff
;;;;;

;items format: (('mainitem <'subitems...> urlstring), more items...)
;active format: match itemspec above, omitting urlstring
(defparameter *menu-items* nil)
(defvar *menu-active* nil)


;;;;;;;;
;;; React
;;;;;;;;

(define-parts react
  (add-part :@javascript
            "https://cdnjs.cloudflare.com/ajax/libs/react/0.14.2/react.js")
  (add-part :@javascript
            "https://cdnjs.cloudflare.com/ajax/libs/react/0.14.2/react-dom.js")
  (add-part :@javascript #'react:build))

(define-parts redux
  (add-part :@javascript
            "https://cdnjs.cloudflare.com/ajax/libs/redux/3.5.2/redux.js")
  (add-part :@javascript
            "https://cdnjs.cloudflare.com/ajax/libs/react-redux/4.4.5/react-redux.js"))

(defmacro mount-component ((component-name &key mount-id) &body parameters)
  "Produces html output to mount a named react component in place, creating a named div element, then creating a script element that renders the component in the div. Parameters are alternating keys and values, sent to the component as initial props. Values, therefore, are parenscript. Lisp values must be wrapped in lisp or lisp-raw.

Mount-id, when specified, causes the component to be mounted to the element named by mount-id, instead of in place."
  (let ((tagid (unless mount-id
                 (mkstr (gensym (mkstr "mount-" component-name #\-))))))
    `(html-out
       (when ,tagid (htm (:div :id ,tagid)))
       (:script
        :type "text/javascript"
        (str
         (ps
           (react:render
            (react:create-element ,component-name
                            (create ,@parameters))
            (chain document
                   (get-element-by-id ,(or tagid mount-id))))))))))

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

(defun blank-key-p (item)
  "Determine if an item denotes an empty (root) url path."
  (cond
    ((stringp item) (not (boolify (length item))))
    ((keywordp item) (member item '(:empty :blank :/)))
    ((symbolp item) (eq item '/))
    (t nil)))

(defparameter *url-parentage* nil)

(defmacro url-case (&body clauses)
  (with-gensyms (matfunc input foundkey)
    (multiple-value-bind (keys cases)
        (with-collectors (keys< cases<)
          (dolist (clause clauses)
            (destructuring-bind (key clause-body) clause
              (if (atom key)
                  (cond
                    ((symbolp key)
                     (if (eq key 'otherwise)
                         (cases< `(t ,clause-body))
                         (if (blank-key-p key)
                             (cases< `((not ,input) ,clause-body))
                             (progn (cases< `((eq ,foundkey ,key)
                                              ,clause-body))
                                    (keys< key)))))
                    ((stringp key)
                     (if (string= "" key)
                         (cases< `((not ,input) ,clause-body))
                         (error
                          "Strings/pattern matching not supported yet.")))
                    ((numberp key)
                     (cases< `((eq ,foundkey ,key) ,clause-body))
                     (keys< key)))
                  (error "Non-atomic routes not supported yet.")))))
      `(let* ((,matfunc (gadgets:match-various ',keys))
              (,input (car *regular-web-input*))
              (,foundkey (when ,input (funcall ,matfunc ,input)))
              (*url-parentage* (cons (car *regular-web-input*)
                                     *url-parentage*))
              (*regular-web-input* (cdr *regular-web-input*)))
         (cond
           ,@cases)))))

(defparameter *default-content-type* "text/html")

(defun call-with-webhax-environment (func env)
  (handle-web-fail
    (let* ((*web-env* env)
           (*session* (session-from-env env))
           (*request* (lack.request:make-request env))
           (*response* (lack.response:make-response 200))
           (*key-web-input* (lack.request:request-parameters *request*))
           (*regular-web-input*
            ;;How to handle mounted sub-apps?
            (if (boundp '*regular-web-input*)
                *regular-web-input*
                (split-sequence
                 #\/ (lack.request:request-path-info *request*)
                 :remove-empty-subseqs t))))
      (funcall func))))

(defun wrap-with-webhax-environment (func params)
  (lambda (env)
    (call-with-webhax-environment
     (lambda ()
       (with-content-type *default-content-type*
        (let ((res (alexandria:ensure-list (apply func params))))
          (if (numberp (car res)) ;Test: is already a response
              res
              (progn
                (setf (lack.response:response-body *response*) res)
                (lack.response:finalize-response *response*))))))
     env)))

(defmacro define-webapp (name parameters &body body)
  (let ((name-int (symb name '-internal)))
    `(progn
       (defun ,name-int ,parameters
         ,@body)
       (defun ,name (&rest params)
         (wrap-with-webhax-environment #',name-int params)))))

(defvar *clack-app*)

(defmacro define-middleware (name parameters &body body)
  (let ((name-int (symb name '-internal)))
    `(let ((%app nil))
       ;;FIXME: Would be nice to use parameters here so that user options
       ;;show up in the hints.
       (defun ,name-int ,parameters
         (let ((*clack-app* %app))
           ,@body))
       (defun ,name (&rest params)
         (lambda (app)
           (setf %app app)
           (wrap-with-webhax-environment #',name-int params))))))

(defmacro with-content-type (ctype &body body)
  `(progn
     (setf (getf (lack.response:response-headers *response*) :content-type)
           ,ctype)
     ,@body))

(defmacro as-html (&body body)
  `(with-content-type "text/html" ,@body))

(defmacro as-json (&body body)
  `(with-content-type "application/json" ,@body))

(defun middleware-chain (&rest mwarez)
  "Join a chain of middlewares into a single middleware"
  (lambda (app)
    (reduce #'funcall mwarez :initial-value app :from-end t)))
