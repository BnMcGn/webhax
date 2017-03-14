;;;; route.lisp

(in-package #:cl-user)

(defpackage #:webhax-route
  (:use #:cl #:alexandria #:gadgets #:webhax-core)
  (:export
   #:quick-page
   #:url-case
   #:call-with-webhax-environment
   #:wrap-with-webhax-environment
   #:define-webapp
   #:define-middleware
   #:middleware-chain
   #:call-endware
   #:blank-key-p
   #:init
   #:main))
(in-package #:webhax-route)

(defmacro quick-page ((&rest parts-and-templates) &body body)
  `(webhax-core:input-function-wrapper
    (lambda ()
      (webhax-metaplate:display-page
       webhax-metaplate:*metaplate-default-layout*
       webhax-metaplate:*metaplate-default-parts*
       ,@parts-and-templates
       ,@(when body `(:@inner
                      (lambda ()
                        ,@body)))))))

(defparameter *url-parentage* nil)
(defparameter *url-parentage-lock-level* 0
  "To keep call-endware from unwinding the parentage more than was intended")

(defun blank-key-p (item)
  "Determine if an item denotes an empty (root) url path."
  (cond
    ((stringp item) (not (boolify (length item))))
    ((keywordp item) (member item '(:empty :blank :/)))
    ((symbolp item) (eq item '/))
    (t nil)))

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

(defun %%divide-body (code)
  "See if code has (init ... (main ...)) structure. If so, split them out. Otherwise put everything in the (main ...) position."
  (if (and (listp code) (listp (car code)) (eq 'init (caar code)))
      (preserve-other-values
       (divide-tree (lambda (x) (and (listp x) (eq 'main (car x))))
                    (cdar code))
       (lambda (x) (or x (error "Web component body has init without main"))))
      (values
       #'list
       (list* 'filler code))))

(defmacro define-webapp (name parameters &body body)
  (multiple-value-bind (outer ibody) (%%divide-body body)
    (let ((name-int (symb name '-internal)))
      `(progn
         (defun ,name-int ,parameters
           ,@(cdr ibody))
         (defun ,name (&rest params)
           ,@(funcall outer
                     `(wrap-with-webhax-environment #',name-int params)))))))

(defmacro define-middleware (name parameters &body body)
  (multiple-value-bind (outer ibody) (%%divide-body body)
    (let ((name-int (symb name '-internal)))
      `(let ((%app nil))
         ;;FIXME: Would be nice to use parameters here so that user options
         ;;show up in the hints.
         (defun ,name-int ,parameters
           (let ((*clack-app* %app))
             ,@(cdr ibody)))
         (defun ,name (&rest params)
           (lambda (app)
             (setf %app app)
             ,@(funcall
               outer `(wrap-with-webhax-environment #',name-int params))))))))

(defun middleware-chain (&rest mwarez)
  "Join a chain of middlewares into a single middleware"
  (lambda (app)
    (reduce #'funcall mwarez :initial-value app :from-end t)))

;;FIXME: The machination below should happen in wrap/call-with-whx-env
;; so that users can mindlessly call apps and not arbitrarily need
;; call-endware
(defun call-endware (&key (app *clack-app*)
                       (env *web-env*)
                       (index 0))
  "Call the next app in the clack chain as endware, that is, without any
trimming done to the parent portion of the URL."
  ;;Undo any changes to web input back to last call-subware
  (let ((%index (- *url-parentage-lock-level* index)))
    (let ((*url-parentage*
           (last *url-parentage* %index))
          (*regular-web-input*
           (concatenate 'list
                        (butlast *url-parentage* %index)
                        *regular-web-input*)))
      (funcall app env))))
