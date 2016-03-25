;;;; webhax.lisp

(in-package #:webhax)

;;; "webhax" goes here. Hacks and glory await!

(defparameter *set-content-type* nil)
(defparameter *session* ningle:*session*)

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
  (error "Needs reimplementation"))
 
(defun input-function-wrapper (handler &key content-type)
  (lambda (input)
    (when content-type
      (set-content-type content-type))
    (multiple-value-bind (*regular-web-input* *key-web-input*)
        (input-normalize input)
      (bind-webspecials (nth-value 1 (input-normalize input))
        (with-output-to-string (*webhax-output*)
          (funcall handler))))))

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

;;;;;;;;


(defmacro clack-server-manager (handler-var app &rest clackup-params)
  `(progn
     (if (boundp ',handler-var)
         (clack:stop ,handler-var)
         (defvar ',handler-var nil))
     (setf ,handler-var
           (clack:clackup
            ,app
            ,@clackup-params))))
