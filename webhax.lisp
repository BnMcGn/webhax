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

(defparameter *set-content-type* nil)
(defvar *regular-web-input*)
(defvar *key-web-input*)

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
  (setf (clack.response:headers *response* :content-type) ctype))

(defun input-function-wrapper (handler &key content-type)
  (lambda (input)
    (when content-type
      (set-content-type content-type))
    (multiple-value-bind (*regular-web-input* *key-web-input*)
        (input-normalize input)
      (bind-webspecials (nth-value 1 (input-normalize input))
        (with-output-to-string (*webhax-output*)
          (funcall handler))))))

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
