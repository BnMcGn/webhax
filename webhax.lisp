;;;; webhax.lisp

(in-package #:webhax)

;;; "webhax" goes here. Hacks and glory await!


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

(defpsmacro collecting-set (&body body)
  (let ((res (gensym)))
    `(let ((,res (create)))
       (labels ((collect (itm)
                  (setf (getprop ,res itm) itm)))
         ,@body)
       (collecting (dolist (x (chain -object (keys ,res)))
                     (collect (getprop ,res x)))))))

(defpsmacro do-window ((var/s source
                              &key (size 2) (step 1)
                              (start-padding '(array))) &body body)
  (let ((size (if (listp var/s) (length var/s) size))
        (data (gensym))
        (i (gensym)))
    `(let ((,data (chain ,start-padding (concat ,source))))
       (dolist (,i (range 0 (1+ (- (length ,data) ,size)) ,step))
         ,(if (listp var/s)
              `(destructuring-bind ,var/s (chain ,data (slice ,i (+ ,i ,size)))
                 ,@body)
              `(let ((,var/s (chain ,data (slice ,i (+ ,i ,size)))))
                 ,@body))))))

(defpsmacro strcat (first &rest rest)
  `(chain ,first (concat ,@rest)))

(defparameter *js-second* 1000)
(defparameter *js-minute* (* 60 *js-second*))
(defparameter *js-hour* (* 60 *js-minute*))
(defparameter *js-day* (* 24 *js-hour*))
(defparameter *js-week* (* 7 *js-day*))
(defparameter *js-month* (* 30 *js-day*)) ;Ok, things start to get wierd.
(defparameter *js-year* (* 365 *js-day*))

(defun ps-gadgets ()
  (strcat
   (compile-script *ps-lisp-library*)
   (ps
     (defun say (thing)
       (chain console (log thing))
       thing)

     (defun ensure-array (arr)
       (cond
         ((or (equal (typeof arr) "undefined")
               (null arr))
          ([]))
         ((chain arr (has-own-property 'length))
          arr)
         (t ([] arr))))

     (defun remove-if-not (test arr)
       (collecting
         (dolist (itm (ensure-array arr))
           (when (funcall test itm)
             (collect itm)))))

     (defun range (start &optional stop (step 1))
       (let ((start (if stop start 0))
             (stop (if stop stop start)))
         (let ((stop-p (if (> step 0) (lambda (x) (< x stop)) (lambda (x) (> x stop)))))
           (if (or (and (> step 0) (>= start stop) (and (< step 0) (<= start stop))))
             ([])
             (collecting
               (while (funcall stop-p start)
                 (collect start)
                 (incf start step)))))))

     (defun position-difference (element1 element2)
       (let ((pos1 (chain element1 (get-bounding-client-rect)))
             (pos2 (chain element2 (get-bounding-client-rect))))
         (create top (- (@ pos1 top) (@ pos2 top))
                 left (- (@ pos1 left) (@ pos2 left)))))

     (defun mapleaves (fn tree)
       "Map a one-argument function FN over each leaf node of the TREE
   in depth-first order, returning a new tree with the same structure."
       (labels ((rec (node)
                  (if (atom node)
                      (funcall fn node)
                      (mapcar #'rec node))))
         (when tree
           (rec tree))))

     (defun ago (date-obj)
       (let ((diff (- (chain -date (now)) date-obj)))
         (create
          get-years (lambda () (parse-int (/ diff (lisp *js-year*))))
          get-months (lambda () (parse-int (/ diff (lisp *js-month*))))
          get-weeks (lambda () (parse-int (/ diff (lisp *js-week*))))
          get-days (lambda () (parse-int (/ diff (lisp *js-day*))))
          get-hours (lambda () (parse-int (/ diff (lisp *js-hour*))))
          get-minutes (lambda () (parse-int (/ diff (lisp *js-minute*))))
          get-seconds (lambda () (parse-int (/ diff (lisp *js-second*)))))))

         ))); End ps-gadgets



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
  (setf (clack.response:headers ningle:*response* :content-type) ctype))

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

(ps:defpsmacro define-react-class (name render &rest other)
  "A convenience wrapper macro for create-class. The created class will be
assigned to the name specified by the first variable. The second value is
code to be placed in the render method. It will be automatically wrapped in
an anonymous function. The remainder of the parameters are key/value pairs
That will become attributes of the object.

If name is set to nil, the macro will return the class without attempting to
assign it to a variable.

If render is set to nil, the macro will not fill the render attribute. It can
then be manually filled in the rest section."
  (let ((classcode
          `(react:create-class
            (ps:create ,@(when render `(:render (lambda () ,render)))
                    ,@other))))
    (if name
        `(ps:var ,name ,classcode)
        classcode)))
