
(in-package #:webhax-json-call)

;;;Json-call: a quick-n-dirty way to call any lisp function from a web client.

(defvar *json-call-symbols* nil)
(defvar *json-call-symbol-funcs* nil)
(defvar *json-call-callables* nil)

;;;FIXME: Should be able to specify custom validators for individual parameters.
(defun register-json-call (func-symbol)
  (declare (type symbol func-symbol))
  (unless (fboundp func-symbol)
    (error "Json-call: can't find function for symbol."))
  (push func-symbol *json-call-callables*))

(defun register-json-symbols (symbol-list)
  (declare (type list symbol-list))
  (push symbol-list *json-call-symbols*))

(defun register-json-symbol-func (func)
  (declare (type function func))
  (push func *json-call-symbol-funcs*))

(defun json-symbols ()
  (let ((slist *json-call-symbols*))
    (dolist (func *json-call-symbol-funcs*)
      (awhen (funcall func)
             (push it slist)))
    slist))

(defun string-unless-symbol-unless-number (in-string symbol-coll)
  (let ((value (string-unless-number in-string)))
    (typecase value
      (number value)
      (string
       (dolist (symlist symbol-coll value)
         (dolist (sym symlist)
           (when (eq-symb sym value)
             (return-from string-unless-symbol-unless-number sym))))))))

(defun match-keyword (item symbol-coll)
  (dolist (symlist symbol-coll)
    (dolist (sym symlist)
      (when (and (keywordp sym) (eq-symb-multiple sym item))
        (return-from match-keyword sym)))))

(defun prep-keywords-ignorant (params symbol-coll)
  (hash->plist
   (collecting-hash-table (:mode :replace)
     (dolist (p params)
       (destructuring-bind (k . v)
           p
         (let ((key (match-keyword k symbol-coll)))
           (unless key
             (error "Attempted use of unregistered keyword"))
           (if (multiple-key-p k)
               (collect key
                 (string-unless-symbol-unless-number v symbol-coll)
                 :mode :append)
               (collect key
                 (string-unless-symbol-unless-number v symbol-coll)))))))))

;;;FIXME: Implement non-ignorant: call recording and type guessing, etc.
(defun prep-call-ignorant (params keys
                  &key (symbols (json-symbols))
                    (callables *json-call-callables*))
  (let ((function (first-match (curry #'eq-symb (car params))
                               callables))
        (reg-params
          (collecting
            (dolist (param (cdr params))
              (collect (string-unless-symbol-unless-number param symbols)))))
        (key-params
          (prep-keywords-ignorant keys symbols)))
    (values function (concatenate 'list reg-params key-params))))

;;;FIXME: No support for authorization checking. Might want.
(webhax:define-middleware json-call-component ()
  (webhax:url-case
    (:json-call
     (multiple-value-bind (func params)
         (prep-call-ignorant *regular-web-input* *key-web-input*)
       (cl-json:encode-json-to-string
        (apply (symbol-function func) params))))
    (otherwise (webhax:call-endware))))

(defclass json-call (clack-tool)
  ((webhax-core::base-url :initform "/json/")))

(defmethod execute ((this json-call))
  (multiple-value-bind (func params)
      (prep-call-ignorant *regular-web-input* *key-web-input*)
    (cl-json:encode-json-to-string
     (apply (symbol-function func) params))))

;;;FIXME: is the concept of call-to-link useful here? Some way to express
;;;a function call as a link or as a javascript function prototype.
;;- problem: json doesn't work as function in javascript: needs callback.
;;- react connection component might work.
