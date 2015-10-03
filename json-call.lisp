
(in-package #:webhax)

;;;Json-call: a quick-n-dirty way to call any lisp function from a web client.

(defvar *json-call-symbols* nil)
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

(defun string-unless-symbol-unless-number (in-string symbol-coll)
  (let ((value (string-unless-number in-string)))
    (typecase value
      (number value)
      (string
       (dolist (symlist symbol-coll value)
         (dolist (sym symlist)
           (when (eq-symb sym value)
             (return sym))))))))

(defun multiple-key-p (stritem)
  (ends-with-subseq "[]" stritem))

(defun match-keyword (item symbol-coll)
  (dolist (symlist symbol-coll)
    (dolist (sym symlist)
      (when (and (keywordp sym) (eq-symb-multiple sym item))
        (return sym)))))

(defun prep-keywords-ignorant (params symbol-coll)
  (hash->plist
   (collecting-hash-table (:mode :replace)
     (dolist (p params)
       (destructuring-bind (k . v)
           p
         (let ((key (match-keyword k symbol-coll)))
           (if (multiple-key-p k)
               (collect key
                 (string-unless-symbol-unless-number v symbol-coll)
                 :mode :append)
               (collect key
                 (string-unless-symbol-unless-number v symbol-coll)))))))))

;;;FIXME: Implement non-ignorant: call recording and type guessing, etc.
(defun prep-call-ignorant (params keys
                  &key (symbols *json-call-symbols*)
                    (callables *json-call-callables*))
  (let ((function (first-match callables
                               (curry #'eq-symb (car params))))
        (reg-params
          (collecting
            (dolist (param (cdr params))
              (collect (string-unless-symbol-unless-number param symbols)))))
        (key-params
          (prep-keywords-ignorant keys symbols)))
    (values function (list* reg-params key-params))))

(defclass json-call (clack-tool)
  ((base-url :initform "/json/")))

(defmethod execute ((this json-call))
  (multiple-value-bind (func params)
      (prep-call-ignorant *regular-web-input* *key-web-input*)
    (apply (symbol-function func) params)))

;;;FIXME: is the concept of call-to-link useful here? Some way to express
;;;a function call as a link or as a javascript function prototype.

