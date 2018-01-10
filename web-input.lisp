(in-package :webhax)

(defun extract-webspecials-from-parameters (params)
  (with-collectors (norm< spec<)
    (dolist (p params)
      (if
       (ppcre:scan "^~(.*)~" (car p))
       (spec< p)
       (norm< p)))))

(defparameter *webspecial-validators* (make-hash-table))

(defun bound-webspecials ()
  "Creates a list of symbols to be automatically bound as specials by bind-webspecials."
  (hash-table-keys *webspecial-validators*))

(defun default-validator ()
  (??length-within 200))

(defmacro def-webspecial (sym &optional default (validator #'default-validator))
  `(progn
     (setf (gethash ',sym *webspecial-validators*) ,validator)
     (defparameter ,sym ,default)))

(defmacro bind-webspecials (input &body body)
  `(let
       ,(collecting
         (dolist (var (bound-webspecials))
           (collect `(,var (aif
                            (assoc ,(symbol-name var) ,input
                                   :test #'string-equal)
                            (fail-if-not-valid
                             (gethash ',var *webspecial-validators*)
                             (cdr it))
                            ,var)))))
     ,@body))

;;;;;;;;;;;;
; Validation functions:
; 1st value: converted (or original) input value
; 2nd value: boolean to indicate acceptable input.
;

;FIXME: Use validate.lisp instead of these.


(defun >>integer (&key (emsg "Not an integer"))
  (lambda (data)
    (handler-case
	(values (parse-integer data) t)
      (parse-error () (values data nil emsg)))))

(defun ??length-within (max &key (emsg "Field too long"))
  (lambda (data)
    (if (<= (length data) max)
	(values data t)
	(values data nil emsg))))

(defun fail-if-not-valid (test &rest parameters)
  (multiple-valplex (apply test parameters)
    (if v1
	v0
	(error v2))))
