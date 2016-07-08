;;;; bind-validated-input.lisp

(in-package #:webhax)


(defun %%make-regular-params-fetcher (bindspecs)
  (let ((min-vals 0)
        (max-vals 0)
        ;(found-optional nil) ;FIXME:Don't seem to be using: will ever need?
        (found-rest nil)
        (vlength (length bindspecs)))
    (dolist (itm bindspecs)
      (when found-rest (error "No regular parameters allowed after :rest"))
      (if (fetch-keyword :optional itm)
          (progn
            (when (fetch-keyword :rest itm)
              (error ":rest and :optional not allowed in same spec."))
            (incf max-vals))
            ;;(setf found-optional t))
          (if (fetch-keyword :rest itm)
              (setf found-rest t)
              (progn (incf min-vals) (incf max-vals)))))
    `(lambda (input)
       (let ((l-in (length input)))
         (when (< l-in ,min-vals)
           (error
            (format nil "~a parameters found, ~a required" l-in ,min-vals)))
         (when (and (not ,found-rest) (> l-in ,max-vals))
           (error
            (format nil "Too many parameters: ~a found, ~a specified."
                    l-in ,max-vals)))
         (if (< l-in ,vlength)
             (values
              (concatenate 'list input (make-list (- ,vlength l-in)))
              (concatenate 'list (make-list l-in :initial-element t)
                           (make-list (- ,vlength l-in))))
             (values
              (if ,found-rest
                  (concatenate 'list (subseq input 0 (1- ,vlength))
                               (list (nthcdr (1- ,vlength) input)))
                  input)
              (make-list ,vlength :initial-element t)))))))

(defun %spec-name (bindspec)
  (car (ensure-list (car bindspec))))

(defun %%make-key-param-fetcher (keyspec input)
  (bind-extracted-keywords (keyspec other :multiple :required)
    (with-gensyms (value)
      `(let ((,value ,(if multiple
                          `(assoc-all ',(%spec-name other)
                                      ,input :test #'eq-symb-multiple)
                          `(assoc ',(%spec-name other)
                                  ,input :test #'eq-symb))))
         ,@(when required
             `((unless ,value
                 (error
                  (format nil
                          "No value found for required keyword parameter ~a"
                          ',(%spec-name other))))))
         (if ,value
             (values ,(if multiple value `(cdr ,value)) t)
             (values nil nil))))))

(defun %%default-decider (bindspec inputform foundvar)
  (let ((filledp? (and (listp (car bindspec))
                       (third (car bindspec))
                       (symbolp (third (car bindspec)))))
        (multiple (or (fetch-keyword :multiple bindspec)
                      (fetch-keyword :rest bindspec))))
    (with-gensyms (item found vitem valid)
      (collecting
        (collect
            (list (%spec-name bindspec)
                  `(multiple-value-bind (,item ,found) ,inputform
                     ,@(when filledp?
                         `((setf ,foundvar ,found)))
                     (if ,found
                         (multiple-value-bind
                               (,vitem ,valid)
                             ,(if multiple
                                  `(funcall
                                    (mkparse-all-members ,(second bindspec))
                                    ,item)
                                  `(funcall-in-macro
                                    ,(second bindspec) ,item))
                           (if ,valid
                               ,vitem
                               (error
                                (format nil "~a: ~a"
                                        ,(mkstr
                                          (%spec-name bindspec)) ,vitem))))
                         ,(if (listp (car bindspec))
                              (second (car bindspec))
                              nil)))))
        (when filledp?
          (collect
              (list (third (car bindspec)) foundvar)))))))

;bindspec:
;(name -or- (name default filled-p)
;   validator
;  &keys key (required multiple) -or- &keys (key nil) (rest optional))


(defmacro bind-validated-input ((&rest bindspecs) &body body)
  (multiple-value-bind (keys regular)
      (splitfilter bindspecs
                   (lambda (x)
                     (fetch-keyword :key x)))
    (with-gensyms (foundp regvals regfill reg-input key-input)
      `(let ((,reg-input *regular-web-input*)
             (,key-input *key-web-input*))
         (declare (ignorable ,reg-input ,key-input))
         (multiple-value-bind (,regvals ,regfill)
             (funcall ,(%%make-regular-params-fetcher regular) ,reg-input)
           (declare (ignorable ,regvals ,regfill))
           (let ((,foundp nil))
             (declare (ignorable ,foundp))
             (let ,(apply #'concatenate
                          'list
                          (loop for i from 0
                                for regspec in regular
                                append
                                (%%default-decider regspec `(values
                                                             (elt ,regvals ,i)
                                                             (elt ,regfill ,i))
                                                   foundp))
                          (collecting
                            (dolist (kspec keys)
                              (collect
                                  (%%default-decider
                                   kspec
                                   (%%make-key-param-fetcher kspec key-input)
                                   foundp)))))
               ,@body)))))))
