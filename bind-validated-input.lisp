;;;; bind-validated-input.lisp

(in-package #:webhax)

(defun %%reg-params-splitter (bindspecs)
  (cl-utilities:with-collectors (reg< opt< rest<)
    (let ((collect #'reg<))
      (dolist (b bindspecs)
        (case b
          (&rest (setf collect #'rest<))
          (&optional (setf collect #'opt<))
          (otherwise
           (funcall collect b)))))))

(defun %%make-regular-params-fetcher (bindspecs)
  (multiple-value-bind (regular optional rest)
      (%%reg-params-splitter bindspecs)
    (when (and optional rest)
      (error "&rest and &optional not allowed in same spec"))
    (when (< 1 (length rest))
      (error "Only one &rest parameter allowed"))
    (let ((min-vals (length regular))
          (max-vals (+ (length regular)
                       (max (length optional) (length rest)))))
      `(lambda (input)
         (let ((l-in (length input)))
          (when (< l-in ,(length regular))
            ;;(error
            ;; (format nil "~a web parameters found, ~a required"
            ;;         l-in ,min-vals))
            (web-fail-404))
          ,@(unless rest
                    `((when (> l-in ,max-vals)
                        ;;(error
                        ;; (format
                        ;;  nil
                        ;;  "Too many web parameters: ~a found, ~a specified."
                        ;;  l-in ,max-vals))
                        (web-fail-404))))
          (if (< l-in ,max-vals)
              (values
               (concatenate 'list input (make-list (- ,max-vals l-in)))
               (concatenate 'list (make-list l-in :initial-element t)
                            (make-list (- ,max-vals l-in))))
              (values
               (if (zerop ,(length rest))
                   input
                   (concatenate 'list (subseq input 0 (1- ,max-vals))
                                (list (nthcdr (1- ,max-vals) input))))
               (make-list ,max-vals :initial-element t))))))))

(defun %spec-name (bindspec)
  (car (ensure-list (car bindspec))))

(defun %%make-key-param-fetcher (keyspec input)
  (bind-extracted-keywords (keyspec other :multiple :required)
    (with-gensyms (value)
      `(let ((,value ,(if multiple
                          `(assoc-all ',(%spec-name other)
                                      ,input
                                      :test #'gadgets:string-equal-multiple)
                          `(assoc ',(%spec-name other)
                                  ,input :test #'string-equal))))
         ,@(when required
             `((unless ,value
                 (error
                  ;;(format nil
                  ;;        "No value found for required keyword parameter ~a"
                  ;;        ',(%spec-name other))
                  (web-fail-404)))))
         (if ,value
             (values ,(if multiple value `(cdr ,value)) t)
             (values nil nil))))))

(defvar *rest-toggled* nil)
(defun %%map-regspecs (func regspecs)
  (labels ((proc (regspecs count)
             (cond
               ((null regspecs) nil)
               ((eq '&optional (car regspecs))
                (proc (cdr regspecs) count))
               ((eq '&rest (car regspecs))
                (let ((*rest-toggled* t))
                  (proc (cdr regspecs) count)))
               (t
                (cons (funcall func (car regspecs) count)
                      (proc (cdr regspecs) (1+ count)))))))
    (apply #'concatenate 'list (proc regspecs 0))))

(defun %%prep-valspec (vspec)
  `(compile-validator
    ,(if (and (consp vspec) (eq 'lambda (car vspec)))
         vspec
         `(list ,@(if (keywordp vspec)
                      (list vspec)
                      vspec)))))

(defun %%default-decider (bindspec inputform foundvar)
  (let ((filledp? (and (listp (car bindspec))
                       (third (car bindspec))
                       (symbolp (third (car bindspec)))))
        (multiple (or (fetch-keyword :multiple bindspec) *rest-toggled*)))
    (with-gensyms (item found vitem valid)
      (cl-utilities:collecting
        (cl-utilities:collect
            (list (%spec-name bindspec)
                  `(multiple-value-bind (,item ,found) ,inputform
                     ,@(when filledp?
                         `((setf ,foundvar ,found)))
                     (if ,found
                         (multiple-value-bind
                               (,vitem ,valid)
                             ,(if multiple
                                  `(funcall
                                    (mkparse-all-members
                                     ,(%%prep-valspec (second bindspec)))
                                    ,item)
                                  `(funcall
                                    ,(%%prep-valspec (second bindspec))
                                    ,item))
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
          (cl-utilities:collect
              (list (third (car bindspec)) foundvar)))))))

;bindspec:
;(name -or- (name default filled-p)
;   validator
;  &key key (required multiple) -or- &key (key nil) (rest optional))


(defmacro bind-validated-input ((&rest bindspecs) &body body)
  (multiple-value-bind (regular keys)
      (part-on-true (curry #'eq '&key) bindspecs)
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
                          (%%map-regspecs
                           (lambda (regspec i)
                             (%%default-decider regspec `(values
                                                          (elt ,regvals ,i)
                                                          (elt ,regfill ,i))
                                                foundp))
                           regular)
                          (cl-utilities:collecting
                              (dolist (kspec (cdr keys))
                                (cl-utilities:collect
                                    (%%default-decider
                                     kspec
                                     (%%make-key-param-fetcher kspec key-input)
                                     foundp)))))
               ,@body)))))))
