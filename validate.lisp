(in-package :cl-user)

(defpackage #:webhax-validate
  (:use #:cl #:gadgets #:ratify)
  (:export
   #:mkparse-in-list
   #:mkparse-all-members
   #:compile-validator))

(in-package :webhax-validate)


(defun ratify-wrapper (basename)
  (let ((*package* (find-package 'webhax-validate)))
    (let* ((p-name (mkstr 'parse- basename))
           (test (or ;Not all ratify tests have a parse- form
                  (and (find-symbol p-name)
                       (fboundp (symbolize p-name))
                       (symbol-function
                        (symbolize p-name)))
                  (symbol-function
                   (symbolize (mkstr 'test- basename))))))
      (lambda (x)
        (handler-case
            (values (funcall test x) t)
          (t (e) (values (message e) nil)))))))

(defmethod message ((condition condition))
  (princ-to-string condition))

(defvar *webhax-input-limit* 200)

(define-test overlength (item)
  (if (< (length item) *webhax-input-limit*)
      item
      (ratification-error item
                          (format nil "Field is longer than system limit of ~a chars."
                                  *webhax-input-limit*))))

(defun mkparse-in-list (items)
  (lambda (item)
    (aif2only (match-a-symbol item items)
              (values it t)
              (values "Value not in list of options" nil))))

(defun mkparse-all-members (subtest)
  (lambda (itemlist)
    (block exit
      (values
       (collecting
         (dolist (itm itemlist)
           (aif2only (funcall subtest itm)
                     (collect it)
                     (return-from exit (values it nil)))))
       t))))

(defun compile-validator (valspec)
  (cond
    ((functionp valspec)
     valspec)
    ((member valspec *ratify-tests*)
     (ratify-wrapper valspec))
    ((and (listp valspec) (symbolp (car valspec)))
     (case (car valspec)
       (:pickone
        (mkparse-in-list (cdr valspec)))
       (:picksome
        (mkparse-all-members (mkparse-in-list (cdr valspec))))))
    ((and (listp valspec) (functionp (car valspec)))
     (apply (car valspec) (cdr valspec)))))

(defparameter *ratify-tests*
  '(:bit :day :date :hour :real :time :year :float :month :ratio :minute :number :offset :second :string :boolean :complex :integer :datetime :rational :character :unsigned-integer :ip :tel :uri :url :file :host :ipv4 :ipv6 :name :port :text :user :week :color :email :query :radio :range :domain :failed :object :scheme :search :numeric :checkbox :fragment :hostname :password :property :protocol :textarea :authority :alphabetic :alphanumeric :absolute-path :rootless-path :datetime-local :hierarchical-part))
