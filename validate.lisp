(in-package :cl-user)

(defpackage #:webhax-validate
  (:use #:cl #:gadgets #:ratify)
  (:export
   #:mkparse-in-list
   #:mkparse-all-members
   #:compile-validator
   #:ratify-wrapper ;;FIXME: un-export ratify-wrapper
   #:nullok?
   #:recommend-widget
   #:options-list
   #:normalize-fieldspec-body
   #:prep-fieldspec-body-for-json
   #:convert-fieldspecs-to-json
   #:multiple?))

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

;;;FIXME: How to handle start,end? Are they always defined?
(define-test overlength (item start end)
  (if (< (length item) *webhax-input-limit*)
      item
      (ratification-error
       item (format nil "Field is longer than system limit of ~a chars."
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

(defun nullok-test (subtest)
  (lambda (item)
    (if (and item (< 0 (length item)))
        (funcall subtest item)
        (values nil t))))

(let ((keymap '((:yesno . :boolean))))
  (defun %handle-keyword (valkey)
    (anaphora:aif (assoc valkey keymap)
                  (ratify-wrapper (cddr anaphora:it))
                  (ratify-wrapper valkey))))

;;;FIXME: Badly needs tidying
;;;FIXME: how shall overlength work?
;;;FIXME: function/list specs have design issue: notnull in parameters

(defun compile-validator (valspec)
  (cond
    ((functionp valspec)
     valspec)
    ((member valspec *ratify-tests*)
     (ratify-wrapper valspec))
    ((eq valspec :yesno)
     (ratify-wrapper :boolean))
    ((eq valspec :overlength)
     (ratify-wrapper :overlength))
    ((and (listp valspec) (symbolp (car valspec)))
     (case (car valspec)
       (:pickone
        (mkparse-in-list (cdr valspec)))
       (:picksome
        (mkparse-all-members (mkparse-in-list (cdr valspec))))
       (otherwise
        (%handle-keyword (car valspec)))))
    ((and (listp valspec) (functionp (car valspec)))
     (apply (car valspec) (cdr valspec)))
    (t (error "Validator type not found"))))

(defparameter *ratify-tests*
  '(:bit :day :date :hour :real :time :year :float :month :ratio :minute :number :offset :second :string :boolean :complex :integer :datetime :rational :character :unsigned-integer :ip :tel :uri :url :file :host :ipv4 :ipv6 :name :port :text :user :week :color :email :query :radio :range :domain :failed :object :scheme :search :numeric :checkbox :fragment :hostname :password :property :protocol :textarea :authority :alphabetic :alphanumeric :absolute-path :rootless-path :datetime-local :hierarchical-part))

(defun nullok? (valspec)
  "Null is ok unless explicitly set otherwise."
  (not (and (listp valspec) (member :notnull valspec))))

(defun multiple? (widget)
  (member widget '(:picksome :picksome-long)))

;;;FIXME: Mostly just a placeholder for now. Will fill out with time.
(defun recommend-widget (valspec)
  (let ((valsym (or (and (listp valspec) (car valspec)) valspec)))
    (if (member valsym '(:integer :string :boolean :pickone :picksome :yesno))
        valsym
        :string)))

(defun options-list (valspec)
  (and (listp valspec)
       (mapcar
        (lambda (option)
          (cond
            ((and (consp option) (not (consp (cdr option))))
             (list (car option) (cdr option)))
            ((listp option)
             (if (eq 2 (length option))
                 option
                 (error "Option must be a list of 2 elements")))
            ;;For some reason this seemed to be a bad idea earlier???
            ;;See what happens:
            ((or (symbolp option) (stringp option) (numberp option))
             (list option option))
            (t (error "Not a valid option"))))
        (gadgets:fetch-keyword :options valspec))))

(defun normalize-fieldspec-body (fieldspec)
  (if (stringp (car fieldspec))
      (list* :description (car fieldspec)
             (cddr (normalize-fieldspec-body (cdr fieldspec))))
      (let* ((vspec (car fieldspec))
             (fspec (cdr fieldspec))
             (widget (getf fspec :widget
                           (recommend-widget vspec)))
             (nullok (nullok? vspec)))
        ;;Doesn't handle name
        (list
         :description (getf fspec :description "")
         :initial (getf fspec :initial)
         :compiled-validator (if nullok
                                 (nullok-test (compile-validator vspec))
                                 (compile-validator vspec))
         :widget widget
         :multiple (multiple? widget)
         :nullok nullok
         :options (options-list vspec)
         :type vspec
         :config (getf fspec :config)
         :documentation (getf fspec :documentation "")))))

(defun prep-fieldspec-body-for-json (fspec)
  ;;Doesn't handle name
  (cl-hash-util:plist->alist
   (nth-value
    1 (gadgets:extract-keywords '(:compiled-validator) fspec))))

(defun convert-fieldspecs-to-json (fspecs)
  (json:encode-json-to-string
   (cl-hash-util:collecting-hash-table (:mode :replace)
     (gadgets:map-by-2
      (lambda (k v)
        (cl-hash-util:collect k
          (prep-fieldspec-body-for-json v)))
      fspecs))))


