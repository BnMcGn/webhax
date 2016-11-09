(in-package :cl-user)

(defpackage #:webhax-validate
  (:use #:cl #:gadgets #:ratify)
  (:import-from #:alexandria
                #:compose)
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
   #:multiple?
   #:validate-batch))

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

;;; Existing value: some tests will need to know the previous value of a field,
;;; perhaps to see if it is being changed. The code that calls the validator
;;; needs to support this feature, if desired, by calling the validator with
;;; *existing-value* set to the established value and
;;; *existing-value-available-p* set to t.
;;;
;;; Validators that use the feature should be written with the fact in mind
;;; that it is optional and may not be present in the environment. Check
;;; *existing-value-available-p*, in other words.

(defvar *existing-value* nil)
(defvar *existing-value-available-p* nil)

;;;FIXME: How to handle start,end? Are they always defined?
(define-test overlength (item start end)
  (if (< (length item) *webhax-input-limit*)
      item
      (ratification-error
       item (format nil "Field is longer than system limit of ~a chars."
                                  *webhax-input-limit*))))

(defun mkparse-in-list (items)
  (let ((matcher (match-various (mapcar #'car items))))
    (lambda (item)
      (aif2only (funcall matcher item)
                (values it t)
                (values "Value not in list of options" nil)))))

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

(defun notnull-test (subtest)
  (lambda (item)
    (unless (stringp item)
      (error (make-condition 'type-error :datum item :expected-type 'string)))
    (if (> 1 (length item))
        (values "Field must not be empty" nil)
        (funcall subtest item))))

(defun string-check (str)
  (if (stringp str)
      str
      (error (make-condition 'type-error :datum str :expected-type 'string))))

(let ((keymap '((:yesno . :boolean))))
  (defun %handle-keyword (valkey)
    (anaphora:aif (assoc valkey keymap)
                  (ratify-wrapper (cdr anaphora:it))
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
        (mkparse-in-list (options-list valspec)))
       (:picksome
        (mkparse-all-members (mkparse-in-list (options-list valspec))))
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
  "Shall return list of options that consist of a two element list: (value label)"
  (and (listp valspec)
       (mapcar
        (lambda (option)
          (cond
            ((and (consp option) (not (consp (cdr option))))
             (list (car option) (cdr option)))
            ((listp option)
             (when (eq (car option) 'quote)
               (error "Options-list shouldn't receive quoted lists"))
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
                                 (notnull-test (compile-validator vspec)))
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

(defun normalize-input (input fieldspecs &optional translation-table)
  (let ((trans
         (if translation-table
             (lambda (key)
               (gethash key translation-table))
             #'identity)))
    (collecting
      (gadgets:do-window ((k v) fieldspecs :step 2)
        (let* ((out-key (funcall trans k))
               (val (if (getf v :multiple)
                        (cons k (gadgets:assoc-all
                                 out-key input
                                 :test #'webhax-core:eq-symb-multiple))
                        (assoc out-key input :test #'equal))))
          (when val
            (collect (cons k (cdr val)))))))))

(defparameter *incoming-values* nil)

(defun validate-batch (input-alist fieldspecs-plist
                       &key existing-hash edit translation-table keylist)
  "Translation-table has the internal key as the key and the input key as the
 value"
  (when (and edit (not existing-hash))
    (error "Edit is t, but no existing store supplied"))
  (let ((keylist (or keylist
                     (gadgets:map-by-2
                      (lambda (&rest x) (car x)) fieldspecs-plist)))
        (results (make-hash-table))
        (errors (make-hash-table))
        (input
         (normalize-input input-alist fieldspecs-plist translation-table)))
    (dolist (key keylist)
      (when (assoc key input)
        (let ((*existing-value-available-p*
               (and existing-hash (gadgets:key-in-hash? key existing-hash) t))
              (*existing-value* (and existing-hash (gethash key existing-hash)))
              ;;FIXME: incoming-values won't be validated! Don't like!
              (*incoming-values* input))
          (multiple-value-bind (val sig)
              (funcall
               (getf (getf fieldspecs-plist key) :compiled-validator)
               (gadgets:assoc-cdr key input))
            (if sig
                (setf (gethash key results) val)
                (setf (gethash (if translation-table
                                   (gethash key translation-table)
                                   key)
                               errors)
                      val))))))
    (if (not-empty (alexandria:hash-table-keys errors))
        (values (if translation-table
                    (rekey errors (invert-hash-table translation-table))
                    errors)
                nil)
        (values (if edit
                    (collecting-hash-table (:existing existing-hash :mode :replace)
                      (do-hash-table (k v results)
                        (collect k v)))
                    results)
                t))))
