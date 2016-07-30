(in-package #:webhax-test)

(in-suite webhax-test)

(defparameter result nil)

(multiple-value-bind (askman names)
    (webhax::t-ask
      (q some "Are there any?" :yesno)
      (if (a some)
          (q enough? "How many?" (:pickone :options '(3 5 6 18)))
          (q want "Why not?" :string))
      (and (q are :yesno)
           (q you :yesno)
           (q sure? :yesno))
      :target (lambda (answ)
                (maphash (lambda (k v)
                           (setf (gethash k answ) (car v))) answ)
                (setf result answ)))
  (funcall askman :update nil)
  (funcall askman :update `((,(car names) . "true")))
  (funcall askman :update `((,(second names) . "5")))
  (funcall askman :update `((,(third names) . "true")))
  (funcall askman :update `((,(fourth names) . "false"))))

(test t-ask
  (is (hash-table-p result))
  (is (= 5 (gethash 'enough? result))))

;;;FIXME: Has a problematic amount of internal knowledge embedded.
(defmacro ask-test-fixture (form &body body)
  (with-gensyms (formname aman store)
    `(let* ((webhax::*session* (make-hash-table))
            (form-html (with-output-to-string (webhax:*webhax-output*) ,form))
            (,formname (car (hash-table-keys
                             (gethash :askdata webhax::*session*))))
            (,aman (gethash ,formname (gethash :askdata webhax::*session*)))
            (,store (funcall ,aman :get-store nil)))
       (declare (ignorable form-html))
       (labels ((send (command data)
                  (call-ask-manager ,formname command
                                    (mapcar
                                     (lambda (x)
                                       (cons
                                        (webhax::translate-key ,store (car x))
                                        (cdr x)))
                                     data))))
         ,@body))))

(defvar *date-test-result*)
(defun date-test-form ()
  (ask
    (form
     (q item :date :nullok))
    :target (lambda (data)
        (setf *date-test-result* (gethash 'item data)))))

(test ask-date
  (is (null
       (progn
         (ask-test-fixture (date-test-form)
                           (send :update '((item . ""))))
         (car *date-test-result*))))
  (is (eq :error
          (caar (ask-test-fixture (date-test-form)
                                  (send :update '((item . "today"))))))))
