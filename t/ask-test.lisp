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
      (done
       (server (setf result (answers)))))
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
     (q item :date))
    (done
     (server (setf *date-test-result* (gethash 'item (answers)))))))

(test ask-date
  (is (null
       (progn
         (ask-test-fixture (date-test-form)
           (send :update '((item . ""))))
         (car *date-test-result*))))
  (is (eq :error
          (caar (ask-test-fixture (date-test-form)
                                  (send :update '((item . "today"))))))))

;; Ask demo page

(defun ask-testing-tools ()
  (ps:ps

    (defun get-current-qs (tree)
      (chain -react-test-utils
             (scry-rendered-components-with-type tree widgi-select)))

    (defun get-displayable-manager (tree)
      (chain -react-test-utils
             (find-rendered-component-with-type tree ask-displayable-manager)))

    (defun send-edit (tree name value)
      "Use internal dispatch function to place a value into a test Ask form."
      (let ((widget-container
             (first-match
              (lambda (x)
                (equal name (@ x props name)))
              (get-current-qs tree))))
        (unless widget-container
          (raise "Tried to set inactive widget value"))
        (chain
         widget-container props
         (dispatch (create :type :edit :name name :value value)))))

    (defun send-submit (tree)
      (chain (get-displayable-manager tree) props
             (dispatch (create :type :submit))))

    (defun send-update-callback (tree cb)
      (chain (get-displayable-manager tree) props
             (dispatch (create :type 'set-callback :callback cb))))

    (defun oneshot (func)
      (lambda ()
        (when func
          (let ((tmp func))
            (setf func nil)
            (funcall tmp)))))

    ))

(defun ask-demo-page ()
  (html-out
    (:script :src "/static/reactest.js")
    (:script :src "/static/mocha.js")
    (:link :href "/static/mocha.css")
    (:script :type "text/javascript"
             (str (ps:ps (defvar -react-test-utils
                           (require "react-addons-test-utils"))
                         (defvar chai (require "chai"))))
             (str (ask-testing-tools))))

  (fake-ask (ask-demo)
    (q some "Are there any?" :yesno)
    (if (a some)
        (q enough? "How many?" (:pickone :options '(3 5 6 18)))
        (q want "Why not?" :string))
    (and (q are :yesno)
         (q you :yesno)
         (q sure? :yesno))
    (done
     (client (grab (answers)))))

  (html-out
    (:div :id "mocha")
    (:script
     :type "text/javascript"
     (str
      (ps:ps
        (defvar demo (ask-demo))
        (let ((sert (@ chai assert))
              (suite (@ mocha suite))
              (test (@ mocha test))
              (item nil))
          (chain mocha (setup "tdd"))
          (suite
           "Ask"
           (lambda ()
             (test "Ask session should start with a single widget"
               (lambda ()
                 (chain sert (length-of (get-current-qs demo) 1))))
             (test "Widget should be a yesno"
               (lambda ()
                 (setf item (chain (get-current-qs demo) 0))
                 (chain sert (equal "yesno" (@ item props widget)))))
             (test "Widget value should be undefined"
               (lambda ()
                 (chain sert (equal undefined (@ item props value)))))
             (test "Should submit without error"
               (lambda (done)
                 (send-edit demo (@ item props name) "true")
                 (send-update-callback
                  demo
                  (oneshot done))
                 (send-submit demo)
                 nil));;Beware the implicit return!
             (test "Widget should now be a pickone"
               (lambda ()
                 (setf item (chain (get-current-qs demo) 0))
                 (say (get-current-qs demo))
                 (chain sert (equal "pickone" (@ item props widget)))))
             )))
          (chain mocha (run)))))))


(register-demo-page 'ask-demo-page)


