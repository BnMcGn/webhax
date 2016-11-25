(in-package #:webhax-test)

(in-suite webhax-test)


'(test validate
  (is-false (nth-value 1 (callval #'val1 "4")))
  (is-true (nth-value 1 (callval #'val1 "5")))
  (is (= 5 (callval #'val1 "5")))
  (signals type-error (callval #'val1 5))
  (is-false (nth-value 1 (callval #'val1 ""))))


(defun pretend-env-nologin ()
  (CL-HASH-UTIL:PLIST->HASH
   (list
    :INPUT
    (list :LACK.SESSION
          (CL-HASH-UTIL:ALIST->HASH
           '((:OID-CONNECT-DESTINATION
              . "http://logintest.warflagger.com:5000/sign-up"))
           :EXISTING (MAKE-HASH-TABLE :TEST #'EQUAL))
          :REQUEST-METHOD :GET :SCRIPT-NAME "" :PATH-INFO
          "/sign-up" :SERVER-NAME "logintest.warflagger.com"
          :HEADERS
          (CL-HASH-UTIL:ALIST->HASH
           '(("cookie"
              . "lack.session=a9ec6e57f29a68dc68bab8d7a6968be0b086091b"))
           :EXISTING (MAKE-HASH-TABLE :TEST #'EQUAL))
          :COOKIES
          '(("lack.session"
            . "a9ec6e57f29a68dc68bab8d7a6968be0b086091b"))
          :BODY-PARAMETERS NIL)
    :OUTPUT nil)))

(defparameter *pretend-env* nil)

(defmacro ptest (&body body)
  `(webhax::call-with-webhax-environment
    (lambda () ,@body)
    (gethash :input *pretend-env*)))

(test user-nologin
  (setf *pretend-env* (pretend-env-nologin))
  (is-false (ptest (authenticated?)))
  (is-false (ptest (signed-up?)))
  (is (= 403 (car (ptest (check-authenticated)))))
  (is (= 403 (car (ptest (check-signed-up)))))
  (is-false (ptest (get-user-name)))
  (is-false (ptest (get-display-name))))


