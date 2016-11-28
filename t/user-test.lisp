(in-package #:webhax-test)

(in-suite webhax-test)


'(test validate
  (signals type-error (callval #'val1 5)))


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
    (lambda ()
      (let ((userfig:*userfig-realm* 'test-userfig))
        ,@body))
    (gethash :input *pretend-env*)))

(test user-nologin
  (setf *pretend-env* (pretend-env-nologin))
  (is-false (ptest (authenticated?)))
  (is-false (ptest (signed-up?)))
  (is (= 403 (car (ptest (check-authenticated)))))
  (is (= 403 (car (ptest (check-signed-up)))))
  (is-false (ptest (get-user-name)))
  (is-false (ptest (get-display-name))))


(defun pretend-env-nosignup ()
  (CL-HASH-UTIL:PLIST->HASH
   (LIST :INPUT
         (list
          :LACK.SESSION
          (CL-HASH-UTIL:ALIST->HASH
           '((:OID-CONNECT-DESTINATION
              . "http://logintest.warflagger.com:5000/sign-up")
             (:OID-CONNECT-PROVIDER . :GOOGLE)
             (:OID-CONNECT-ACCESS-TOKEN . "large_random_number")
             (:OID-CONNECT-USERINFO (:SUB . "large_randomish_number")
              (:NAME . "Joe Random") (:GIVEN--NAME . "Joe")
              (:FAMILY--NAME . "Random") (:PICTURE . "file:///me.jpg")
              (:EMAIL . "joe@random.gov") (:EMAIL--VERIFIED . T)
              (:LOCALE . "en"))
             (:USERNAME . "_large_userid_@google")
             (:DISPLAY-NAME . "Joe"))
           :EXISTING (MAKE-HASH-TABLE :TEST #'EQUAL))
          :REQUEST-METHOD :GET :SCRIPT-NAME "" :PATH-INFO "/sign-up" :SERVER-NAME
          "logintest.warflagger.com" :SERVER-PORT 5000 :SERVER-PROTOCOL :HTTP/1.1
          :REQUEST-URI "/sign-up" :URL-SCHEME "http" :REMOTE-ADDR "127.0.0.1"
          :REMOTE-PORT 54708 :QUERY-STRING NIL 
          :HEADERS
          (CL-HASH-UTIL:ALIST->HASH
           '(("host" . "logintest.warflagger.com:5000")
             ("referer" . "http://logintest.warflagger.com:5000/sign-up")
             ("cookie"
              . "lack.session=a9ec6e57f29a68dc68bab8d7a6968be0b086091b"))
           :EXISTING (MAKE-HASH-TABLE :TEST #'EQUAL))
          :COOKIES
          '(("lack.session" . "a9ec6e57f29a68dc68bab8d7a6968be0b086091b"))
          :BODY-PARAMETERS NIL)
         :output nil)))

(test user-nosignup
  (setf *pretend-env* (pretend-env-nosignup))
  (is-true (ptest (authenticated?)))
  (is-false (ptest (signed-up?)))
  (is (= 403 (car (ptest (check-signed-up)))))
  (is (equal "joe@random.gov" (ptest (login-provider-fields :email)))))

