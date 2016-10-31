;;;; user.lisp

(in-package #:cl-user)

(defpackage #:webhax-user
  (:use #:cl #:webhax-core #:webhax #:gadgets #:parenscript)
  (:shadowing-import-from #:lack.component #:call))

(in-package #:webhax-user)

;;; User identification stuff goes here.

(defun get-user-name ()
  (when *session*
    (gethash :username *session*)))

(defun get-display-name ()
  (when *session*
    (gethash :display-name *session*)))

(defun signed-up-p (&optional user)
  "Because a user could sign in, say with OpenID, yet not be known on the site"
  (let ((userfig:*userfig-user* user))
    (userfig:userfig-value 'signed-up)))

(defun login-method ()
  (when (get-user-name)
    (cond
      ((key-in-hash? :oid-connect-provider *session*) :openid-connect))))

(defun webhax-user (&key userfig-specs)
  (middleware-chain
   (userfig:userfig-component userfig-specs)
   (webhax-user-core )))

(defparameter *userfig-for-user*
  '(signed-up
    (:initial nil :editable nil :viewable t :type :datestamp
     :description "Date when you signed up"
     :documentation "Date when the user created an account. Nil if the user hasn't created an account.")
    screen-name
    (:initial nil :editable t :type :string
     :description "Your preferred screen name")
    email
    (:initial nil :editable t :type :email
     :description "Your email address")
    ))


;; From session :username :display-name :email?

'(defun sign-up-page ()
  (with-html-output-to-string (s)
      (:html
       (:head (:title "Sign up"))
       (:body (:h2 "New Account")
              (:p "Please confirm a few details to create your account.")
              (ask
                (form
                 (q )))))))

(define-middleware webhax-user-core ()
  (url-case
    (:sign-up (sign-up-page))
    ))




