;;;; user.lisp

(in-package #:cl-user)

(defpackage #:webhax-user
  (:use #:cl #:webhax-core #:webhax #:gadgets #:parenscript)
  (:shadowing-import-from #:lack.component #:call)
  (:export
   #:list-of-screen-names
   #:webhax-user
   #:check-authenticated
   #:check-signed-up
   #:authenticated?
   #:signed-up?
   #:get-user-name
   #:login-provider-fields
   #:user-info-bundle
   #:signed-up
   #:screen-name
   #:email
   #:login-destination
   #:get-user-by-screenname
   #:get-openid-display-name))

(in-package #:webhax-user)

;;; User identification stuff goes here.

(defun get-user-name ()
  "This is the unique identifier from OpenID"
  (when *session*
    (gethash :username *session*)))

(defun get-openid-display-name ()
  "Taken from OpenID data"
  (when *session*
    (gethash :display-name *session*)))

(defun login-provider-fields (&optional key)
  (when *session*
    ;;FIXME: This is openid connect specific.
    ;;FIXME: More keys needed??
    (let* ((data (gethash :clath-userinfo *session*))
           (result
            (list
             (cons :name (assoc-cdr :name data))
             (cons :given-name (assoc-cdr :given--name data))
             (cons :family-name (assoc-cdr :family--name data))
             (cons :picture (assoc-cdr :picture data))
             (cons :email (assoc-cdr :email data))
             (cons :email-verified (assoc-cdr :email--verified data))
             (cons :locale (assoc-cdr :locale data)))))
      (if key
          (assoc-cdr key result)
          result))))

;;FIXME: Rethink someday. Oid connect specific.
(defun login-destination ()
  clath:*login-destination*)

(defsetf login-destination () (newval)
  `(setf clath:*login-destination* ,newval))

(defun authenticated? ()
  (and (hash-table-p *session*) (gethash :username *session*)))

(defun signed-up? (&optional user)
  "Because a user could sign in, say with OpenID, yet not be known on the site"
  (and (authenticated?)
       (let ((userfig:*userfig-user* (or user (authenticated?))))
         (userfig:initialized?))))

(defun check-authenticated ()
  (unless (authenticated?)
    (error 'web-fail :response 403 :text "Please log in")))

;;;For now, we leave the middleware to decide whether a login or signup
;;; is needed.
(defun check-signed-up ()
  (check-authenticated)
  (unless (signed-up?)
    (error 'web-fail :response 403 :text "User not a member here")))

(defun login-method ()
  (when (get-user-name)
    (cond
      ((key-in-hash? :clath-provider *session*) :openid-connect))))

(defun webhax-user (&key userfig-specs)
  (let ((specs (concatenate 'list *userfig-for-user* userfig-specs)))
    (middleware-chain
     (userfig:userfig-component specs)
     (webhax-user-core))))

(defun list-of-screen-names ()
  (remove-if #'null
             (userfig:map-users
              (lambda (user data)
                (declare (ignore user))
                (and data (gethash 'screen-name data))))))

(defun get-user-by-screenname (sname)
  (declare (type string sname))
  (userfig:map-users (lambda (user data)
               (when (equal (gethash 'screen-name data) sname)
                 (return-from get-user-by-screenname user)))))

(defparameter *userfig-for-user*
  '(signed-up
    (:date :initial nil :editable nil :viewable t
     :description "Sign up date"
     :documentation "Date when you first signed up.")
    screen-name
    ((:unique :options-func list-of-screen-names) :initial nil :editable t
     :description "Preferred screen name")
    email
    (:email :initial nil :editable t
     :description "Email address")
    ))

(defun save-signed-up-user (settings)
  (funcall (getf *web-env* :userfig-initialize-user) (authenticated?))
  (setf (userfig:userfig-value 'screen-name)
        (gethash 'screen-name settings))
  (setf (userfig:userfig-value 'email)
        (gethash 'email settings))
  ;;FIXME: Better to store UTC datestamp? Visible to users.
  (setf (userfig:userfig-value 'signed-up)
        (local-time:now)))

(defun sign-up-page ()
  (bind-validated-input
      (&key (page-test-enabled :boolean))
    (unless page-test-enabled (check-authenticated))
    (funcall
     (webhax:quick-page
         (#'react-parts #'redux-parts
          :@javascript #'webhax-widgets:ps-widgets
          :@javascript #'webhax:webhax-ask)
       (webhax:html-out
         (:h2 "New Account")
         (:p "Please confirm a few details to create your account.")
         (ask
           :prefill (list :screen-name (get-openid-display-name)
                          :email (login-provider-fields :email))
           (form
            (q screen-name "Your preferred screen name"
               (:unique :options-func 'list-of-screen-names))
            (q email "Your email address" :email))
           (done
            (server (save-signed-up-user (answers)))
            (client (setf (@ window location)
                          (lisp (login-destination)))))))))))

(define-simple-middleware webhax-user-core ()
  (url-case
    (:sign-up (sign-up-page))
    (:user-status (print-user-status))
    (otherwise
     (let ((result (call-endware)))
       ;;FIXME: Sometimes we shouldn't redirect to login page, such as on a
       ;; 403 from a json url. Add support for a flag in the header?
       (if (and (listp result) (eql 403 (car result)) (authenticated?))
           (progn
             (setf (login-destination) (url-from-env *web-env*))
             (sign-up-page))
           result)))))

(defun signup-url ()
  "/sign-up/")

;;;Make the login process send user to sign-up page if not signed up.
(setf clath:*login-destination-hook*
      (lambda (&key username)
        (if (userfig:new-user-p username)
            (signup-url)
            (login-destination))))

(defun user-info-bundle ()
  ;;FIXME: Should userfig fields be in here? probably not.
  (let ((res (login-provider-fields)))
    (push (cons :login-url (clath:login-url)) res)
    (push (cons :logout-url (clath:logout-url)) res)
    (push (cons :settings-url (userfig:settings-url)) res)
    res))

(register-link 'clath::login
               (clath:login-url)
               :label "Log In")
(register-link 'clath::logout
               (clath:logout-url)
               :label "Sign Out")
(register-link 'userfig::settings
               (userfig:settings-url)
               :label "Settings")

(defun print-user-status ()
  (print "OpenID fields from session:")
  (print (gethash :clath-userinfo *session*))
  (print "Session :username")
  (print (get-user-name))
  (print "(authenticated?):")
  (print (authenticated?))
  (print "Session :display-name")
  (print (get-openid-display-name))
  (print "Userfig: (new-user-p (get-user-name))")
  (print (userfig:new-user-p (get-user-name)))
  (print "Userfig: what-user?")
  (print (userfig::what-user?))
  (print "Userfig: initialized?")
  (print (userfig:initialized?))
  (print "Userfig: 'screen-name")
  (print (userfig:userfig-value 'screen-name))
  (print "Userfig: 'email")
  (print (userfig:userfig-value 'email)))
