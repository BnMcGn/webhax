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
   #:get-display-name
   #:get-user-name
   #:login-provider-fields
   #:user-info-bundle))

(in-package #:webhax-user)

;;; User identification stuff goes here.

(defun get-user-name ()
  (when *session*
    (gethash :username *session*)))

(defun get-display-name ()
  (when *session*
    (gethash :display-name *session*)))

(defun login-provider-fields (&optional key)
  (when *session*
    ;;FIXME: This is oid connect specific.
    (let* ((data (gethash :oid-connect-userinfo *session*))
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
  clack-openid-connect:*login-destination*)

(defsetf login-destination () (newval)
  `(setf clack-openid-connect:*login-destination* ,newval))

(defun authenticated? ()
  (gethash :username *session*))

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
      ((key-in-hash? :oid-connect-provider *session*) :openid-connect))))

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

;;;FIXME: Need good way to test this page.
(defun sign-up-page ()
  (check-authenticated)
  (funcall
   (webhax:quick-page
       (#'webhax::react
        #'webhax::redux
        :@javascript #'webhax-widgets:ps-widgets
        #'webhax:webhax-ask)
     (webhax:html-out
       (:h2 "New Account")
       (:p "Please confirm a few details to create your account.")
       (ask
         :prefill (list :screen-name (get-display-name)
                        :email (login-provider-fields :email))
         (form
          (q screen-name "Your preferred screen name"
             (:unique :options-func 'list-of-screen-names))
          (q email "Your email address" :email))
         (done
          (server (save-signed-up-user (answers)))
          (client (setf (@ window location)
                        (lisp (login-destination))))))))))

#|
;;;FIXME: Metaplate needs reworking. Temporary hack.
(defun %javascript-part-extract (partfunc)
  (with-output-to-string (s)
    (dolist (func (gethash :@javascript (funcall partfunc (make-hash-table))))
      (princ (funcall func) s))))

(defun sign-up-page ()
  (check-authenticated)
  (html-out-str
      (:html
       (:head
        (:title "Sign up")
        (:script :type "text/javascript"
           :src "https://cdnjs.cloudflare.com/ajax/libs/react/0.14.2/react.js")
        (:script :type "text/javascript"
           :src "https://cdnjs.cloudflare.com/ajax/libs/react/0.14.2/react-dom.js")
        (:script :type "text/javascript"
                 :src "/static/javascript/redux.js")
        (:script :type "text/javascript"
                 :src "/static/javascript/react-redux.js")
        (:script :type "text/javascript"
                 :src "/static/javascript/jquery/1.9.1/jquery.js")
        (:script :type "text/javascript" (str (cl-react:build)))
        (:script :type "text/javascript" (str (ps-gadgets:ps-gadgets)))
        (:script :type "text/javascript" (str (webhax-widgets:ps-widgets)))
        (:script :type "text/javascript" (str (%javascript-part-extract
                                               #'webhax:webhax-ask))))
       (:body (:h2 "New Account")
              (:p "Please confirm a few details to create your account.")
              (ask
                :prefill (list :screen-name (get-display-name)
                               :email (login-provider-fields :email))
                (form
                 (q screen-name "Your preferred screen name"
                    (:unique :options-func 'list-of-screen-names))
                 (q email "Your email address" :email))
                (done
                 (server (save-signed-up-user (answers)))
                 (client (setf (@ window location)
                               (lisp (login-destination))))))))))
|#

(define-middleware webhax-user-core ()
  (url-case
    (:sign-up (sign-up-page))
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
(setf clack-openid-connect:*login-destination-hook*
      (lambda (&key username)
        (if (userfig:new-user-p username)
            (signup-url)
            (login-destination))))

(defun user-info-bundle ()
  (let ((res (login-provider-fields)))
    (push (cons :login-url (clack-openid-connect:login-url)) res)
    (push (cons :logout-url (clack-openid-connect:logout-url)) res)
    (push (cons :settings-url (userfig:settings-url)) res)
    res))

(register-link 'clack-openid-connect::login
               (clack-openid-connect:login-url)
               :label "Log In")
(register-link 'clack-openid-connect::logout
               (clack-openid-connect:logout-url)
               :label "Sign Out")
(register-link 'userfig::settings
               (userfig:settings-url)
               :label "Settings")



