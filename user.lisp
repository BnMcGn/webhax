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
   #:signed-up?))

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
    (let ((data (gethash :oid-connect-userinfo *session*)))
      ;;FIXME: Some of the field names need cleaning up.
      (if key
          (assoc-cdr key data)
          data))))

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
       (let ((userfig:*userfig-user* user))
         (userfig:userfig-value 'signed-up))))

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
  (middleware-chain
   (userfig:userfig-component userfig-specs)
   (webhax-user-core)))

(defun list-of-screen-names ()
  (remove-if #'null
             (userfig:map-users
              (lambda (user data)
                (declare (ignore user))
                (and data (gethash 'screen-name data))))))

(defparameter *userfig-for-user*
  '(signed-up
    (:initial nil :editable nil :viewable t :type :datestamp
     :description "Date when you signed up"
     :documentation "Date when the user created an account. Nil if the user hasn't created an account.")
    screen-name
    (:initial nil :editable t :type (:unique :options-func list-of-screen-names)
     :description "Your preferred screen name")
    email
    (:initial nil :editable t :type :email
     :description "Your email address")
    ))

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
                 (server (progn
                           (setf (userfig:userfig-value 'screen-name)
                                 (gethash 'screen-name (answers)))
                           (setf (userfig:userfig-value 'email)
                                 (gethash 'email (answers)))))
                 (client (setf (@ window location)
                               (lisp (login-destination))))))))))

(define-middleware webhax-user-core ()
  (url-case
    (:sign-up (sign-up-page))
    (otherwise
     (let ((result (funcall *clack-app* *web-env*)))
       ;;FIXME: Sometimes we shouldn't redirect to login page, such as on a
       ;; 403 from a json url. Add support for a flag in the header?
       (if (and (listp result) (eql 403 (car result)) (authenticated?))
           (progn
             (setf (login-destination) (url-from-env *web-env*))
             (sign-up-page))
           result)))))




