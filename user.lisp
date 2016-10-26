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

(defun signed-up-p (user)
  "Because a user could sign in, say with OpenID, yet not be known on the site"
  nil) ;;FIXME: don't know how to tell yet.

(defun login-method ()
  (when (get-user-name)
    ))

(define-middleware webhaz-user (&key userfig-specs)
    (declare (ignore userfig-specs)))




