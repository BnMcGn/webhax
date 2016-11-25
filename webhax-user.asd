;;;; webhax-user.asd

(asdf:defsystem #:webhax-user
  :description "Describe webhax-user here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:gadgets
               #:webhax
               #:parenscript
               #:alexandria
               #:anaphora
               #:cl-utilities
               #:ps-gadgets
               #:ps-react-gadgets
               #:webhax-validate
               #:clack-openid-connect ;FIXME: shouldn't
               #:userfig)
  :serial t
  :components ((:file "user")))

