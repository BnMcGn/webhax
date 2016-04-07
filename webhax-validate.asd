;;;; webhax-validate.asd

(asdf:defsystem #:webhax-validate
  :description "Describe webhax here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:gadgets
               #:alexandria
               #:anaphora
               #:cl-utilities
               #:ratify)
  :serial t
  :components ((:file "validate")))

