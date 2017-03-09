;;;; webhax-metaplate.asd

(asdf:defsystem #:webhax-metaplate
  :description "Describe webhax here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:alexandria
               #:cl-utilities
               #:gadgets
               #:cl-hash-util
               #:cl-who
               #:webhax-core
               #:cl-react)
  :serial t
  :components ((:file "metaplate")))

