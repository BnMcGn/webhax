;;;; webhax.asd

(asdf:defsystem #:webhax
  :description "Describe webhax here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:alexandria
               #:cl-utilities
               #:gadgets
               #:cl-who
               #:webhax-core)
  :serial t
  :components ((:file "metaplate")))

