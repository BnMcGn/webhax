;;;; webhax-route.asd

(asdf:defsystem #:webhax-route
  :description "Describe webhax here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:alexandria
               #:cl-utilities
               #:gadgets
               #:webhax-core
               #:webhax-metaplate
               #:lack)
  :serial t
  :components ((:file "route")))

