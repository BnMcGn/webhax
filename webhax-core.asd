;;;; webhax.asd

(asdf:defsystem #:webhax-core
  :description "Describe webhax-core here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:gadgets
               #:alexandria
               #:cl-who
               #:clack
               #:lack-request
               #:lack-response)
  :serial t
  :components ((:file "webhax-core")))

