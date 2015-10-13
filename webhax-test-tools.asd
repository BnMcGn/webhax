;;;; webhax.asd

(asdf:defsystem #:webhax-test-tools
  :description "Describe webhax here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:clack #:usocket)
  :serial t
  :components ((:file "test-tools")))
