;;;; webhax-widgets.asd

(asdf:defsystem #:webhax-widgets
  :description "Describe webhax-widgets here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:gadgets
               #:alexandria
               #:anaphora
               #:ps-gadgets
               #:cl-react)
  :serial t
  :components ((:file "ps-widgets")))

