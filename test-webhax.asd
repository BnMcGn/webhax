;;;; test-webhax.asd

(asdf:defsystem #:test-webhax
  :description "Describe webhax here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:webhax
               #:fiveam) 
  :serial t
  :components ((:module :t
                        :serial t
                        :components ((:file "package")
                                     (:file "bind-validated-test")
                                     (:file "ask-test")))))

