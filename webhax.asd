;;;; webhax.asd

(asdf:defsystem #:webhax
  :description "Describe webhax here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:gadgets
               #:cl-who
               #:parenscript
               #:alexandria
               #:anaphora
               #:cl-utilities
               #:cl-cont
               #:ratify
               #:cl-json
               #:thing-labels)
  :serial t
  :components ((:file "package")
               (:file "webhax")
               (:file "validate")
               (:file "metaplate")
               (:file "html-s-exp")
               (:file "web-input")
               (:file "page-mod")
               (:module :ask
                        :serial t
                        :components ((:file "util")
                                     (:file "client")
                                     (:file "server")
                                     (:file "ask" :depends-on ("server"))))))

