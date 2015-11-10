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
               #:thing-labels
               #:clack
               #:ningle
               #:cl-react)
  :serial t
  :components ((:file "package")
               (:file "webhax-core")
               (:file "web-input")
               (:file "webhax" :depends-on ("web-input"))
               (:file "clack-tool")
               (:file "json-call")
               (:file "validate")
               (:file "metaplate")
               (:file "html-s-exp")
               (:file "page-mod")
               (:file "bind-validated-input")
               (:module :ask
                        :serial t
                        :components ((:file "util")
                                     (:file "client")
                                     (:file "server")
                                     (:file "ask" :depends-on ("server"))))))

