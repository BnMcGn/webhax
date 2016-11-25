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
               #:cl-json
               #:thing-labels
               #:clack-v1-compat
               #:clack
               #:ningle
               #:cl-react
               #:ps-gadgets
               #:ps-react-gadgets
               #:webhax-validate
               #:userfig
               #:optima)
  :serial t
  :components ((:file "package")
               (:file "webhax-core")
               (:file "web-input")
               (:file "metaplate")
               (:file "webhax" :depends-on ("web-input" "metaplate"))
               (:file "clack-tool")
               (:file "json-call")
               (:file "html-s-exp")
               (:file "page-mod")
               (:file "bind-validated-input")
               (:file "demo-pages")
               (:module :ask
                        :serial t
                        :components ((:file "util")
                                     (:file "client")
                                     (:file "server")
                                     (:file "ask" :depends-on ("server"))))))

