
(defpackage #:webhax-test
  (:use #:cl #:webhax #:fiveam #:webhax-validate #:webhax-user #:gadgets
        #:cl-who
        #:alexandria #:cl-react #:parenscript)
  (:shadowing-import-from #:parenscript #:switch))

(in-package :webhax-test)
(def-suite webhax-test)
