;;;; package.lisp

(defpackage #:webhax-validate
  (:use #:cl #:gadgets #:ratify)
  (:export
   #:mkparse-in-list
   #:ratify-wrapper))

(defpackage #:webhax
  (:use #:cl #:gadgets #:parenscript #:anaphora 
	#:alexandria #:webhax-validate #:cl-who #:thing-labels)
  (:shadowing-import-from #:parenscript #:switch)
  (:export
   #:define-ask
   #:ask
   #:q
   #:a
   #:*session*
   #:*webhax-output*
   #:html-out
   #:call-ask-manager
   #:ps-gadgets))



