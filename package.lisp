;;;; package.lisp

(defpackage #:webhax
  (:use #:cl #:gadgets #:html-thing-lister #:parenscript #:anaphora 
	#:alexandria #:webhax-validate)
  (:export
   #:define-ask
   #:ask
   #:q
   #:a))

(defpackage #:webhax-validate
  (:use #:cl #:gadgets #:ratify)
  (:export
   #:mkparse-in-list
   #:ratify-wrapper))

