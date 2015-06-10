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
   #:ps-gadgets
   #:do-keyvalue
   #:collecting-string
   #:collecting
   #:->html
   #:*->html-list-handler*
   #:*->html-hash-handler*
   #:*->html-alist-handler*
   #:*->html-symbol-handler*
   #:*->html-misc-handler*
   #:*->html-main-handler*
   #:*->html-depth*
   #:*metaplate-part-names*
   #:define-parts
   #:define-layout
   #:define-page
   #:page-base
   #:two-side-columns
   #:def-webspecial
   #:*webspecial-validators*
   #:bind-webspecials
   #:>>integer
   #:pbit-featurebox-side
   #:pbit-featurebox-content
   #:str
   #:ps-page-mod
   #:assemble-page-mod
   #:set-value
   #:replace-with
   #:page-mod))



