;;;; package.lisp

(defpackage #:webhax-validate
  (:use #:cl #:gadgets #:ratify)
  (:export
   #:mkparse-in-list
   #:ratify-wrapper
   #:mkparse-all-members))

(defpackage #:webhax
  (:use #:cl #:gadgets #:parenscript #:anaphora 
        #:alexandria #:webhax-validate #:cl-who #:thing-labels #:clack
        #:clack.request #:clack.response)
  (:shadowing-import-from #:parenscript #:switch #:call)
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
   #:set-value
   #:replace-with
   #:page-mod
   #:bind-validated-input
   #:create-route
   #:*request*
   #:*regular-web-input*
   #:*key-web-input*
   #:input-function-wrapper
   #:create-simple-route
   #:output-string
   #:create-page-mod
   #:form
   #:ask-parts
   #:add-part
   #:alist->ps-object-code
   #:set-href
   #:*menu-items*
   #:define-default-layout
   #:define-default-parts
   #:quick-page
   #:*metaplate-default-layout*
   #:*metaplate-default-parts*))



