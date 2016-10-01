;;;; package.lisp

(defpackage #:webhax-core
  (:use #:cl #:gadgets #:alexandria #:clack #:lack.request #:lack.response
        #:cl-who)
  (:export
   #:*webhax-output*
   #:html-out
   #:*regular-web-input*
   #:*key-web-input*
   #:output-string
   #:eq-symb-multiple
   #:logged-in-p
   #:clack-tool
   #:function-wrapper
   #:execute
   #:multiple-key-p
   #:start-test-app
   #:*session*
   #:get-user-name
   #:get-display-name))

(defpackage #:webhax-json-call
  (:use #:cl #:webhax-core #:gadgets #:alexandria)
  (:export
   #:register-json-call
   #:register-json-symbols
   #:json-call
   #:register-json-symbol-func))

(defpackage #:webhax
  (:use #:cl #:gadgets #:parenscript #:anaphora 
        #:alexandria #:webhax-validate #:webhax-core
        #:webhax-json-call
        #:cl-who #:thing-labels #:clack
        #:lack.request #:lack.response #:cl-react)
  (:shadowing-import-from #:parenscript #:switch)
  (:shadowing-import-from #:lack.component #:call)
  (:export
   #:define-ask
   #:ask
   #:q
   #:a
   #:answers
   #:done
   #:client
   #:client/react
   #:*session*
   #:*webhax-output*
   #:html-out
   #:call-ask-manager
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
   #:set-href
   #:*menu-items*
   #:define-default-layout
   #:define-default-parts
   #:quick-page
   #:*metaplate-default-layout*
   #:*metaplate-default-parts*
   #:register-json-call
   #:register-json-symbols
   #:json-call
   #:clack-tool
   #:call
   #:execute
   #:define-react-class
   #:clack-server-manager
   #:under-path-p
   #:repath-clack-env
   #:url-from-env
   #:session-from-env
   #:mount-component
   #:done
   #:server
   #:register-demo-page
   #:demo-pages
   #:webhax-ask
   #:test-component
   #:*ask-mount-name*
   #:fake-ask))



