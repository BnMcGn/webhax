;;;; package.lisp


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
        #:webhax-json-call #:webhax-route #:webhax-metaplate
        #:cl-who #:clack
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
   #:html-out-str
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
   #:set-href
   #:*menu-items*
   #:define-default-layout
   #:define-default-parts
   #:display-page
   #:quick-page
   #:*metaplate-default-layout*
   #:*metaplate-default-parts*
   #:register-json-call
   #:register-json-symbols
   #:json-call
   #:clack-tool
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
   #:fake-ask
   #:url-case
   #:define-middleware
   #:define-webapp
   #:define-simple-middleware
   #:define-simple-webapp
   #:with-content-type
   #:as-html
   #:as-json
   #:middleware-chain
   #:*clack-app*
   #:*web-env*
   #:call-endware
   #:*webhax-link-collection*
   #:register-link
   #:init
   #:main
   #:react-parts
   #:redux-parts
   #:page-base
   #:two-side-columns
   #:write-html-file
   #:*named-text-locations*
   #:named-text))



