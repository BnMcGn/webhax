;;;; client.lisp

(in-package #:webhax)

;;; Ask clientside/javascript stuff goes here.

(defun generate-q-data (q)
  `(lisp-raw
    (json:encode-json-alist-to-string
     (list*
      (cons :real-name ',(%q-real-name q))
      (webhax-validate:prep-fieldspec-body-for-json
       (%q-validator ,(%%unquote-q q)))))))

(defun prep-client-code-block (spec)
  `(create
    :executable (lambda () ,@(cdr spec))
    :is-element nil))

(defun prep-react-element (spec)
  `(create
    :executable (lambda () ,@(cdr spec))
    :is-element t))

(defun generate-client-data (symbols qs)
  `(create
    ,@(collecting
       (loop for s in symbols
          for q in qs
          do (progn
               (collect s)
               (collect (case (car q)
                          (q (generate-q-data q))
                          (client (prep-client-code-block q))
                          (client/react (prep-react-element q)))))))))

(define-parts webhax-ask
  (add-part
   :@javascript
   (lambda ()
     (ps

       (defun %ask-answers (data fieldspecs)
         (let ((res (-object)))
           (do-keyvalue (k fspec fieldspecs)
             (setf (getprop res (@ fspec :real-name))
                   (getprop data k)))
           res))

       (def-component ask-main
           (psx
            (:ask-server-connection
             :commands (prop commands)
             :askname (prop askname)
             :info (prop info)
             ;;NOTE: prefill is not used by ask. prefills are put in the updates
             ;;from the server
             :prefill (prop prefill)
             :server-url (prop :server-url)))
         prop-types
         (create
          :server-url (chain -react -prop-types string is-required)))

       (def-component ask-server-connection
           (psx
            (:ask-collection-layer
             :commands (state commands)
             :ordering (state ordering)
             :errors (state errors)
             :command-keys (chain -object (keys (state commands)))
             :info (prop info)
             :prefill (prop prefill)
             :dispatch (@ this call-server)))
         get-initial-state
         (lambda ()
           ;;commands prop switches nature here:
           ;;instead of containing subfields :next :errors, next is passed down as
           ;;commands and errors gets its own prop.
           (let ((commands (if (chain (prop commands) (has-own-property :next))
                               (prop commands next)
                               (throw "Ask: empty commands parameter")))
                 (errors (if (chain (prop commands) (has-own-property :errors))
                             (prop commands errors)
                             (-object))))
             (create :commands commands :errors errors
                     :ordering (prop commands ordering))))
         call-server
         (lambda (updates)
           (ps-gadgets:json-post-bind (commands
                                       (ps-gadgets:strcat
                                        (prop server-url) (prop askname))
                                       updates)
              (when (chain commands (has-own-property :|next|))
                (set-state commands (@ commands next))
                (set-state ordering (@ commands ordering)))
              (when (chain commands (has-own-property :errors))
                (set-state errors (@ commands errors))))))

       (def-component ask-collection-layer
           (psx
            (:ask-displayable-manager
             :commands (prop commands)
             :ordering (prop ordering)
             :info (prop info)
             :data (state data)
             :dispatch (@ this dispatch)
             :errors (prop errors)))
         ;; Prefill: state of fields at start of form, fields optional
         ;; Data: current state of all fields, fields optional
         ;; Current: a copy of Data that contains only items found in
         ;; command-keys, fields optional
         ;; Dispatch on submit: must contain all of the keys in command-keys
         ;; even if they aren't found anywhere else
         get-initial-state
         (lambda ()
           (create :data (or (prop prefill) (-object))))
         dispatch
         (lambda (action) ;Can be replaced with redux dispatching.
           (case (@ action type)
             (:submit
              (funcall
               (prop dispatch)
               (let ((res (-object))
                     (dat (state data)))
                 (dolist (k (prop command-keys))
                   (if (chain dat (has-own-property k))
                       (setf (getprop res k) (getprop dat k))
                       (setf (getprop res k) nil)))
                 res)))
             (:edit
              ;;FIXME: add client-side validation here?
              (if (member (@ action name) (prop command-keys))
                  (set-state :data
                             (set-copy (state data)
                                       (@ action name) (@ action value)))
                  (throw "Tried to write to a non-current Ask field")))
             (otherwise
              (throw "No such action!")))))

       (def-component ask-displayable-manager
           (psx
            (:webhax-form-toplevel
             :prefill (or (prop data) (create))
             :errors (prop errors)
             :dispatch (prop dispatch)
             :fieldspecs
             (ps-gadgets:collecting
                 (dolist (k (prop ordering))
                   (ps-gadgets:collect k)
                   (let ((fspec (getprop (prop info) k)))
                     (if (chain fspec (has-own-property :executable))
                         (let ((res
                                (labels ((answers ()
                                           (%ask-answers
                                            (prop data) (prop info))))
                                  (funcall (@ fspec :executable)))))
                           (when (@ fspec :is-element)
                             (ps-gadgets:collect (create :prebuilt res))))
                         (ps-gadgets:collect fspec)))))))
         prop-types
         (create :ordering
                 (chain -react -prop-types
                        (array-of
                         (chain -react -prop-types string)))))

       ))))

(defun %%ask-page-insert (nbody qs names)
  `(let* ((formname (register-ask-manager
                     ,(create-ask-manager nbody qs names)))
          (initial-display (call-ask-manager formname :update nil)))
     (mount-component (ask-main)
       :commands (lisp-raw (json:encode-json-alist-to-string initial-display))
       :askname (lisp formname)
       :info ,(generate-client-data names qs)
       :server-url  (lisp *ask-control-url*))))
