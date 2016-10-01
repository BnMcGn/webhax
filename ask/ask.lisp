;;;; ask.lisp

(in-package #:webhax)

;;; Ask goes here. Hacks and debugging await!

;;; Ask is the webhax user interrogation DSL. Including forms.


(defmacro define-ask (name &body body)
  "Create a named Ask for later use, either standalone or as a subform."
  `(eval-always
     (defun ,name ()
       (values
        (quote ,body)
        :ask-form))))






;Ask DSL - first draft:
;
;Question spec:
;(q <symbol-or-string> :helpful-hint?
;   (or :picksome :pickone :string :yesno :integer -other validation specs
;       :subform? :date :time etc, etc)
;   (or :prefill :prefill-url(discouraged - not portable to other interfaces)))
;   (or :validator :and-validator :or-validator)
;   :nullok
;
;Symbol-or-string will be used as a visible label (via thing-labels) if no other label is provided. Also used as a key for return data.
;-Q must decide whether it has been successfully filled. Validation. Use 1st return value for value(s), second for status. Can return failed items!
;-Maybe should spec. behavior on failure? Error messages? Might be frontend dependent too.

;Form spec:
;(form (list #q #q #q #dq ,@dq))
;-Might also contain text additions.
;-Should it output a page-parts collection?

;Prefill:
;:prefill - uses eq-symb to match prefills to keys. Can be alist or hash.
;:prefill in a q will override general prefill for that q.

;Options: bad choice of name. Could be configuration options. But: matches html
;usage. Use "params" for config?
;Source? Ambiguous.

;When both prefill and options are provided:
;-options can contain prefill info with the selected field
;-if multiple, prefill and options selected info will be used
;-if single, last prefill will override.
;-if updating from JSON source is implemented, all prefill and option info
;-will be ignored as out of date once JSON is called.

;Termination:
;:target

;Multiform:
;(?? (or/and form #dq ...) (numspec or add one more on successful fill))

;(and #q #q #f)
;-Unhide next when first filled, etc..., return if all success

;(or #q #q #q)
;-Unhide next when first attempted + failed. Return first success.

;(cond
;  (#qa #qb ...)
;  (#qc #qd ...))
;Ask #qb if #qa successful, et cetera.

;(when (eq #qa x)
;  ...

;(if #qa
;    #qb
;    (form #fc))
;"


'(t-ask
  (q some "Are there any?" :yesno)
  (if (a some)
      (q enough? "How many?" :pickone :source '(3 5 6 18))
      (q want "Why not?" :string))
  (and (q are :yesno)
   (q you :yesno)
   (q sure? :yesno)))

(defmacro ask (&body body)
  (bind-extracted-keywords
      (body short-body (:prefill :multiple))
    (let ((*ask-prefills* prefill))
      (multiple-value-bind (nbody qs names)
          (process-ask-code short-body)
        (%%ask-page-insert nbody qs names)))))

(defmacro fake-ask ((name) &body body)
  (bind-extracted-keywords
      (body short-body (:prefill :multiple))
    (let ((*ask-prefills* prefill))
      (multiple-value-bind (nbody qs names)
          (process-ask-code short-body)
        (let ((*ask-mount-name* name))
          (%%ask-test-create nbody qs names))))))

;;;FIXME: Review the service url, make sure it works everywhere.
(defun create-ask-service (app-obj &key (url "/ask-data/"))
  (create-route (app-obj url :content-type "text/json")
      ((askid (webhax-validate:ratify-wrapper :overlength)))
    (print (output-string
            (json:encode-json-alist-to-string
             (webhax:call-ask-manager askid :update *key-web-input*))))))

(define-parts ask-parts
  (add-part :@javascript "/static/jquery.js")
  (add-part :@javascript #'webhax::ps-ask-lib)
  (add-part :@javascript #'webhax:ps-page-mod)
  ;;FIXME: select2 stuff should only be loaded when needed
  (add-part :@javascript
            "//cdnjs.cloudflare.com/ajax/libs/select2/4.0.0/js/select2.min.js")
  (add-part :@css
            "//cdnjs.cloudflare.com/ajax/libs/select2/4.0.0/css/select2.min.css"))

(defun default-ask-finish (astore)
  (declare (ignore astore))
  (create-page-mod
   (replace-with (format nil "form#~a > div" *ask-formname*)
                 (html-out (:div (:span "Entry Submitted"))))))

(defmacro t-ask (&body body)
  (bind-extracted-keywords
      (body short-body :target :finish (:prefill :multiple))
    (let ((*ask-prefills* prefill))
      (multiple-value-bind (nbody qs names)
          (process-ask-code short-body)
        `(values
          ,(create-ask-manager nbody qs names)
          ',names)))))
