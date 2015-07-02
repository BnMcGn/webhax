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
;   (or :source :source-url(discouraged - not portable to other interfaces)))
;   (or :validator :and-validator :or-validator)
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

(defvar *ask-target* nil)

(defmacro ask (&body body)
  (bind-extracted-keywords (body short-body :target :finish)
    (let ((*ask-target* (when (boundp 'target) target))
	  (*ask-finish* (when (boundp 'finish) finish))
	  (*ask-prefills* (when (boundp 'prefill) prefill))
      (multiple-value-bind (nbody qs names)
	  (process-ask-code short-body)
	(ask-page-insert nbody qs names)))))

(create-route (:ask-data :content-type "text/json")
    ((askid (webhax-validate:ratify-wrapper :overlength)))
  (print (output-string
	  (json:encode-json-alist-to-string
	   (webhax:call-ask-manager askid :update *key-web-input*)))))   

(defun default-ask-finish (astore)
  (declare (ignore astore))
  (create-page-mod
   (replace-with (format nil "form#~a > div" *ask-formname*)
		 (html-out (:div (:span "Entry Submitted"))))))

(setf *ask-finish* #'default-ask-finish)

(defmacro t-ask (&body body)
  (multiple-value-bind (nbody qs names)
      (process-ask-code body)
    (let ((*ask-target* nil))
      `(values
	,(create-ask-manager nbody qs names)
	',names))))

(defun server-test ()
  (multiple-value-bind (askman names)
      (t-ask
	(q some "Are there any?" :yesno)
	(if (a some)
	    (q enough? "How many?" :pickone :source '(3 5 6 18))
	    (q want "Why not?" :string))
	(and (q are :yesno)
	     (q you :yesno)
	     (q sure? :yesno)))
    (print names)
    (print (funcall askman :update nil))
    (print (funcall askman :update `((,(car names) . "true"))))
    (print (funcall askman :update `((,(second names) . "5"))))
    (print (funcall askman :update `((,(third names) . "fred"))))
    (print (funcall askman :update `((,(fourth names) . "false"))))))

(defun tester ()
  (ask
    :target #'print
    (q some "Are there any?" :yesno)
    (if (a some)
	(q enough? "How many?" :pickone :source '(3 5 6 18))
	(q want "Why not?" :string))
    (form (q are :yesno)
	 (q you :yesno)
	 (q sure? :yesno))))
