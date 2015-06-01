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
;   (or :validator :validator-and :validator-or)
;  
;Symbol-or-string will be used as a visible label (via thing-labels) if no other label is provided. Also used as a key for return data.
;-Q must decide whether it has been successfully filled. Validation. Use 1st return value for value(s), second for status. Can return failed items!
;-Maybe should spec. behavior on failure? Error messages? Might be frontend dependent too.

;Form spec:
;(form (list #q #q #q #dq ,@dq))
;-Might also contain text additions.
;-Should it output a page-parts collection?

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
  (multiple-value-bind (nbody qs names)
      (process-ask-code body)
    (ask-page-insert nbody qs names)))

(defmacro t-ask (&body body)
  (multiple-value-bind (nbody qs names)
      (process-ask-code body)
    `(values
      ,(create-ask-manager nbody qs names)
      ',names)))

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
    (print (funcall askman nil))
    (print (funcall askman `((,(car names) . "true"))))
    (print (funcall askman `((,(second names) . "5"))))
    (print (funcall askman `((,(third names) . "fred"))))
    (print (funcall askman `((,(fourth names) . "false"))))))


(defun tester ()
  (ask
    (q some "Are there any?" :yesno)
    (if (a some)
	(q enough? "How many?" :pickone :source '(3 5 6 18))
	(q want "Why not?" :string))
    (and (q are :yesno)
	 (q you :yesno)
	 (q sure? :yesno))))


