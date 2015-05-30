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

(defun extract-qs (code)
  (collecting 
    (dotree (itm code)
      (when (and (consp itm) (eq (car itm) 'q))
	(collect itm)))))

(defun process-ask-code (code)
  (let (res)
    (multiple-value-bind (syms qs)
	(with-collectors (syms< qs<)
	  (labels ((treeproc (tree)
		     (if (atom tree)
			 tree
			 (if (eq-symb (car tree) 'q)
			     (let ((isym 
				    (create-numbered-name (second tree))))
			       (syms< isym)
			       (qs< tree)
			       `(display ,isym))
			     (cons
			      (treeproc (car tree))
			      (treeproc (cdr tree)))))))
	    (setf res (treeproc code))))
      (values res qs syms))))


(defun %create-input-processor (qs names)
  (let ((validators
	 (collecting-hash-table (:mode :replace)
	   (loop for q in qs
		for n in names
		do (collect n (%q-validator q))))))
    (lambda (data storage)
      (multiple-value-bind (good errors)
	  (with-collectors (g< e<)
	    (dolist (n names)
	      (awhen (assoc n data :test #'equal)
		(aif2only
		 (funcall (gethash n validators) (cdr it))
		 (g< (cons n it))
		 (e< (cons n it))))))
	(if errors
	    errors
	    (dolist (itm good)
	      (setf (gethash (first itm) storage) (cons (cdr itm) t))))))))

(defun %prep-q-dispatch (q name prev-val)
  (list (cons :next (plist-hash-table (list name 
    (plist-hash-table
     (list :default
	   (if prev-val prev-val
	       (aif2only (keyword-value :default q) it nil)))))))))

(defun %unquote-q (q)
  "Decide what portions of the q should not be executed."
  (let ((newq (copy-list q)))
    (quotef (first newq)) ;The q
    (quotef (second newq)) ;The symbol/label
    (cons 'list newq)))

(defun %dispatch-keys (disp)
  (hash-table-keys (assoc-cdr :next disp)))

(defun create-ask-manager (code qs names)
  (with-gensyms (stor continuations dispatch inproc)
    `(let ((,stor (make-hash-table))
	   (,continuations nil)
	   (,dispatch nil)
	   (,inproc (%create-input-processor 
		     (list ,@(mapcar #'%unquote-q qs)) ',names)))
       (labels ((exists-answer (name)
		  (nth-value 1 (gethash name ,stor)))
		(answer (name)
		  (car (gethash name ,stor))))
	 (macrolet ((a (itm)
		      `(answer ,(assoc-cdr itm ',(pairlis (mapcar #'second qs)
							  names)))))
	   (cl-cont:with-call/cc
	     (labels 
		 ((display (name)
		    (let ((q (assoc-cdr name ',(pairlis names qs))))
		      (setf ,dispatch 
			    (%prep-q-dispatch q name (and (exists-answer name)
							  (a name))))
		      (cl-cont:let/cc k
			(push k ,continuations))
		      (answer name))))
	       ,@code
	       (setf ,dispatch '((:success . t))))))
	 (lambda (data)
	   (aif (funcall ,inproc data ,stor)
		(progn
		  (print data)
		  (print `((:error . ,it))))
		(values
		 (progn
		   (when (with-any/all/none
			   (dolist (keyname (%dispatch-keys ,dispatch))
			     (all (exists-answer keyname))) t)
		     (funcall (car ,continuations)))
		   ,dispatch)
		 ,stor)))))))

(defun register-ask-manager (aman &key (session *session*))
  (unless (gethash :askstore session)
    (setf (gethash :askstore session) (make-hash-table :test #'equal)))
  (let* ((stor (gethash :askstore session))
	 (id (loop for idx = (create-numbered-name :ask)
		while (gethash idx stor)
		finally (return idx))))
    (setf (gethash id stor) aman)
    id))

(defun call-ask-manager (aname data &key (session *session*))
  (let ((askstore (gethash :askstore session)))
    (unless (hash-table-p askstore)
      (error "Askstore not found."))
    (aif (gethash aname askstore)
	 (funcall it data)
	 (error 
	  (format nil "Form ~a not found in askstore." aname)))))

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
	    (q enough? "How many?" :pickone :source (3 5 6 18))
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


