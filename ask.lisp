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

(defparameter *ask-control-types*
  '(:yesno :string :pickone :picksome :integer :date :month :datetime 
    :datetime-local))

(defvar *ask-control-url*)


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

(let ((counter 0))
  (defun create-numbered-name (name)
    (when (> counter (- most-positive-fixnum 2))
      (setf counter 0))
    (mkstr name (incf counter))))

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

(defun make-q-label (q)
  (if (stringp (third q))
      (third q)
      (thing-lister:thing-label	(second q))))

(defun get-q-type (q)
    (aif (first-match *ask-control-types* (lambda (x) (member x (cddr q))))
       it
       (error "Control type not found")))

(defun get-q-functype-symbol (q)
  (symb 'client- (get-q-type q)))

(defun generate-q-data (q)
  `(create
    :function ,(get-q-functype-symbol q)
    :label ,(make-q-label q)
    ,@(awhen2 (fetch-keyword :source q)
	      (list :source it))
    ,@(awhen2 (fetch-keyword :source-url q)
	      (list :source-url it))
    ,@(aif2 (fetch-keyword :default q)
	      (list :default it) (list :default nil))))

(defun generate-client-data (symbols qs)
  `(create
     ,@(collecting
	(loop for s in symbols
	     for q in qs
	     do (progn 
		  (collect s) 
		  (collect (generate-q-data q)))))))

(defun ps-ask-lib ()
  ;FIXME: divide into solid and doubtful, form and control
  (ps
    (defmacro do-keyvalue ((key val obj) &body body)
      (let ((obj-v (gensym)))
      `(let ((,obj-v ,obj))
	 (for-in (,key ,obj-v)
	    (if (chain ,obj-v (has-own-property ,key))
		(let ((,val (getprop ,obj-v ,key)))
		  ,@body))))))

    (defmacro collecting-string (&body body)
      (let ((res (gensym)))
	`(let ((,res ""))
	   (labels ((collect (itm)
		      (setf ,res (+ ,res itm))))
	     ,@body)
	   ,res)))

    (defmacro collecting (&body body)
      (let ((res (gensym)))
	`(let ((,res (array)))
	   (labels ((collect (itm)
		      (chain ,res (push itm))))
	     ,@body)
	   ,res)))

    (defun say (thing)
      (chain console (log thing)))

    ;FIXME: should decide between current and original default for fill.
    (defun make-control (name data)
      (who-ps-html
       (:div (:span (getprop data :label)) 
	     (:span (funcall (getprop data :function) name data)))))

    (defun display-specified-controls (target commands data)
      (setf (getprop data :_curr-disp) (chain commands :next))
      (setf (chain target inner-h-t-m-l)
	    (collecting-string
	     (dolist (k (chain -object (keys (chain commands :next))))
	       (collect (make-control k (getprop data k)))))))

    (defun update-form-data (stor data)
      (do-keyvalue (k v (chain data :next))
	(do-keyvalue (kk vv v)
	  (setf (getprop stor k kk) vv))))

    (defun get-control-parent-form (control)
      (if (equal (@ control tag-name) "FORM")
	  (@ control id)
	  (get-control-parent-form (@ control parent-element))))

    (defun control-updated (name control value) ;FIXME: doesn't have auto
      (let ((form (get-control-parent-form control)))
	(setf (getprop (chain document askdata) form name :current) value)))

    (defun post-ask-update (form &optional (url (lisp *ask-control-url*)))
      (let ((data (-object))
	    (currkeys
	     (chain -object (keys 
	       (getprop document 'askdata form :_curr-disp)))))
	(dolist (k currkeys)
	  (let ((v (getprop document 'askdata form k)))
	    (if (chain v (has-own-property :current))
		(progn (setf (getprop data k) (@ v current))
		       (setf (@ v current-saved) (@ v current))
		       (delete (@ v current)))
		(if (chain v (has-own-property :default))
		    (setf (getprop data k) (@ v default))))))
	(chain $ (get-j-s-o-n (+ url form) data
		   (lambda (x)
		     (update-form-data 
		      (getprop (chain document askdata) form) x)
		     (display-specified-controls
		      (chain document (get-element-by-id form) 
			     first-element-child)
		      x
		      (getprop (chain document askdata) form)))))))		

    (defmacro updatecode ()
      '(ps-inline (control-updated name this (@ this value))))

    (defun client-yesno (name params)
      (ps-html
       ((:input :type "radio" :name name :value "true" 
		:onchange (updatecode)
		(getprop params :default) :checked "checked") "Yes")
       ((:input :type "radio" :name name :value "false" 
		:onchange (updatecode)
		(not (getprop params :default)) :checked "checked") "No")))

    (defun client-pickone (name params)
      (collecting-string
       (dolist (x (getprop params :source))
	 (collect 
	   (ps-html
	    ((:input :type "radio" :name name :value x
	       :onchange (updatecode)
	       (eq (getprop params :default) x) :checked "checked") x))))))

    (defmacro make-simple-client-control (name type)
      `(defun ,(lisp (symb 'client- name)) (name params)
	 (ps-html
	  ((:input :type ,(lisp (string-downcase (mkstr type))) :name name
		   :value (getprop params :default)
		   :onchange (updatecode))))))
    
    (make-simple-client-control string text)
    (make-simple-client-control email email)
    (make-simple-client-control number number)
    (make-simple-client-control date date)
    (make-simple-client-control month month)
    (make-simple-client-control datetime datetime)
    (make-simple-client-control datetime-local datetime-local)))


;;; End PS stuff

;;;Validation


(let ((ratify-matches
       '(:email :number :date :month :datetime :datetime-local :integer)))
  (defun %ratify-sym-for-type (tsym)
    (cond ((member tsym ratify-matches) tsym)
	  ((eq tsym :yesno) :boolean)
	  (t nil))))

(defun %default-validator (q)
  (aif (%ratify-sym-for-type (get-q-type q))
       (ratify-wrapper it)
       (case (get-q-type q)
	 (:pickone 
	  (mkparse-in-list (fetch-keyword :source q)))
	 (:picksome
	  (mkparse-in-list (fetch-keyword :source q)))
	 (otherwise ;Default - limits input length
	  (ratify-wrapper :overlength))))) 

(defun %q-validator (q)
  "Validator spec: function that returns (values <adjusted val> t) if good, or (values <error message> nil) if bad."
  (%default-validator q));FIXME: add user validator support

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
 
(defun ask-page-insert (nbody qs names)
  "The part of an Ask that gets stuck into the web page, including the 
HTML form."
  `(let* ((formname (register-ask-manager
		     ,(create-ask-manager nbody qs names)))
	  (initial-display (call-ask-manager formname nil)))
     (html-out 
       (:form 
	:id formname
	(:div)
	(:input :type "button"
		:value "Next"
		:onclick 
		(let ((*js-string-delimiter* #\"))
		  (ps-inline (post-ask-update (lisp formname))))))
       (:script
	:type "text/javascript"
	(str (ps
	  (let ((formname (lisp formname))
		(formdata ,(generate-client-data names qs))
		(initstate 
		 (lisp-raw (json:encode-json-alist-to-string initial-display))))
	    (unless (chain document (has-own-property "askdata"))
	      (setf (chain document askdata) (create)))
	    (setf (getprop (chain document askdata) formname) formdata)
	    (display-specified-controls 
	     (chain document (get-element-by-id formname) first-element-child)
	     initstate
	     (getprop (chain document askdata) formname)))))))))

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


