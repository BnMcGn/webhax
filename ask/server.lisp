;;;; ask.lisp

(in-package #:webhax)

;;;Server side and other bits of the DSL


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

(defclass ask-store ()
  ((stor
    :initarg :storage
    :initform (make-hash-table))
   (qs 
    :initarg :q-clauses)
   (names
    :initarg :names)
   validators))

(defmethod initialize-instance :after ((stor ask-store) &key)
  (with-slots (qs names validators) stor
    (setf validators 
	  (collecting-hash-table (:mode :replace)
	    (loop for q in qs
	       for n in names
	       do (collect n (%q-validator q)))))))

(defgeneric add-input (stor input)
  (:documentation
   "Add input from a web source (alist format) to a storage object. Returns
   error messages if any input items don't validate."))

(defmethod add-input ((astor ask-store) input)
  (with-slots (validators names stor) astor
    (multiple-value-bind (good errors)
	(with-collectors (g< e<)
	  (dolist (n names)
	    (awhen (assoc n input :test #'equal)
	      (aif2only
	       (funcall (gethash n validators) (cdr it))
	       (g< (cons n it))
	       (e< (cons n it))))))
      (if errors
	  errors
	  (dolist (itm good)
	    (setf (gethash (first itm) stor) (cons (cdr itm) t)))))))

(defgeneric translate-key (stor key)
  (:documentation "Given the original symbol supplied by the user, return the
applicable numbered key."))
(defmethod translate-key ((astor ask-store) key)
  (with-slots (qs names) astor
    (assoc-cdr key (pairlis (mapcar #'second qs)
			    names))))
      
(defgeneric exists-answer (stor key &key translate))
(defmethod exists-answer ((astor ask-store) key &key translate)
  (with-slots (stor) astor
      (nth-value 1 (gethash 
		    (if translate (translate-key astor key) key) 
		    stor))))

(defgeneric answer (stor key &key translate))
(defmethod answer ((astor ask-store) key &key translate)
  (with-slots (stor) astor
    (car (gethash 
	  (if translate (translate-key astor key) key)
	  stor))))

(defgeneric all-answers (stor &key translate))
(defmethod all-answers ((astor ask-store) &key translate)
  (with-slots (qs names stor) astor
    (if translate
	(let ((trans-table
	       (pairlis names (mapcar #'second qs))))
	  (collecting-hash-table (:mode :replace)
	    (maphash 
	     (lambda (k v)
	       (collect (assoc-cdr k trans-table) v))
	     stor))))
    stor))
	     
	   