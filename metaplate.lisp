

(in-package :webhax)

;FIXME: finish deprecation of page-bit
(defmacro def-page-bit (name cat label &optional (tag :div))
  `(defmacro ,name (params &body body)
     (declare (ignore params))
     `(html-out
	(,,tag ,,cat ,,label
	       (htm ,@body)))))

(def-page-bit pbit-featurebox-side :class "featurebox-side")
(def-page-bit pbit-featurebox-content :class "featurebox-center")

(defparameter *metaplate-part-names*
  '(:@css :@javascript :@site-index :@title :@main-content 
    :@side-content :@site-search :@notifications :@external-links :@logo 
    :@account-info :@footnotes :@copyright :@messages))

(defmacro define-parts (name &body parts)
  `(eval-always
     (defun ,name (previous)
       (collecting-hash-table (:existing previous)
	 ,@parts))))

(eval-always
  (defun %make-part-func (key keyclauses)
    (let ((lines (collecting
		   (dolist (x keyclauses)
		     (when (eq (car x) key)
		       (dolist (y x)
			 (collect y)))))))
      (when lines
	`(lambda (previous)
	   (collecting-hash-table (:existing previous)
	     ,@lines))))))

(defmacro define-layout ((name &key wrapper) &body template)
  (multiple-value-bind (keyclauses template)
      (extract-keywords '(:prepend-parts :append-parts) template :in-list t)
    `(eval-always
       (defun ,name ()
	 (values
	  (quote ,(if wrapper 
		      (tree-search-replace (funcall-in-macro wrapper)
					   :match :@inner :value (car template))
		      template))
	  (quote ,(%make-part-func :prepend-parts keyclauses))
	  (quote ,(%make-part-func :append-parts keyclauses)))))))

(defun %render-part (key data params)
  (html-out
    (dolist (x (gethash key data))
      (if (stringp x)
	  (str x)
	  (apply #'funcall-in-macro x params)))))

(defun %render-title (key data params)
  (assert (eq key :@title))
  (html-out
    (:title (str
     (apply #'concatenate 'string
       (collecting 
	 (dolist (x (gethash :@title data))
	   (if (stringp x)
	       (collect x)
	       (collect (apply-in-macro x params))))))))))

(defun %render-javascript (key data params)
  (declare (ignore params))
  (assert (eq key :@javascript))
  ;FIXME: hack: assumes itm is js URL if it is a string. If func, will be 
  ;source code. 
  (html-out
    (dolist (itm (gethash :@javascript data))
      (if (functionp itm)
	  (htm (:script :type "text/javascript" (str (funcall itm))))
	  (htm (:script :src itm))))))

(defun %render-css (key data params)
  (declare (ignore params))
  (assert (eq key :@css))
  (html-out
    (dolist (itm (gethash :@css data))
      (htm (:link :href itm :rel "stylesheet" :type "text/css")))))

(defun %get-render-func (key)
  (assert (member key *metaplate-part-names*))
  (case key
    (:@css '%render-css)
    (:@javascript '%render-javascript)
    (:@title '%render-title)
    (otherwise '%render-part)))

(defun %expand-templates (templates parts-sym params-sym)
  (labels ((walk-tree (tree)
	     (if (atom tree)
		 (cond 
		   ((eq tree :@inner)
		    (unless (cdr templates)
		      (error "Last template should not contain :@inner"))
		    (%expand-templates (cdr templates) parts-sym params-sym))
		   ((member tree *metaplate-part-names*)
		    `(,(%get-render-func tree) ,tree 
		       ,parts-sym ,params-sym))
		   (t tree))
		 (cons (walk-tree (car tree))
		       (walk-tree (cdr tree))))))
    (walk-tree (car templates))))

(defun %collate-parts (parts)
  "Each part will be a function specifier - #'function or a lambda expression,
or an expression that otherwise evaluates to a function. This function will
take a pre-existing hash table as its sole parameter, and will return a hash
table"
  (if (null parts)
      '(make-hash-table)
      (if (functionp-in-macro (car parts))
	  `(funcall-in-macro ,(car parts) ,(%collate-parts (cdr parts)))
	  `(funcall ,(car parts) ,(%collate-parts (cdr parts))))))

(defmacro define-page (name parts templates)
  (let (prepend-parts append-parts)
    (labels ((proc-template (tmpl)
	       (multiple-value-bind (tmp pre app)
		   (funcall-in-macro tmpl)
		 (and pre (push pre prepend-parts))
		 (and app (push app append-parts))
		 tmp)))
      (with-gensyms (parts-sym params-sym)
	(let ((template (%expand-templates 
			 (collecting
			   (dolist (tm templates)
			     (collect (if (functionp-in-macro tm)
					  (proc-template tm)
					  tm))))
			 parts-sym params-sym)))
	  `(let ((,parts-sym 
		  ,(%collate-parts (concatenate 'list
				     prepend-parts parts append-parts))))
	     (,@(if name `(defun ,name) '(lambda)) (&rest ,params-sym)
		,@template)))))))

(define-layout (page-base) 
    (html-out
     (:html
      (:head
       :@title
       :@javascript
       :@css)
      (:body
       :@inner))))

(define-layout (two-side-columns :wrapper #'page-base)
  (:prepend-parts 
   (collect :@css "/static/css/style.css"))
  (html-out
    ;Header
    (:div :id "header_wrapper"
      (:div :id "header" :@logo)
      (:div :id "navcontainer"
	    (:ul :id "navlist" :@menu)))
    ;Main content
    (:div :id "left_side"
	  :@site-index :@side-content)
    (:div :id "right_side"
	  :@site-search :@account-info :@external-links)
    (:div :id "content"
	  :@messages :@main-content :@footnotes)
    ;Footer
    (:div :id "footer" :@copyright)))