(in-package :cl-user)

(defpackage #:webhax-metaplate
  (:use #:cl #:webhax-core #:gadgets #:alexandria)
  (:export
   #:*metaplate-default-layout*
   #:*metaplate-default-parts*
   #:define-parts
   #:define-default-parts
   #:define-layout
   #:define-default-layout
   #:add-part
   #:define-page
   #:render-menu
   #:two-side-columns
   #:react-parts
   #:redux-parts))

(in-package #:webhax-metaplate)

(defparameter *metaplate-part-names*
  '(:@css :@javascript :@site-index :@title :@menu :@main-content
    :@side-content :@site-search :@notifications :@external-links :@logo
    :@account-info :@footnotes :@copyright :@messages))

(defvar *metaplate-default-layout*)
(defvar *metaplate-default-parts*)

(defmacro define-parts (name &body parts)
  `(eval-always
     (watch-for-recompile/auto-watcher ,name
       (defun ,name (previous)
         (collecting-hash-table (:existing previous :mode :append)
           (labels ((add-part (section part)
                      (collect section part)))
             ,@parts))))))

(defmacro define-default-parts (name &body parts)
  `(eval-always
     (define-parts ,name ,@parts)
     (setf *metaplate-default-parts* '(function ,name))))

(eval-always
  (defun %make-part-func (key keyclauses)
    (let ((lines (collecting
                   (dolist (x keyclauses)
                     (when (eq (car x) key)
                       (dolist (y x)
                         (collect y)))))))
      (when lines
        `(lambda (previous)
           (collecting-hash-table (:existing previous :mode :append)
             (labels ((add-part (section part)
                        (collect section part)))
               ,@lines)))))))

(defmacro define-layout ((name &key wrapper) &body template)
  (multiple-value-bind (keyclauses template)
      (extract-keywords '(:prepend-parts :append-parts) template :in-list t)
    `(eval-always
       (watch-for-recompile/auto-watcher ,name
         (defun ,name ()
           (values
            (quote ,(if wrapper
                        (leaves-search-replace (funcall-in-macro wrapper)
                                               :match :@inner
                                               :value (car template))
                        template))
            (quote ,(%make-part-func :prepend-parts keyclauses))
            (quote ,(%make-part-func :append-parts keyclauses))))))))

(defmacro define-default-layout ((name &key wrapper) &body template)
  `(eval-always
     (define-layout (,name :wrapper ,wrapper) ,@template)
     (setf *metaplate-default-layout* '(function ,name))))

;;;;;;;;;;;;;;;;;;;;;
;;; Define-page macro
;;;;;;;;;;;;;;;;;;;;;

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
  ;;FIXME: hack: assumes itm is js URL if it is a string. If func, will be
  ;;source code.
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

(defun %%expand-templates (templates parts-sym params-sym)
  (labels ((walk-tree (tree)
             (if (atom tree)
                 (cond
                   ((eq tree :@inner)
                    (unless (cdr templates)
                      (error "Last template should not contain :@inner"))
                    (%%expand-templates (cdr templates) parts-sym params-sym))
                   ((member tree *metaplate-part-names*)
                    `(,(%get-render-func tree) ,tree
                      ,parts-sym ,params-sym))
                   (t tree))
                 (cons (walk-tree (car tree))
                       (walk-tree (cdr tree))))))
    (walk-tree (car templates))))

(defun %%process-templates (templates parts-sym params-sym)
  (%%expand-templates
   (collecting
       (dolist (template templates)
         (if (functionp-in-macro template)
             (progn
               (push (get-function-name-in-macro template) *watch-names*)
               (multiple-value-bind (tmpl pre app) (funcall-in-macro template)
                 (and pre (push pre *prepend-parts*))
                 (and app (push app *append-parts*))
                 (collect tmpl)))
             (collect template))))
   parts-sym params-sym))

(defun %%collate-parts (parts)
  "Each part will be a function specifier - #'function or a lambda expression,
or an expression that otherwise evaluates to a function. This function will
take a pre-existing hash table as its sole parameter, and will return a hash
table"
  (if (null parts)
      '(make-hash-table)
      (if (functionp-in-macro (car parts))
          (progn
            (push (get-function-name-in-macro (car parts))
                  *watch-names*)
            `(funcall-in-macro ,(car parts) ,(%%collate-parts (cdr parts))))
          `(funcall ,(car parts) ,(%%collate-parts (cdr parts))))))

(defun %%process-parts (parts)
  (%%collate-parts
   (concatenate 'list *prepend-parts* (nreverse parts) *append-parts*)))

(defun add-part (section part)
  "This add-part is for use in the parts section of define-page."
  (lambda (previous)
    (collecting-hash-table (:existing previous :mode :append)
      (collect section part))))

(defvar *prepend-parts*)
(defvar *append-parts*)
(defvar *watch-names*)

(defmacro define-page (name parts templates)
  (with-gensyms (parts-sym params-sym)
    ;;Templates will sometimes include parts. They will placed in the
    ;;*pre/ap-end-parts* lists by the template processor, and picked up
    ;; by the parts processor. Both processors may supply names to be
    ;; watched for recompilation.
    (let* ((*prepend-parts* nil)
           (*append-parts* nil)
           (*watch-names* nil)
           (template (%%process-templates templates parts-sym params-sym))
           (parts (%%process-parts parts)))
      (if name
          `(watch-for-recompile
             (dependency-watcher (,name ,@*watch-names*)
               (defun ,name (&rest ,params-sym)
                 (let ((,parts-sym ,parts))
                   ,@template))))
          `(lambda (&rest ,params-sym)
             (let ((,parts-sym ,parts))
               ,@template
               (request-watch-on-names ',*watch-names*)))))))

;;;End define-page


(defun render-menu (&rest _)
  (declare (ignore _))
  (dolist (item *menu-items*)
                                        ;FIXME: handle subitems
    (when (= (length item) 2)
      (html-out
        (:li :class (when (equal (butlast item) *menu-active*) "active")
             (:a :href (car (last item)) (str (thing-label (car item)))))))))

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
   (add-part :@css "/static/css/style.css")
   (add-part :@menu #'render-menu))
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

;;;;;;;;
;;; React
;;;;;;;;

(define-parts react-parts
  (add-part :@javascript
            "https://cdnjs.cloudflare.com/ajax/libs/react/0.14.2/react.js")
  (add-part :@javascript
            "https://cdnjs.cloudflare.com/ajax/libs/react/0.14.2/react-dom.js")
  (add-part :@javascript #'react:build))

(define-parts redux-parts
  (add-part :@javascript
            "https://cdnjs.cloudflare.com/ajax/libs/redux/3.5.2/redux.js")
  (add-part :@javascript
            "https://cdnjs.cloudflare.com/ajax/libs/react-redux/4.4.5/react-redux.js"))
