(in-package :cl-user)

(defpackage #:webhax-metaplate
  (:use #:cl #:webhax-core #:gadgets #:alexandria #:cl-who)
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
   #:redux-parts
   #:display-page
   #:page-base))

(in-package #:webhax-metaplate)

(eval-always
  (defparameter *metaplate-part-names*
    '(:@css :@javascript :@head :@site-index :@title :@menu :@inner
      :@side-content :@site-search :@notifications :@external-links :@logo
      :@account-info :@footnotes :@copyright :@messages
      :@css-link :@javascript-link)))

(defun %output-item (itm)
  "Return value of this function should be ignored. "
  (if (stringp itm)
      (write-line itm *webhax-output*) ;; FIXME: write-string? no newline?
      (funcall-in-macro itm)))

(defun %prep-item (itm)
  "Returns string of item."
  (if (stringp itm)
      itm
      (funcall-in-macro itm)))

(defun %render-part (key)
  (mapc #'%output-item (gethash key *parts*)))

(defun %render-title (key)
  (assert (eq key :@title))
  (html-out
    (:title
     (mapc #'%output-item (gethash :@title *parts*)))))

;;FIXME: Javascript rendering is different than general. Confusion likely.
(defun %render-javascript (key)
  (assert (eq key :@javascript))
  (html-out
    (dolist (itm (gethash :@javascript *parts*))
      (htm (:script :type "text/javascript" (str (%prep-item itm)))))))

(defun %render-javascript-link (key)
  (assert (eq key :@javascript-link))
  (html-out
    (dolist (itm (gethash :@javascript-link *parts*))
      (htm (:script :type "text/javascript"
                    :src (%prep-item itm))))))

(defun %render-css (key)
  (declare (ignore key))
  ;;FIXME:
  (when (gethash :@css *parts*)
    (error "Not Implemented")))

(defun %render-css-link (key)
  (assert (eq key :@css-link))
  (html-out
    (dolist (itm (gethash :@css-link *parts*))
      (htm (:link :href itm :rel "stylesheet" :type "text/css")))))

(eval-always
  (defun %get-render-func (key)
    (assert (member key *metaplate-part-names*))
    (case key
      (:@css '%render-css)
      (:@css-link '%render-css-link)
      (:@javascript '%render-javascript)
      (:@javascript-link '%render-javascript-link)
      (:@title '%render-title)
      (:@inner '%render-inner)
      (otherwise '%render-part))))

(defvar *parts*)
(defvar *template-stack* nil)

(defun %render-inner (key)
  (declare (ignore key))
  (if *template-stack*
    (let ((tmpl (car *template-stack*))
          (*template-stack* (cdr *template-stack*)))
      (funcall tmpl))
    ;;FIXME: Should use %ensure-string here?
    (dolist (itm (gethash :@inner *parts*))
      (funcall itm))))

(eval-always
  (defun %%process-template (template)
    "Convert all :@ tags into %render- calls"
    (labels ((walk-tree (tree)
               (if (atom tree)
                   (if (member tree *metaplate-part-names*)
                       `(,(%get-render-func tree) ,tree)
                       tree)
                   (cons (walk-tree (car tree))
                         (walk-tree (cdr tree))))))
      (walk-tree template))))

(defmacro define-parts (name &body parts)
  `(eval-always
     (defun ,name ()
       (hu:plist->hash
        (list ,@parts) :mode :append))))

(defmacro define-layout ((name &key wrapper) &body template)
  ;;Creates a function that returns three functions:
  ;; - function that runs the layout
  ;; - function that returns hash table containing any prepended parts
  ;; - function that returns hash table containing any appended parts
  (multiple-value-bind (keyclauses template)
      (extract-keywords '(:prepend-parts :append-parts) template :in-list t)
    (let ((pre (flatten-1 (assoc-all :prepend-parts keyclauses)))
          (app (flatten-1 (assoc-all :append-parts keyclauses))))
      `(eval-always
         (defun ,name ()
           (values
            (cons (lambda ()
                    ,(%%process-template (car template)))
                  ,(when wrapper
                         `(funcall ,wrapper)))
            (hu:plist->hash (list ,@pre) :mode :append)
            (hu:plist->hash (list ,@app) :mode :append)))))))

(defvar *metaplate-default-layout*)
(defvar *metaplate-default-parts*)

(defmacro define-default-parts (name &body parts)
  `(eval-always
     (define-parts ,name ,@parts)
     (setf *metaplate-default-parts* (symbol-function ',name))))

(defmacro define-default-layout ((name &key wrapper) &body template)
  `(eval-always
     (define-layout (,name :wrapper ,wrapper) ,@template)
     (setf *metaplate-default-layout* (symbol-function ',name))))

(defun %mapc-template-items (func input)
  "Send items one at a time to func, unless starts with a :@ keyword. Then send two items."
  (if (null input)
      nil
      (if (member (car input) *metaplate-part-names*)
          (if (null (cdr input))
              (error "Metaplate tag needs a part after it.")
              (progn
                (funcall func (car input) (second input))
                (%mapc-template-items func (cddr input))))
          (progn
            (funcall func (car input) nil)
            (%mapc-template-items func (cdr input))))))

(defun %process-template-items (items)
  "This function does not know how to handle function items that are not eitherparts functions or template functions."
  (let* ((templates nil)
         (parts
          (hu:collecting-hash-table (:mode :concatenate)
              (%mapc-template-items
               (lambda (item aux)
                 (typecase item
                   (keyword
                    (unless (member item *metaplate-part-names*)
                      (error "Not a metaplate tag"))
                    (hu:collect item aux :mode :append))
                   (hash-table
                    (maphash #'hu:collect item))
                   (function
                    (let ((result (multiple-value-list (funcall item))))
                      (cond
                        ((= 3 (length result))
                         (dolist (tmp (reverse (car result)))
                           (push tmp templates))
                         (maphash
                          (alexandria:rcurry
                           #'hu:collect
                           :mode
                           (lambda (existing new)
                             (concatenate 'list new existing)))
                          (second result))
                        (maphash #'hu:collect (third result)))
                        ((hash-table-p (car result))
                         (maphash #'hu:collect (car result)))
                        (t (error
                            "Not a parts collection or template")))))))
               items))))
    (values (nreverse templates) parts)))

(defun %render (templates parts)
  "Templates will be a list of template functions, from outer to inner. Parts will be a hash table of :@<label> keys containing lists of parts."
  (let ((*template-stack* (cdr templates))
        (*parts* parts))
    (when templates
      (funcall (car templates)))))

(defun display-page (&rest templates-and-parts)
  (multiple-value-bind (templates parts)
      (%process-template-items
       ;;Sometimes we'll get macro-style function designators coming through...
       (mapcar
        (lambda (x)
          (if-let ((fname (gadgets:get-function-name-in-macro x)))
            (symbol-function fname)
            x))
        templates-and-parts))
    (%render templates parts)))

;;;End of metaplate core items

(defun render-menu (&rest _)
  (declare (ignore _))
  (dolist (item webhax-core:*menu-items*)
    ;;FIXME: handle subitems
    (when (= (length item) 2)
      (html-out
        (:li :class (when (equal (butlast item) webhax-core:*menu-active*)
                      "active")
             (:a :href (car (last item))
                 (str (capitalize-first (car item)))))))))

(define-layout (page-base)
  (html-out
    (:html
     (:head
      :@head
      :@title
      :@javascript-link
      :@javascript
      :@css-link
      :@css)
     (:body
      :@inner))))

(define-layout (two-side-columns :wrapper #'page-base)
  (:prepend-parts
   :@css-link "/static/css/style.css"
   :@menu #'render-menu)
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
          :@messages :@inner :@footnotes)
                                        ;Footer
    (:div :id "footer" :@copyright)))

;;;;;;;;
;;; React
;;;;;;;;

;;FIXME: Because the server can't access the cl-react.lisp file. Whole JS
;; deployment needs reworking.
(defvar *clreact-build* (react:build))

#|
(define-parts react-parts
  :@javascript-link
  "https://cdnjs.cloudflare.com/ajax/libs/react/0.14.9/react.js"
  :@javascript-link
  "https://cdnjs.cloudflare.com/ajax/libs/react/0.14.9/react-dom.js"
  :@javascript (lambda () *clreact-build*))
|#

(define-parts react-parts
  :@javascript-link "/static/javascript/warflagger-bundle.js"
  :@javascript-link "https://cdnjs.cloudflare.com/ajax/libs/babel-polyfill/6.26.0/polyfill.js"
  :@javascript-link "https://unpkg.com/react@16.0.0/umd/react.development.js"
  :@javascript-link "https://unpkg.com/react-dom@16.0.0/umd/react-dom.development.js"
  :@javascript (ps:ps (setf (ps:@ -react create-class) (require "create-react-class"))
                      (setf (ps:@ -react -d-o-m) (require "react-dom-factories")))
 ; :@javascript-link ""
  :@javascript (lambda () *clreact-build*))

  ;  <script src="https://unpkg.com/babel-standalone@6/babel.min.js"></script>


(define-parts redux-parts
  :@javascript-link
  "https://cdnjs.cloudflare.com/ajax/libs/redux/3.5.2/redux.js"
  :@javascript-link
  "https://cdnjs.cloudflare.com/ajax/libs/react-redux/4.4.5/react-redux.js")
