;;;; demo-pages.lisp

(in-package #:webhax)

;;; Collect and display various temporary/demo/testing pages

(defvar *demo-pages* nil)

(defun register-demo-page (name)
  (unless (member name *demo-pages*)
    (push name *demo-pages*)))

(defun demo-pages ()
  ;(bind-validated-input
  ;    (&key
  ;     (show (:pickone :options *demo-pages*) :multiple t))
  (html-out
    (:div
     (:h2 "Pages")
     (dolist (itm *demo-pages*)
       (htm
        (:h3 (mkstr itm)))))))
