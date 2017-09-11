
(in-package :webhax)

(defvar *->html-list-handler*)
(defvar *->html-hash-handler*)
(defvar *->html-alist-handler*)
(defvar *->html-symbol-handler*)

(defvar *->html-misc-handler*)
(defvar *->html-main-handler*)
(defvar *->html-depth* 0)

(defun ->html (thing)
  "Attempt to pleasantly display an s-expression as HTML"
  (let ((*->html-depth* (1+ *->html-depth*)))
    (funcall
     (cond
       ((listp thing) *->html-list-handler*)
       ((hash-table-p thing) *->html-hash-handler*)
       ((symbolp thing) *->html-symbol-handler*)
       (t *->html-misc-handler*))
     thing)))

(defun symbol-handler (thing)
  (html-out (str (thing-label thing))))

(defun misc-handler (thing)
  (html-out (str thing)))

(defun list-handler (thing)
  (if (null (remove-if #'consp thing)) ; Not guaranteed to be an alist!
      (funcall *->html-alist-handler* thing)
      (html-out
        (:div
         (dolist (th thing)
           (htm (:div (funcall *->html-main-handler* th))))))))

(defun hash-handler (thing)
  (html-out
    (:table
     (maphash
      (lambda (k v)
        (html-out
          (:tr (:td
                (funcall *->html-main-handler* k))
               (:td
                (funcall *->html-main-handler* v)))))
      thing))))

(defun alist-handler (thing)
  (html-out
    (:table
     (dolist (itm thing)
       (htm (:tr (:td
                  (funcall *->html-main-handler* (car itm)))
                 (:td
                  (funcall *->html-main-handler* (cdr itm)))))))))

(setf *->html-list-handler* #'list-handler)
(setf *->html-hash-handler* #'hash-handler)
(setf *->html-alist-handler* #'alist-handler)
(setf *->html-symbol-handler* #'symbol-handler)
(setf *->html-misc-handler* #'misc-handler)
(setf *->html-main-handler* #'->html)

