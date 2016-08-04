;;;; server.lisp

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

(defun %prep-q-dispatch (q name prev-val)
  (list (cons :next
              (plist-hash-table (list name
                                      (plist-hash-table
                                       (list :default
                                             (if prev-val prev-val
                                                 (aif2only
                                                  (keyword-value :default q)
                                                  it nil)))))))))

(defun %unquote-q (q)
  "Decide what portions of the q should not be executed."
  (labels ((unquote-valspec (vs)
             (optima:match vs
               ((and (type keyword) val) val)
               ((list* val rest)
                `(list ,val ,@rest)))))
    (optima:match q
     ((list* 'q (and (type symbol) name) (and (type string) description)
             valtype rest)
      `(list 'q ',name ,description ,(unquote-valspec valtype) ,@rest))
     ((list* 'q (and (type symbol) name) valtype rest)
      `(list 'q ',name ,(unquote-valspec valtype) ,@rest))
     (_ (error "Malformed q clause")))))

(defun %dispatch-keys (disp)
  (hash-table-keys (assoc-cdr :next disp)))

(defun %process-form (code)
  `(%form-display
    (collecting
        (labels ((display (name)
                   (collect name)))
          ,@code))))

(defun %ask-proc-finish (askstore target finish)
  "*ask-finish* is assumed to be returning page-mod instructions for the FE. If
it is set to nil, then *ask-target* is assumed to be returning page-mod."
  (let ((res (funcall-in-macro
              (or target #'identity)
              (all-answers askstore :translate t))))
    (if finish
        (funcall-in-macro finish (all-answers askstore :translate t))
        res)))

(defun %ask-proc-exit/server (exit-body)
  ;;Retain only server portions of the exit clause
  (mapcan
   (lambda (x)
     (when
         (and (listp x) (eq (car x) 'server))
       (cdr x)))
   exit-body))

(defun %insert-prefills (askstore prefills)
  (dolist (pr prefills)
    (load-prefills askstore pr)))

(defun create-ask-manager (code qs names)
  (with-gensyms (continuations dispatch stor destroy display-queue)
    `(let ((,continuations nil)
           (,dispatch nil)
           (,stor (make-instance 'ask-store
                                 :q-clauses (list ,@(mapcar #'%unquote-q qs))
                                 :names ',names))
           (,destroy nil)
           (,display-queue nil))
       (%insert-prefills ,stor (list ,@*ask-prefills*))
       (flet ((answer (&rest params)
                (apply #'answer ,stor params))
              (exists-answer (&rest params)
                (apply #'exists-answer ,stor params))
              (answers ()
                ;;FIXME: Verify that :strip t is correct here.
                (all-answers ,stor :strip t :translate t))
              (%display-enqueue (name)
                (push name ,display-queue)))
         (macrolet ((a (itm)
                      `(answer ',itm :translate t))
                    (form (&body body)
                      (%process-form body))
                    (done (&body body)
                      `(prog1
                           ,@(%ask-proc-exit/server body)
                         (remove-ask-manager *ask-formname*))))
           (cl-cont:with-call/cc
             (labels
                 ((display (name)
                    (setf ,dispatch
                          (dispatch-for-names
                           ,stor
                           (nreverse (cons name ,display-queue))))
                    (setf ,display-queue nil)
                    (cl-cont:let/cc k (push k ,continuations))
                    (answer name)) ;What does this do?
                  (%form-display (namelist)
                    (setf ,dispatch
                          (dispatch-for-names
                           ,stor
                           (concatenate 'list (nreverse ,display-queue)
                                        namelist)))
                    (setf ,display-queue nil)
                    (cl-cont:let/cc k (push k ,continuations))
                    :???))
               (declare (ignorable (function display) (function %form-display)))
               ,@code
               (setf ,dispatch
                     (list (cons :success (%ask-proc-finish
                                           ,stor ,*ask-target* ,*ask-finish*))))
               (setf ,destroy t)))))
       (lambda (command data)
         (case command
           (:update
            (aif (add-input ,stor data)
                 (progn
                   `((:error . ,it)))
                 (progn
                   (when (with-any/all/none
                           (dolist (keyname (%dispatch-keys ,dispatch))
                             (all (exists-answer ,stor keyname))) t)
                     (funcall (car ,continuations)))
                   (when ,destroy
                     (remove-ask-manager *ask-formname*))
                   ,dispatch)))
           (:get-store
            ,stor)
           (:destroy
            (remove-ask-manager *ask-formname*))
           (:back
            (error "Not implemented"))
           (otherwise
            (error "Not implemented")))))))

;;;FIXME: Should have some way of cleaning old askstores from session/askdata
(defun register-ask-manager (aman &key (session *session*))
  (unless (gethash :askdata session)
    (setf (gethash :askdata session) (make-hash-table :test #'equal)))
  (let* ((stor (gethash :askdata session))
         (id (loop for idx = (create-numbered-name :ask)
                while (gethash idx stor)
                finally (return idx))))
    (setf (gethash id stor) aman)
    id))

(defun call-ask-manager (aname command data &key (session *session*))
  (let ((askdata (gethash :askdata session)))
    (unless (hash-table-p askdata)
      (error "Askdata not found."))
    (aif (gethash aname askdata)
         (let ((*ask-formname* aname))
           (funcall it command data))
         (error
          (format nil "Form ~a not found in askdata." aname)))))

(eval-always
  (defun remove-ask-manager (aname &key (session *session*))
    (if (not (hash-table-p session))
        (warn "Session not found")
        (let ((askdata (gethash :askdata session)))
          (if (not (hash-table-p askdata))
              (warn "Askdata not found")
              (if (not (gethash aname askdata))
                  (warn "Ask-manager not found in session!")
                  (remhash aname askdata)))))))

(defclass ask-store ()
  ((stor
    :initarg :storage
    :initform (make-hash-table))
   (qs
    :initarg :q-clauses)
   (names
    :initarg :names)
   validators
   (prefill-stor
    :initform (make-hash-table))))

(defmethod initialize-instance :after ((stor ask-store) &key)
  (with-slots (qs names validators) stor
    (setf validators
          (collecting-hash-table (:mode :replace :test #'equal)
            (loop for q in qs
               for n in names
               do (collect n (%q-validator q)))))))

(defgeneric nullok-p (stor name))
(defmethod nullok-p (astor name)
  (with-slots (validators) astor
    (getf (gethash name validators) :nullok)))

(defgeneric multiple-p (stor name))
(defmethod multiple-p (astor name)
  (with-slots (validators) astor
    (getf (gethash name validators) :multiple)))

(defgeneric add-input (stor input)
  (:documentation
   "Add input from a web source (alist format) to a storage object. Returns
   error messages if any input items don't validate."))

(defmethod add-input ((astor ask-store) input)
  (with-slots (validators names stor) astor
    (multiple-value-bind (good errors)
        (with-collectors (g< e<)
          (dolist (n names)
            (let ((value (if (multiple-p astor n)
                             `((,n . ,(assoc-all n input
                                                 :test #'eq-symb-multiple)))
                             (assoc n input :test #'equal))))
              (when value
                (multiple-value-bind (val sig)
                    (funcall
                     (getf (gethash n validators) :compiled-validator)
                     (cdr value))
                  (if sig
                      (g< (cons n val))
                      (e< (cons n val))))))))
      (if errors
          errors
          (dolist (itm good)
            (setf (gethash (first itm) stor) (cons (cdr itm) t)))))))

(defgeneric translation-table (stor &key reversed))
(defmethod translation-table (stor &key reversed)
  (with-slots (names qs) stor
    (if reversed
        (pairlis names (mapcar #'second qs))
        (pairlis (mapcar #'second qs) names))))

(defgeneric load-prefills (stor dict &key test)
  (:documentation "For loading alists or hash-tables into the prefill store. No validation."))
(defmethod load-prefills (astor dict &key (test #'eq-symb))
  (with-slots (qs names prefill-stor) astor
    (let ((table (translation-table astor)))
      (if (hash-table-p dict)
          (do-hash-table (k v dict)
            (setf (gethash (assoc-cdr k table :test test) prefill-stor) v))
          (do-alist (k v dict)
            (setf (gethash (assoc-cdr k table :test test) prefill-stor) v))))))

(defgeneric translate-key (stor key)
  (:documentation "Given the original symbol supplied by the user, return the
applicable numbered key."))
(defmethod translate-key ((astor ask-store) key)
  (assoc-cdr key (translation-table astor)))

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

(defgeneric calculate-prefill (astor key))
(defmethod calculate-prefill (astor key)
  (with-slots (names qs prefill-stor) astor
    (if (exists-answer astor key)
        (car (answer astor key))
        (multiple-value-bind (val sig)
            (keyword-value :default (assoc-cdr key (pairlis names qs)))
          (if sig
              (values val t)
              (aif2only (gethash key prefill-stor)
                        (values it t)
                        (values nil nil)))))))

;;;FIXME: should all-answers return cleaned up stuff?
(defgeneric all-answers (stor &key translate strip))
(defmethod all-answers ((astor ask-store) &key translate strip)
  (with-slots (qs names stor) astor
    (funcall
     (if strip
         (lambda (x)
           (collecting-hash-table (:mode :replace)
             (maphash (lambda (k v) (collect k (car v))) x)))
         #'identity)
     (if translate
         (let ((trans-table
                (pairlis names (mapcar #'second qs))))
           (collecting-hash-table (:mode :replace)
             (maphash
              (lambda (k v)
                (collect (assoc-cdr k trans-table) v))
              stor)))
         stor))))

(defgeneric dispatch-for-names (stor namelist)
  (:documentation
   "Create a dispatch for conversion to JSON for the fields in namelist"))
(defmethod dispatch-for-names ((astor ask-store) namelist)
  (with-slots (qs stor names) astor
    (list
     (cons :next
           (collecting-hash-table (:mode :replace)
             (dolist (n namelist)
               (collect n
                 (plist-hash-table
                  `(,@(multiple-value-bind (val sig)
                                           (calculate-prefill astor n)
                                           (when sig
                                             (list :default val))))))))))))
