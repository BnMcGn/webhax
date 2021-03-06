;;;; server.lisp

(in-package #:webhax)

;;;Server side and other bits of the DSL

(defun displayable-p (form)
  "Checks a form to see if it is an Ask displayable/needs a label."
  (and (consp form) (member (car form) '(q client client/react)
                            :test #'string-equal*)))

(defun %%displayable/server (displ name)
  (optima:match displ
    ((cons 'q _) `(display ,name))
    ((cons 'client _) `(%display-enqueue ,name))
    ((cons 'client/react _) `(%display-enqueue ,name))))

(defun guess-numbered-name (namesym)
  ;;FIXME: Guessing whether a displayable needs a name might be a little
  ;;too clever. Reconsider this someday.
  (create-numbered-name (if (symbolp namesym) namesym :autoname)))

(defun process-ask-code (code)
  (let (res)
    (multiple-value-bind (syms qs)
        (cl-utilities:with-collectors (syms< qs<)
          (labels ((treeproc (tree)
                     (if (atom tree)
                         tree
                         (if (displayable-p tree)
                             (let ((isym
                                    (guess-numbered-name (second tree))))
                               (syms< isym)
                               (qs< tree)
                               (%%displayable/server tree isym))
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
                                                 (multiple-value-bind (val sig)
                                                     (proto:keyword-value :default q)
                                                   (when sig val))))))))))

(defun %%unquote-q (q)
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
      `(list 'q ',name ,(capitalize-first name) ,(unquote-valspec valtype)
             ,@rest))
     ((cons 'client _) nil)
     ((cons 'client/react _) nil)
     (_ (error "Malformed q clause")))))

(defun %%store-gen (qs names &aux (qs (mapcar #'%%unquote-q qs)))
  (multiple-value-bind (qs+ names+)
      (cl-utilities:with-collectors (qs< names<)
        (loop for q in qs for n in names
           when q do (progn (qs< q) (names< n))))
    `(make-instance 'ask-store
                    :q-clauses (list ,@qs+)
                    :names ',names+)))

(defun %dispatch-keys (disp)
  (hash-table-keys (assoc-cdr :next disp)))

(defun %process-form (code)
  `(%form-display
    (cl-utilities:collecting
        (labels ((display (name)
                   (cl-utilities:collect name)))
          ,@code))))

(defun %%ask-proc-exit/server (exit-body)
  ;;FIXME: Should (server...) contain an implicit progn?
  (mapcan
   (lambda (x)
     (if (and (listp x) (eq (car x) 'server))
         (cdr x)
         (list x)))
   exit-body))

(defun %insert-prefills (askstore prefills)
  (dolist (pr prefills)
    (load-prefills askstore pr)))

(defun create-ask-manager (code qs names)
  (with-gensyms (continuations dispatch stor destroy display-queue)
    `(let ((,continuations nil)
           (,dispatch nil)
           (,stor ,(%%store-gen qs names))
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
                (push name ,display-queue))
              (%display-queue-contents ()
                ,display-queue))
         (declare (ignorable #'answers #'%display-queue-contents))
         (macrolet ((a (itm)
                      `(answer ',itm :translate t))
                    (form (&body body)
                      (%process-form body))
                    (done (&body body)
                      `(prog1
                           ,@(%%ask-proc-exit/server body)
                         (when (%display-queue-contents)
                           (%send-display-queue))
                         (remove-ask-manager *ask-formname*)))
                    (display (name)
                      `(progn (%display-enqueue ,name)
                              (%send-display-queue)
                              (answer ,name)))
                    (%form-display (namelist)
                      `(progn (mapc #'%display-enqueue ,namelist)
                              (%send-display-queue)
                              :???)))
           (cl-cont:with-call/cc
             (labels
                 ((%send-display-queue ()
                    (setf ,dispatch
                          (dispatch-for-names
                           ,stor
                           (nreverse ,display-queue)))
                    (setf ,display-queue nil)
                    (cl-cont:let/cc k (push k ,continuations))))
               ,@code
               (setf ,destroy t)))))
       (lambda (command data)
         (case command
           (:update
            (alexandria:if-let ((inp (add-input ,stor data)))
                 (progn
                   `((:errors . ,inp)))
                 (progn
                   (when (every (lambda (itm) (exists-answer ,stor itm)) (%dispatch-keys ,dispatch))
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
    (if-let ((man (gethash aname askdata)))
      (let ((*ask-formname* aname))
        ;;(print command)
        ;;(print data)
        ;;(print "response:")
        (funcall man command data))
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
          (hu:collecting-hash-table (:mode :replace :test #'equal)
            (loop for q in qs
               for n in names
               do (hu:collect n (%q-validator q)))))))

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
        (cl-utilities:with-collectors (g< e<)
          (dolist (n names)
            (let ((value (if (multiple-p astor n)
                             `((,n . ,(assoc-all
                                       n input
                                       :test #'string-equal-multiple)))
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
  (:documentation "For loading alists, plists or hash-tables into the prefill store. No validation."))
(defmethod load-prefills (astor dict &key (test #'string-equal))
  (with-slots (qs names prefill-stor) astor
    (let ((table (translation-table astor)))
      (cond ((hash-table-p dict)
             (do-hash-table (k v dict)
               (setf (gethash (assoc-cdr k table :test test) prefill-stor) v)))
            ((alist-p dict)
             (do-alist (k v dict)
               (setf (gethash (assoc-cdr k table :test test) prefill-stor) v)))
            (t
             (do-window ((k v) dict :size 2 :step 2)
               (setf
                (gethash (assoc-cdr k table :test test) prefill-stor) v)))))))

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
            ;;FIXME: getf?
            (proto:keyword-value :default (assoc-cdr key (pairlis names qs)))
          (if sig
              (values val t)
              (if (key-in-hash? key prefill-stor)
                  (values (gethash key prefill-stor) t)
                  (values nil nil)))))))

;;;FIXME: should all-answers return cleaned up stuff?
(defgeneric all-answers (stor &key translate strip))
(defmethod all-answers ((astor ask-store) &key translate strip)
  (with-slots (qs names stor) astor
    (funcall
     (if strip
         (lambda (x)
           (hu:collecting-hash-table (:mode :replace)
             (maphash (lambda (k v) (hu:collect k (car v))) x)))
         #'identity)
     (if translate
         (let ((trans-table
                (pairlis names (mapcar #'second qs))))
           (hu:collecting-hash-table (:mode :replace)
             (maphash
              (lambda (k v)
                (hu:collect (assoc-cdr k trans-table) v))
              stor)))
         stor))))

(defgeneric dispatch-for-names (stor namelist)
  (:documentation
   "Create a dispatch for conversion to JSON for the fields in namelist"))
(defmethod dispatch-for-names ((astor ask-store) namelist)
  (with-slots (qs stor names) astor
    (list
     (cons :next
           (hu:collecting-hash-table (:mode :replace)
             (dolist (n namelist)
               (hu:collect n
                 (plist-hash-table
                  `(,@(multiple-value-bind (val sig)
                                           (calculate-prefill astor n)
                                           (when sig
                                             (list :default val)))))))))
     (cons :ordering namelist))))
