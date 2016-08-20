;;;; client.lisp

(in-package #:webhax)

;;; Ask clientside/javascript stuff goes here.

(defun get-q-functype-symbol (q)
  (symb 'client- (get-q-type q)))

(defun get-q-postrender-symbol (q)
  (let ((sym (symb 'postrender- (get-q-type q))))
    ;FIXME: Admittedly demented way of doing things.
    (when (search (string-right-trim ";" (ps* sym)) (ps-widget-lib))
      sym)))

(defun generate-options-list (options)
  (if (null options)
      options
      (if (listp (car options))
	  options
	  (mapcar (lambda (x) (list x x)) options))))

;;;FIXME: rewrite
(defun generate-q-data (q)
  `(create
    :function ,(get-q-functype-symbol q)
    :label ,(make-q-label q)
    ,@(awhen (get-q-postrender-symbol q)
             (list :postrender it))
    ;;FIXME: prefill will be overwritten by gen. prefill
    ,@(aif2 (fetch-keyword :prefill q)
            (list :prefill it) (list :prefill nil))
    ,@(awhen2 (fetch-keyword :options-url q)
              (list :options-url it))
    ,@(awhen2 (fetch-keyword :options q)
              (list :options `(lisp-raw
                               (json:encode-json-to-string
                                (webhax::generate-options-list ,it)))))))

(defun generate-q-data (q)
  (let ((val (%q-validator q)))
    (list* 'create :real-name (%q-real-name q)
           (alist->plist
            (webhax-validate:prep-fieldspec-body-for-json val)))))

(defun prep-client-code-block (spec)
  `(create
    :executable (lambda () ,@(cdr spec))
    :is-element nil))

(defun prep-react-element (spec)
  `(create
    :executable (lambda () ,@(cdr spec))
    :is-element t))

(defun generate-client-data (symbols qs)
  `(create
    ,@(collecting
       (loop for s in symbols
          for q in qs
          do (progn
               (collect s)
               (collect (case (car q)
                          (q (generate-q-data q))
                          (client (prep-client-code-block q))
                          (client/react (prep-react-element q)))))))))

(defun ask-client-lib ()
  (ps

    (defun %ask-answers (data fieldspecs)
      (let ((res (-object)))
        (do-keyvalue (k fspec fieldspecs)
          (setf (getprop res (@ fspec :real-name))
                (getprop data k)))
        res))

    (def-component ask-main
        (psx
         (:ask-server-connection
          :commands (prop commands)
          :askname (prop askname)
          :info (prop info)
          ;;NOTE: prefill is not used by ask. prefills are put in the updates
          ;;from the server
          :prefill (prop prefill)
          :server-url (prop server-url))))

    (def-component ask-server-connection
        (psx
         (:ask-collection-layer
          :commands (state commands)
          :errors (state errors)
          :command-keys (state commands (keys))
          :info (prop info)
          :prefill (prop prefill)
          :dispatch (@ this call-server)))
      get-initial-state
      (lambda ()
        ;;commands prop switches nature here:
        ;;instead of containing subfields :next :errors, next is passed down as
        ;;commands and errors gets its own prop.
        (let ((commands (if (chain (prop commands) (has-own-property :next))
                            (prop commands next)
                            (throw "Ask: empty commands parameter")))
              (errors (if (chain (prop commands) (has-own-property :errors))
                          (prop commands errors)
                          (-object))))
          (create :commands commands :errors errors)))
      call-server
      (lambda (updates)
        (json-post-bind (commands (strcat (prop server-url) (prop askname)) updates)
          (when (chain commands (has-own-property :next))
            (set-state commands (@ commands next)))
          (when (chain commands (has-own-property :errors))
            (set-state errors (@ commands errors))))))

    (def-component ask-collection-layer
        (psx
         (:ask-displayable-manager
          :commands (prop commands)
          :info (prop info)
          :data (state data)
          :dispatch (@ this dispatch)
          :errors (prop errors)))
      get-initial-state
      (lambda ()
        (create :current (-object) :data (prop prefill)))
      dispatch
      (lambda (action) ;Can be replaced with redux dispatching.
         (case (@ action type)
           (:submit
            (funcall
             (prop dispatch) (state :current))
            (set-state :current (-object)))
           (:edit
            ;;FIXME: add client-side validation here?
            (if (member (@ action name) (prop command-keys))
                (progn
                  (set-state :current
                             (set-copy (state :current)
                                       (@ action name) (@ action value)))
                  (set-state :data
                             (copy-merge-all (state data) (state current))))
                (throw "Tried to write to a non-current Ask field")))
           (otherwise
            (throw "No such action!")))))

    (def-component ask-displayable-manager
        (psx
         (:webhax-form-toplevel
          :prefill (prop data) :errors (prop errors) :dispatch (prop dispatch)
          :fieldspecs
          (collecting
              (dolist (fspec (prop info))
                (if (chain fspec (has-own-property :executable))
                    (let ((res
                           (labels ((answers ()
                                      (%ask-answers (prop data) (prop info))))
                             (funcall (@ fspec :executable)))))
                      (when (@ fspec :is-element)
                        (collect (create :prebuilt res))))
                    (collect fspec)))))))

    ))

(defun ps-control-lib ()
  (ps
    (defun display-specified-controls (target commands data)
      (setf (getprop data :_curr-disp) (chain commands :next))
      (setf (chain target inner-h-t-m-l)
            (collecting-string
              (dolist (k (chain -object (keys (chain commands :next))))
                (collect (make-control k (getprop data k))))))
      (do-keyvalue (k v data)
        (if (chain v (has-own-property :postrender))
            (funcall (getprop v :postrender) k v))))

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

    (defun current-key-values (form &key (modify t))
      (let ((data (-object))
            (currkeys
             (chain -object
                    (keys (getprop document 'askdata form :_curr-disp)))))
        (dolist (k currkeys)
          (let ((v (getprop document 'askdata form k)))
            (if (chain v (has-own-property :current))
                (progn (setf (getprop data k) (@ v current))
                       (if modify
                           (progn (setf (@ v current-saved) (@ v current))
                                  (delete (@ v current)))))
                (if (chain v (has-own-property :default))
                    (setf (getprop data k) (@ v default))))
            (if (equal (getprop data k) null)
                (setf (getprop data k) ""))))
        data))

    (defun post-ask-update (form &optional (url (lisp *ask-control-url*)))
      (chain $ (get-j-s-o-n (+ url form) (current-key-values form)
                            (lambda (x)
                              (cond
                                ((chain x (has-own-property :next))
                                 (update-form-data
                                  (getprop (chain document askdata) form) x)
                                 (display-specified-controls
                                  (chain document (get-element-by-id form)
                                         first-element-child)
                                  x
                                  (getprop (chain document askdata) form)))
                                ((chain x (has-own-property :error))
                                 (alert
                                  (collecting-string
                                    (do-keyvalue (k v (getprop x :error))
                                      (collect
                                          (getprop (chain document askdata)
                                                   form k 'label))
                                      (collect ": ")
                                      (collect v)))))
                                ((chain x (has-own-property :success))
                                 (page-mod (chain x :success))))))))))

(defun ps-widget-lib ()
  (ps
    ;;FIXME: should decide between current and original default for fill.
    (defun make-control (name data)
      (who-ps-html
       (:div (:span (getprop data :label))
             (:span (funcall (getprop data :function) name data)))))

    (defmacro updatecode ()
      '(ps-inline (control-updated name this (@ this value))))

    ;;FIXME: default needs better handling.
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

    (defun client-textentry (name params)
      (ps-html
       ((:textarea :name name :rows 5 :cols 40
                   :value (getprop params :default)
                   :onchange (updatecode)))))

    (defun pick-get-selected (select)
      (chain ($ select)
             (children "[selected='selected']")
             (map (lambda (x y) (chain y value)))
             (get)))

    (defun client-picksome-long (name params)
      (let ((options
             (if (chain params (has-own-property :options))
                 (collecting-string
                   (dolist (opt (getprop params :options))
                     (collect (ps-html
                               ((:option :value (getprop opt 0)
                                         (or (getprop opt 2)
                                             (member
                                              (getprop opt 0)
                                              (or (getprop params :default)
                                                  (list))))
                                         :selected "selected")
                                (getprop opt 1))))))
                 "")))
        (ps-html
         ((:select :name name :multiple "multiple"
                   :onchange (ps-inline
                              (control-updated name this
                                               (pick-get-selected this))))
          options))))

    (defun postrender-picksome-long (name params)
      (chain ($ (+ "select[name='" name "']")) (select2)))

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

(defun ps-ask-lib ()
  (concatenate
   'string
   (ps-gadgets)
   (ps-control-lib)
   (ps-widget-lib)))

(defun %%ask-page-insert (nbody qs names)
  `(let* ((formname (register-ask-manager
                     ,(create-ask-manager nbody qs names)))
          (initial-display (call-ask-manager formname :update nil)))
     (mount-component (ask-main)
       :commands (lisp-raw (json:encode-json-alist-to-string initial-display))
       :askname (lisp formname)
       :info ,(generate-client-data names qs)
       :server-url  (lisp *ask-control-url*))))

(defun ask-page-insert (nbody qs names)
  "The part of an Ask that gets stuck into the web page, including the 
HTML form."
  `(let* ((formname (register-ask-manager
                     ,(create-ask-manager nbody qs names)))
          (initial-display (call-ask-manager formname :update nil)))
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
                 (update-form-data formdata initstate)
                 (setf (getprop (chain document askdata) formname) formdata)
                 (display-specified-controls
                  (chain document (get-element-by-id formname) first-element-child)
                  initstate
                  (getprop (chain document askdata) formname)))))))))
