;;;; ps-widgets.lisp
(in-package #:cl-user)

(defpackage #:webhax-widgets
  (:use #:cl #:parenscript #:ps-gadgets #:cl-react)
  (:export
   #:ps-widgets))

(in-package #:webhax-widgets)

(defun ps-widgets ()
  (ps
    (defun event-dispatcher (name dispatch-func)
      (lambda (ev)
        (funcall
         dispatch-func
         (create :type :edit :name name :value (@ ev target value)))))

    (def-component ww-simple
        (psx
         (:input :type "text"
                 :default-value (prop value)
                 :on-change (event-dispatcher (prop name) (prop dispatch)))))

    (def-component ww-yesno
        (psx
         (:span
          (:input :type "radio"
                  :name (prop name) :value "true"
                  :on-change (event-dispatcher (prop name) (prop dispatch))
                  :... (when (prop value) (create :checked "checked")))
          (:input :type "radio"
                  :name (prop name) :value "false"
                  :on-change (event-dispatcher (prop name) (prop dispatch))
                  :... (unless (prop value) (create :checked "checked"))))))

    (def-component ww-pickone
        (let (props (@ this props))
          (psx
           (:span
            (mapcar (lambda (option)
                      (psx
                       (:label :for (@ option value)
                               (:input :type "radio" :name (@ props name)
                                       :id (@ option value)
                                       :on-change 
                                       (event-dispatcher
                                        (@ props name) (@ props dispatch))
                                       :value (@ option value)
                                       :... (when (eq (@ props value)
                                                      (@ option value))
                                              (create :checked "checked"))
                                       (@ option label))))
                      (prop options)))))))

    (def-component ww-textentry
        (psx
         (:textarea :rows 5 :cols 40
                    :value (prop value)
                    :on-change (event-dispatcher (prop name) (prop dispatch)))))


    ;;FIXME: picksome should be able to communicate option order. Maybe should
    ;; send whole list rather than single selection on change.
    ;;FIXME: replace me.
    (def-component ww-picksome
        (let ((props (@ this props))
              (callback
               (lambda (ev)
                 (funcall (prop action) (prop form-name) (prop name)
                          (@ ev target value)))))
          (psx
           (:select :multiple "multiple"
                    (mapcar
                     (lambda (option)
                       (psx
                        (:option :label (@ option label)
                                 :on-change callback
                                 :... (when (member (@ option value)
                                                    (@ props value))
                                        (create :selected "selected"))
                                 (@ option value)))))))))

    (def-component widgi-wrap-simple
        (psx
         (:div
          (:span :class "webhax-label" (or (prop description) (prop name))
                 (unless (prop nullok)
                   (psx (:span :style (create "font-color" "red") " *"))))
          (prop children)
          (when (prop error)
            (:span (prop error))))))

    (def-component widgi-select
        (create-element
         (getprop
          (create :string ww-simple :integer ww-simple :boolean ww-yesno
                  :pickone ww-pickone :picksome ww-picksome)
          (prop widget))
         (@ this props)
         nil))

    (defun webhax-form-dispatcher (fieldspecs data callback)
      (lambda (state action)
        (if state
            (case (@ action type)
              (:submit
               (progn
                 (funcall callback (@ state data))
                 state))
              (:edit
               ;;FIXME: Client-side validation not implemented.
               (let ((value (@ action value))
                     (errmsg nil)) ;temporary)
                 (if errmsg
                     (deep-set-copy
                      state (list 'errors (@ action name)) errmsg)
                     (deep-set-copy
                      (deep-set-copy
                       state (list 'data (@ action name)) value)
                      (list 'errors (@ action name)) nil)))))
            (create
             :data
             (let ((res (create)))
               (do-keyvalue (k v fieldspecs)
                 (setf (getprop res k)
                       (if (not (eq (getprop data k) undefined))
                           (getprop data k)
                           null)))
               res)
             :fieldspecs fieldspecs ;; FIXME: When should validators be created?
             :errors (let ((res (create)))
                       (do-keyvalue (k v fieldspecs) ;; borrow keys from data.
                         (setf (getprop res k) nil))
                       res)))))

    (def-component webhax-simple-form
        (let ((dispatch (prop dispatch)))
          (psx
          (:form
           (prop children)
           (:input :type "button" :value "Submit"
                   :on-click (lambda ()
                               (funcall dispatch
                                        (create :type :submit))))))))

    (def-component webhax-form-toplevel
        (let ((fspecs (prop fieldspecs))
              (data (prop data))
              (dispatch (prop dispatch))
              (errors (prop errors))
              (subform (or (prop layout) webhax-simple-form))
              (wrapwidget (if (prop wrap-widget)
                               (prop wrap-widget)
                               (if (eql false (prop wrap-widget))
                                   nil
                                   widgi-wrap-simple))))
          (psx (:subform
                :formdata data :dispatch dispatch :fieldspecs fspecs
                :errors errors
                (collecting
                    (if wrapwidget
                        (do-keyvalue (name fspec fspecs)
                          (collect
                              (psx
                               (:wrapwidget
                                :id name :description (@ fspec description)
                                :name name
                                :error (getprop errors name)
                                :nullok (@ fspec nullok)
                                (:widgi-select
                                 :widget (@ fspec widget)
                                 :options (@ fspec options)
                                 :value (getprop data name) :name name
                                 :formdata data
                                 :dispatch dispatch
                                 :... (@ fspec config))))))
                        (do-keyvalue (name fspec fspecs)
                          (collect
                              (psx
                               (:widgi-select
                                :id (strcat "inner-" name)
                                :widget (@ fspec widget)
                                :options (@ fspec options)
                                :value (getprop data name) :name name
                                :formdata data
                                :dispatch dispatch
                                :... (@ fspec config)))))))))))

    (def-component webhax-form
        (let ((provider (@ -react-redux -provider)))
          (psx
           (:provider
            (funcall
             (chain -react-redux
                    (connect (lambda (stuff own-props)
                               (copy-merge-all stuff own-props))
                             (lambda (dispatch)
                               (create :dispatch dispatch))))))))
      initial-state
      (create :store
              (chain -redux (create-store (webhax-form-dispatcher
                                           (prop fieldspecs) (prop data)
                                           (prop callback))))))

    (defun webhax-form-element (fieldspecs data callback)
      (psx
       (:webhax-form :fieldspecs fieldspecs :data data :callback callback)))

  ))
