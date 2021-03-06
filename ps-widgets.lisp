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
         (:input :type "text" :class "webhax-widget"
                 :key 1
                 :id (prop name)
                 ;:default-value (prop value)
                 :value (or (prop value) "")
                 :on-change (event-dispatcher (prop name) (prop dispatch)))))

    (def-component ww-yesno
        (let ((value (boolify (prop value))))
          (psx
           (:span
            :class "webhax-widget"
            :id (prop name)
            ;;FIXME: labels aren't adjustable
            "Yes"
            (:input :type "radio" :key 1
                    :name (prop name) :value "true"
                    :on-change (event-dispatcher (prop name) (prop dispatch))
                    :... (if value (create :checked "checked") (create)))
            "No"
            (:input :type "radio" :key 2
                    :name (prop name) :value "false"
                    :on-change (event-dispatcher (prop name) (prop dispatch))
                    :... (if  (and (not value) (prop prefilled))
                           (create :checked "checked") (create)))))))

    (def-component ww-pickone
        (let ((props (@ this props)))
          (psx
           (:span
            :class "webhax-widget"
            :id (prop name)
            :key 1
            (mapcar (lambda (option)
                      (let ((label (elt option 1))
                            (value (elt option 0))
                            (valstr (-string (elt option 0))))
                        (psx
                         (:label ;;:for value
                          :key (unique-id)
                          (:input :type "radio" :name (@ props name)
                                  :key 1
                                  :id value
                                  :on-change
                                  (event-dispatcher
                                   (@ props name) (@ props dispatch))
                                  :value value
                                  :... (if (eq (-string (@ props value))
                                                 valstr)
                                           (create :checked "checked")
                                           (create)))
                          label))))
                    (prop options))))))

    (def-component ww-pickone-long
        (let ((props (@ this props)))
          (psx
           (:select :class "webhax-widget"
            :key 1
            :name (prop name)
            :id (prop name)
            :value (or (prop value) "")
            :on-change (event-dispatcher (prop name) (prop dispatch))
            (mapcar
             (lambda (option)
               (let ((value (elt option 0))
                     (label (elt option 1)))
                 (psx
                  (:option :value value :key (unique-id) label))))
             (prop options))))))

    (def-component ww-textentry
        (psx
         (:textarea :rows 5 :cols 40 :class "webhax-widget"
                    :value (or (prop value) "")
                    :id (prop name)
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
           (:select :multiple "multiple" :class "webhax-widget"
                    :id (prop name)
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
          (:span :class "webhax-label" :key 1
                 (or (prop description) (capitalize-first (ensure-string (prop name))))
                 (unless (prop nullok)
                   (psx
                    (:span :style :key 3 (create "fontColor" "red") " *"))))
          (prop children)
          (when (prop error)
            (psx (:span :class "webhax-error" :key 2 (prop error)))))))

    (def-component widgi-select
        (create-element
         (getprop
          (create :string ww-simple :integer ww-simple :boolean ww-yesno
                  :pickone ww-pickone :picksome ww-picksome
                  "pickoneLong" ww-pickone-long :yesno ww-yesno :textentry ww-textentry)
          (prop widget))
         (@ this props)
         nil))
    ;;FIXME: type checking currently unavailable
      ;;prop-types
      ;;(create
      ;; widget (@ -react -prop-types string is-required)))

    (defun webhax-form-dispatcher (fieldspecs data callback)
      (lambda (state action)
        (if state
            (case (@ action type)
              (:submit
               (progn
                 (when (eq (typeof callback) "function")
                   (funcall callback (@ state data)))
                 state))
              (:update ;From server/webhax-form-connector
               (copy-merge-objects state (@ action data)))
              (:edit
               ;;FIXME: Client-side validation not implemented.
               (let ((data (if (@ action data)
                               (@ action data)
                               (let ((dat (create)))
                                 (setf (getprop dat (@ action name))
                                       (@ action value))
                                 dat)))
                     (errmsg nil)) ;temporary
                 (if errmsg
                     (safe-set-copy state 'errors errmsg)
                     (safe-set-copy state 'data
                                    (copy-merge-objects (@ state data) data))))))
            (create
             :data
             (let ((res (create)))
               (do-window ((k v) fieldspecs :step 2)
                 (setf (getprop res k)
                       (if (not (eq (getprop data k) undefined))
                           (getprop data k)
                           null)))
               res)
             :fieldspecs fieldspecs ;; FIXME: When should validators be created?
             :success nil
             :errors (let ((res (create)))
                       (do-window ((k v) fieldspecs :step 2)
                         ;; borrow keys from data.
                         (setf (getprop res k) nil))
                       res)))))

    (def-component webhax-simple-form
        (let ((dispatch (prop dispatch)))
          (psx
           (:form
            :key "form0"
            (prop children)
            (:input :type "button" :value "Submit" :key "form1"
                    :on-click (lambda ()
                                (funcall dispatch
                                         (create :type :submit))))))))

    (def-component webhax-widget-wrapper-builder
        (let* ((wrapwidget (prop wrapwidget))
               (name (prop name))
               (data (prop data))
               (corewidget
                (psx
                 (:widgi-select
                  :id (strcat "inner-" name)
                  :key name
                  :widget (prop fieldspec widget)
                  :options (prop fieldspec options)
                  :value (getprop data name) :name name
                  :prefilled (chain data (has-own-property name))
                  :error (getprop (prop errors) name)
                  :formdata data
                  :dispatch (prop dispatch)
                  :... (or (prop fieldspec config) (create))))))
          (if wrapwidget
              (create-element
               wrapwidget
               (create
                :id name :description (prop fieldspec description)
                :name name :key name
                :error (getprop (prop errors) name)
                :nullok (prop fieldspec nullok))
               corewidget)
              corewidget)))

    (def-component webhax-form-toplevel
        (let ((fspecs (prop fieldspecs))
              (data (prop prefill))
              (dispatch (prop dispatch))
              (errors (prop errors))
              (subform (or (prop layout) webhax-simple-form))
              (wrapwidget (if (prop wrapwidget)
                               (prop wrapwidget)
                               (if (eql false (prop wrapwidget))
                                   nil
                                   widgi-wrap-simple))))
          (psx (:subform
                :key 1
                :formdata data :dispatch dispatch :fieldspecs fspecs
                :errors errors
                :success (prop success)
                (collecting
                    (let ((counter 0))
                      (do-window ((name fspec) fspecs :step 2)
                        (if (chain fspec (has-own-property :prebuilt))
                            (collect (@ fspec :prebuilt))
                            (collect
                                (psx
                                 (:webhax-widget-wrapper-builder
                                  :key (incf counter)
                                  :name name :errors errors
                                  :dispatch dispatch :fieldspec fspec
                                  :data data :wrapwidget wrapwidget)))))))))))

    (def-component webhax-form
        (let ((provider (@ -react-redux -provider))
              (store
               (chain -redux (create-store (webhax-form-dispatcher
                                            (prop fieldspecs) (prop data)
                                            (prop callback)))))
              (app
               (funcall
                (chain -react-redux
                       (connect (lambda (stuff own-props)
                                  (copy-merge-all own-props stuff))
                                (lambda (dispatch)
                                  (create :dispatch dispatch))))
                webhax-form-connector)))
          (psx
           (:provider
            :store store
            (:app :... (@ this props))))))
      #|
      prop-types
      (create :fieldspecs
              (chain -react -prop-types
                     (array-of
                      (chain -react -prop-types
                             (one-of-type
                              (list (chain -react -prop-types string)
                                    (chain -react -prop-types object))))))))
      |#

    ;;FIXME: Should add something to tell redux that we are waiting on JSON
    (def-component webhax-form-connector
        (psx (:webhax-form-toplevel
              :... (@ this props)
              :prefill (prop data)
              :dispatch (@ this dispatch)))
      dispatch
      (lambda (action)
        (if (and (eq (@ action type) :submit) (prop validation-url))
            (ps-gadgets:json-post-bind (res (prop validation-url) (prop data))
              (funcall (prop dispatch) (create :type :update :data res)))
            (funcall (prop dispatch) action))))

    (defun webhax-form-element (fieldspecs data callback)
      (psx
       (:webhax-form :fieldspecs fieldspecs :prefill data :callback callback)))

  ))
