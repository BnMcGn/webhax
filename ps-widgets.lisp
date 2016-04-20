;;;; ps-widgets.lisp

(in-package #:webhax-widgets)

(defun ps-widgets ()

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
                  (prop options))))))

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

  (def-component widgi-wrap
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
             (funcall callback (@ state data)))
            (:edit
             ;;FIXME: Client-side validation not implemented.
             (let ((value (@ action value))
                   (errmsg nil)) ;temporary)
               (if errmsg
                   (deep-set-copy
                    state `(errors ,(@ action name)) errmsg)
                   (deep-set-copy
                    (deep-set-copy
                     state `(data ,(@ action name) value) value)
                    `(errors ,(@ action name)) nil)))))
          (create
           :data data
           :fieldspecs fieldspecs ;; FIXME: When should validators be created?
           :errors (let ((res (create))
                     (do-keyvalue (k v data) ;; borrow keys from data.
                       (setf (getprop res k) nil))
                     res))))))

  (def-component webhax-form-toplevel
      (let ((fspecs (prop fieldspecs))
            (data (prop data))
            (callback (prop callback))
            (errors (prop errors)))
        (psx
         (:form
          (collecting
           (do-keyvalue (name fspec fspecs)
             (collect
                 (psx
                  (:widgi-wrap
                   :id name :description (@ fspec description) :name name
                   :error (getprop errors name) :nullok (@ fspec config nullok)
                   (:widgi-select
                    :widget (@ fspec widget) :options (@ fspec options)
                    :value (getprop data name) :name name :dispatch callback
                    :... (@ fspec config)))))))
          (:input :type "button" :value "Submit"
                  :on-click (lambda () (funcall callback :type :submit)))))))

  (defun webhax-form-element (fieldspecs data callback)
    (let ((store (chain -redux
                        (create-store (webhax-form-dispatcher
                                       fieldspecs data callback))))
          (provider (@ -react-redux -provider))
          (app
           (funcall
            (chain -react-redux
                   (connect (lambda (stuff) (shallow-copy stuff))
                            (lambda (dispatch) (create :dispatch dispatch))))
            webhax-form-toplevel)))
      (create-element provider (create :store store) (create-element app))))


