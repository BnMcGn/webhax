
(in-package #:webhax-test)

(in-suite webhax-test)

(defparameter result nil)

(multiple-value-bind (askman names)
    (webhax::t-ask
      (q some "Are there any?" :yesno)
      (if (a some)
	  (q enough? "How many?" :pickone :source '(3 5 6 18))
	  (q want "Why not?" :string))
      (and (q are :yesno)
	   (q you :yesno)
	   (q sure? :yesno))
      :target (lambda (answ)
		(maphash (lambda (k v)
			   (setf (gethash k answ) (car v))) answ)
		(setf result answ)))
  (funcall askman :update nil)
  (funcall askman :update `((,(car names) . "true")))
  (funcall askman :update `((,(second names) . "5")))
  (funcall askman :update `((,(third names) . "true")))
  (funcall askman :update `((,(fourth names) . "false"))))

(test t-ask
  (is (hash-table-p result))
  (is (= 5 (gethash 'enough? result))))

