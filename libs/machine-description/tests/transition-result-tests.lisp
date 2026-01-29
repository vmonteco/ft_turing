(in-package :machine-description/tests)

(def-suite transition-tests :in machine-description-tests)
(in-suite transition-tests)

(test transition-result-instantiation-tests
	  (let ((tr (make-instance 'transition-result
							   :to-state :|state|
							   :to-char #\0
							   :action :left)))
		(is (equal :|state| (to-state tr)))
		(is (equal #\0 (to-char tr)))
		(is (equal :left (action tr))))
	  (let ((tr (make-instance 'transition-result
							   :to-state :|state|
							   :to-char #\0
							   :action :right)))
		(is (equal :right (action tr))))
	  (signals machine-description::invalid-action-error
			   (make-instance 'transition-result
							  :to-state :|state|
							  :to-char #\0
							  :action :foo)))

(test transition-result-equal-tests
	  (is (transition-result-equal (make-instance 'transition-result
												  :to-state :state
												  :to-char #\0
												  :action :left)
								   (make-instance 'transition-result
												  :to-state :state
												  :to-char #\0
												  :action :left)))
	  (is (transition-result-equal (make-instance 'transition-result
												  :to-state :state
												  :to-char #\1
												  :action :right)
								   (make-instance 'transition-result
												  :to-state :state
												  :to-char #\1
												  :action :right)))
	  (is-false (transition-result-equal (make-instance 'transition-result
														:to-state :state
														:to-char #\0
														:action :right)
										 (make-instance 'transition-result
														:to-state :state
														:to-char #\0
														:action :left)))
	  (is-false (transition-result-equal (make-instance 'transition-result
														:to-state :state
														:to-char #\0
														:action :right)
										 (make-instance 'transition-result
														:to-state :state
														:to-char #\1
														:action :right)))
	  (is-false (transition-result-equal (make-instance 'transition-result
   														:to-state :state0
														:to-char #\0
														:action :right)
										 (make-instance 'transition-result
														:to-state :state1
														:to-char #\0
														:action :right))))
