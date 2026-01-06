(in-package :machine-description/tests)

(def-suite transition-tests :in machine-description-tests)
(in-suite transition-tests)

(test transition-result-instantiation-tests
	  (let ((tr (make-instance 'transition-result
							   :to-state :state
							   :to-char #\0
							   :action :left)))
		(is (equal (to-state tr) :state))
		(is (equal (to-char tr) #\0))
		(is (equal (action tr) :left)))
	  (let ((tr (make-instance 'transition-result
							   :to-state :state
							   :to-char #\0
							   :action :right)))
		(is (equal (action tr) :right)))
	  (signals machine-description::invalid-action-error
			   (make-instance 'transition-result
							  :to-state :state
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
