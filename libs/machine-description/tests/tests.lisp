(in-package :machine-description/tests)

(def-suite machine-description-tests)
(in-suite machine-description-tests)

(test machine-description-instantiation
	  (let* ((t1 (make-instance 'transition-result
								:to-state :running
								:to-char #\0
								:action :right))
			 (t2 (make-instance 'transition-result
								:to-state :running
								:to-char #\1
								:action :right))
			 (t3 (make-instance 'transition-result
								:to-state :running
								:to-char #\.
								:action :right))
			 (running '((#\0 . t1) (#\1 . t2) (#\. t3)))
			 (transitions '(:running . running)))
		;; OK-ish instantiation
		(let ((md (make-instance 'machine-description
								 :name "foo"
								 :alphabet '(#\0 #\1 #\.)
								 :blank #\.
								 :states '(:running :error :success)
								 :initial-state :running
								 :finals '(:error :success)
								 :transitions transitions)))
		  (is (equal (name md) "foo"))
		  (is (and (subsetp (alphabet md) '(#\0 #\1 #\.))
				   (subsetp '(#\0 #\1 #\.) (alphabet md))))
		  (is (and (subsetp (states md) '(:running :error :success))
				   (subsetp '(:running :error :success) (states md))))
		  (is (equal (initial-state md) :running))
		  (is (and (subsetp (finals md) '(:error :success))
				   (subsetp '(:error :success) (finals md))))
		  (is (equalp transitions (transitions md))))

		;; Name issues
		;; Empty name
		(signals invalid-name
				 (make-instance 'machine-description
								:name ""
								:alphabet '(#\0 #\1 #\.)
								:blank #\.
								:states '(:running :error :success)
								:initial-state :running
								:finals '(:error :success)
								:transitions transitions))
	  
		;; Alphabet issues
		;; Empty alphabet (not a non-empty set)
		(signals invalid-alphabet
				 (make-instance 'machine-description
								:name "foo"
								:alphabet ()
								:blank #\.
								:states '(:running :error :success)
								:initial-state :running
								:finals '(:error :success)
								:transitions transitions))
		;; Alphabet with duplicates (not a non-empty set)
		(signals invalid-alphabet
				 (make-instance 'machine-description
								:name "foo"
								:alphabet '(#\0 #\0 #\.)
								:blank #\.
								:states '(:running :error :success)
								:initial-state :running
								:finals '(:error :success)
								:transitions transitions))
		;; Non-list alphabet (not a non-empty set)
		(signals invalid-alphabet
				 (make-instance 'machine-description
								:name "foo"
								:alphabet "bar."
								:blank #\.
								:states '(:running :error :success)
								:initial-state :running
								:finals '(:error :success)
								:transitions transitions))
		;; Alphabet with non-character elements
		(signals invalid-alphabet
				 (make-instance 'machine-description
								:name "foo"
								:alphabet '(1 2 3 #\.)
								:blank #\.
								:states '(:running :error :success)
								:initial-state :running
								:finals '(:error :success)
								:transitions transitions))

		;; Blank issue:
		(signals invalid-blank
				 (make-instance 'machine-description
								:name "foo"
								:alphabet '(#\0 #\1)
								:blank #\.
								:states '(:running :error :success)
								:initial-state :running
								:finals '(:error :success)
								:transitions transitions))
		
		;; States issue
		;; Empty states (not a non-empty set)
		(signals invalid-states
				 (make-instance 'machine-description
								:name "foo"
								:alphabet '(#\0 #\1 #\.)
								:blank #\.
								:states ()
								:initial-state :running
								:finals '(:error :success)
								:transitions transitions))
		;; Non-list states (not a non-empty set)
		(signals invalid-states
				 (make-instance 'machine-description
								:name "foo"
								:alphabet '(#\0 #\1 #\.)
								:blank #\.
								:states "bar"
								:initial-state :running
								:finals '(:error :success)
								:transitions transitions))
		;; States with duplicates (not a non-empty set)
		(signals invalid-states
				 (make-instance 'machine-description
								:name "foo"
								:alphabet '(#\0 #\1 #\.)
								:blank #\.
								:states '(:running :running :error :success)
								:initial-state :running
								:finals '(:error :success)
								:transitions transitions))
		;; States with non-symbol elements
		(signals invalid-states
				 (make-instance 'machine-description
								:name "foo"
								:alphabet '(#\0 #\1 #\.)
								:blank #\.
								:states '(:running :error :success 1)
								:initial-state :running
								:finals '(:error :success)
								:transitions transitions))
		;; Initial state
		(signals invalid-initial-state
				 (make-instance 'machine-description
								:name "foo"
								:alphabet '(#\0 #\1 #\.)
								:blank #\.
								:states '(:running :error :success)
								:initial-state :start
								:finals '(:error :success)
								:transitions transitions))
		;; Finals
		;; Empty finals (not a non-empty set)
		(signals invalid-finals
				 (make-instance 'machine-description
								:name "foo"
								:alphabet '(#\0 #\1 #\.)
								:blank #\.
								:states '(:running :error :success)
								:initial-state :running
								:finals ()
								:transitions transitions))
		;; Finals not a list (not a non-empty set)
		(signals invalid-finals
				 (make-instance 'machine-description
								:name "foo"
								:alphabet '(#\0 #\1 #\.)
								:blank #\.
								:states '(:running :error :success)
								:initial-state :running
								:finals "bar"
								:transitions transitions))
		;; Finals not a subset of states
		(signals invalid-finals
				 (make-instance 'machine-description
								:name "foo"
								:alphabet '(#\0 #\1 #\.)
								:blank #\.
								:states '(:running :error :success)
								:initial-state :running
								:finals '(:state)
								:transitions transitions))
		;; Finals with duplicate elements (not a non-empty set)
		(signals invalid-finals
				 (make-instance 'machine-description
								:name "foo"
								:alphabet '(#\0 #\1 #\.)
								:blank #\.
								:states '(:running :error :success)
								:initial-state :runnning
								:finals '(:error :error :success)
								:transitions transitions))
		;; Finals with non-symbol elements
		(signals invalid-finals
				 (make-instance 'machine-description
								:name "foo"
								:alphabet '(#\0 #\1 #\.)
								:blank #\.
								:states '(:running :error :success)
								:initial-state :running
								:finals '(:error :success 1)
								:transitions transitions))
	  ;; Transitions:
	  ;; Not an alist
	  (signals invalid-transitions
			   (make-instance 'machine-description
							  :name "foo"
							  :alphabet '(#\0 #\1 #\.)
							  :blank #\.
							  :states '(:running :error :success)
							  :initial-state :running
							  :finals '(:error :success)
							  :transitions :running)))
	  (signals invalid-transitions
			   (make-instance 'machine-description
							  :name "foo"
							  :alphabet '(#\0 #\1 #\.)
							  :blank #\.
							  :states '(:running :error :success)
							  :initial-state :running
							  :finals '(:error :success)
							  :transitions '(:running)))
	  (signals invalid-transitions
			   (make-instance 'machine-description
							  :name "foo"
							  :alphabet '(#\0 #\1 #\.)
							  :blank #\.
							  :states '(:running :error :success)
							  :initial-state :running
							  :finals '(:error :success)
							  :transitions '((:running))))
	  ;; Not an alist of alists
	  (signals invalid-transitions
			   (make-instance 'machine-description
							  :name "foo"
							  :alphabet '(#\0 #\1 #\.)
							  :blank #\.
							  :states '(:running :error :success)
							  :initial-state :running
							  :finals '(:error :success)
							  :transitions '((:foo . :running))))
	  (signals invalid-transitions
			   (make-instance 'machine-description
							  :name "foo"
							  :alphabet '(#\0 #\1 #\.)
							  :blank #\.
							  :states '(:running :error :success)
							  :initial-state :running
							  :finals '(:error :success)
							  :transitions '((:foo . '(:running)))))
	  (signals invalid-transitions
			   (make-instance 'machine-description
							  :name "foo"
							  :alphabet '(#\0 #\1 #\.)
							  :blank #\.
							  :states '(:running :error :success)
							  :initial-state :running
							  :finals '(:error :success)
							  :transitions '((:foo . '((:running))))))
	  (let ((tr (make-instance 'transition-result
							  :to-state :running
							  :to-char #\0
							  :action :right)))
		;; Invalid state
		(signals invalid-transitions
				 (make-instance 'machine-description
								:name "foo"
								:alphabet '(#\0 #\1 #\.)
								:blank #\.
								:states '(:running :error :success)
								:initial-state :running
								:finals '(:error :success)
								:transitions '((:foo . '((#\0 . tr))))))
		;; Invalid read char
		(signals invalid-transitions
				 (make-instance 'machine-description
								:name "foo"
								:alphabet '(#\0 #\1 #\.)
								:blank #\.
								:states '(:running :error :success)
								:initial-state :running
								:finals '(:error :success)
								:transitions '((:running . '((#\2 . tr)))))))
	  ;; Invalid char in transision
	  (let ((tr (make-instance 'transition-result
							   :to-state :running
							   :to-char #\2
							   :action :right)))
		(signals invalid-transitions
				 (make-instance 'machine-description
								:name "foo"
								:alphabet '(#\0 #\1 #\.)
								:blank #\.
								:states '(:running :error :success)
								:initial-state :running
								:finals '(:error :success)
								:transitions '((:running . '((#\0 . tr)))))))
	  ;; Invalid state in transision
	  (let ((tr (make-instance 'transition-result
							   :to-state :start
							   :to-char #\1
							   :action :right)))
		(signals invalid-transitions
				 (make-instance 'machine-description
								:name "foo"
								:alphabet '(#\0 #\1 #\.)
								:blank #\.
								:states '(:running :error :success)
								:initial-state :running
								:finals '(:error :success)
								:transitions '((:running . '((#\0 . tr))))))))
