(in-package :machine-description/tests)

(def-suite machine-description-tests)
(in-suite machine-description-tests)

(test machine-description-instantiation
	  (let* ((t1 (make-instance 'transition-result
								:to-state :|running|
								:to-char #\0
								:action :right))
			 (t2 (make-instance 'transition-result
								:to-state :|running|
								:to-char #\1
								:action :right))
			 (t3 (make-instance 'transition-result
								:to-state :|running|
								:to-char #\.
								:action :right))
			 (running (list (cons #\0 t1)
							(cons #\1 t2)
							(cons #\. t3)))
			 (transitions (list (cons :|running| running))))
		;; OK-ish instantiation
		(let ((md (make-instance 'machine-description
								 :name "foo"
								 :alphabet '(#\0 #\1 #\.)
								 :blank #\.
								 :states '(:|running| :|error| :|success|)
								 :initial-state :|running|
								 :finals '(:|error| :|success|)
								 :transitions transitions)))
		  (is (equal "foo" (name md)))
		  (is (and (subsetp (alphabet md) '(#\0 #\1 #\.))
						(subsetp '(#\0 #\1 #\.) (alphabet md))))
		  (is (equal #\. (blank md)))
		  (is (and (subsetp (states md) '(:|running| :|error| :|success|))
				   (subsetp '(:|running| :|error| :|success|) (states md))))
		  (is (equal :|running| (initial-state md)))
		  (is (and (subsetp (finals md) '(:|error| :|success|))
				   (subsetp '(:|error| :|success|) (finals md))))
		  (is (equalp transitions (transitions md))))
		
		;; Name issues
		;; Empty name
		(signals invalid-name
				 (make-instance 'machine-description
								:name ""
								:alphabet '(#\0 #\1 #\.)
								:blank #\.
								:states '(:|running| :|error| :|success|)
								:initial-state :|running|
								:finals '(:|error| :|success|)
								:transitions transitions))
		
		;; Alphabet issues
		;; Empty alphabet (not a non-empty set)
		(signals invalid-alphabet
				 (make-instance 'machine-description
								:name "foo"
								:alphabet ()
								:blank #\.
								:states '(:|running| :|error| :|success|)
								:initial-state :|running|
								:finals '(:|error| :|success|)
								:transitions transitions))
		;; Alphabet with duplicates (not a non-empty set)
		(signals invalid-alphabet
				 (make-instance 'machine-description
								:name "foo"
								:alphabet '(#\0 #\0 #\.)
								:blank #\.
								:states '(:|running| :|error| :|success|)
								:initial-state :|running|
								:finals '(:|error| :|success|)
								:transitions transitions))
		;; Non-list alphabet (not a non-empty set)
		(signals invalid-alphabet
				 (make-instance 'machine-description
								:name "foo"
								:alphabet "bar."
								:blank #\.
								:states '(:|running| :|error| :|success|)
								:initial-state :|running|
								:finals '(:|error| :|success|)
								:transitions transitions))
		;; Alphabet with non-character elements
		(signals invalid-alphabet
				 (make-instance 'machine-description
								:name "foo"
								:alphabet '(1 2 3 #\.)
								:blank #\.
								:states '(:|running| :|error| :|success|)
								:initial-state :|running|
								:finals '(:|error| :|success|)
								:transitions transitions))
		;; Blank issue:
		(signals invalid-blank
				 (make-instance 'machine-description
								:name "foo"
								:alphabet '(#\0 #\1)
								:blank #\.
								:states '(:|running| :|error| :|success|)
								:initial-state :|running|
								:finals '(:|error| :|success|)
								:transitions transitions))
		
		;; States issue
		;; Empty states (not a non-empty set)
		(signals invalid-states
				 (make-instance 'machine-description
								:name "foo"
								:alphabet '(#\0 #\1 #\.)
								:blank #\.
								:states ()
								:initial-state :|running|
								:finals '(:|error| :|success|)
								:transitions transitions))

		;; Non-list states (not a non-empty set)
		(signals invalid-states
				 (make-instance 'machine-description
								:name "foo"
								:alphabet '(#\0 #\1 #\.)
								:blank #\.
								:states "bar"
								:initial-state :|running|
								:finals '(:|error| :|success|)
								:transitions transitions))
		;; States with duplicates (not a non-empty set)
		(signals invalid-states
				 (make-instance 'machine-description
								:name "foo"
								:alphabet '(#\0 #\1 #\.)
								:blank #\.
								:states '(:|running| :|running| :|error| :|success|)
								:initial-state :|running|
								:finals '(:|error| :|success|)
								:transitions transitions))
		;; States with non-symbol elements
		(signals invalid-states
				 (make-instance 'machine-description
								:name "foo"
								:alphabet '(#\0 #\1 #\.)
								:blank #\.
								:states '(:|running| :|error| :|success| 1)
								:initial-state :|running|
								:finals '(:|error| :|success|)
								:transitions transitions))
		;; Initial state
		(signals invalid-initial-state
				 (make-instance 'machine-description
								:name "foo"
								:alphabet '(#\0 #\1 #\.)
								:blank #\.
								:states '(:|running| :|error| :|success|)
								:initial-state :start
								:finals '(:|error| :|success|)
								:transitions transitions))
		
		;; Finals
		;; Empty finals (not a non-empty set)
		(signals invalid-finals
				 (make-instance 'machine-description
								:name "foo"
								:alphabet '(#\0 #\1 #\.)
								:blank #\.
								:states '(:|running| :|error| :|success|)
								:initial-state :|running|
								:finals ()
								:transitions transitions))
		;; Finals not a list (not a non-empty set)
		(signals invalid-finals
				 (make-instance 'machine-description
								:name "foo"
								:alphabet '(#\0 #\1 #\.)
								:blank #\.
								:states '(:|running| :|error| :|success|)
								:initial-state :|running|
								:finals "bar"
								:transitions transitions))
		;; Finals not a subset of states
		(signals invalid-finals
				 (make-instance 'machine-description
								:name "foo"
								:alphabet '(#\0 #\1 #\.)
								:blank #\.
								:states '(:|running| :|error| :|success|)
								:initial-state :|running|
								:finals '(:state)
								:transitions transitions))
		;; Finals with duplicate elements (not a non-empty set)
		(signals invalid-finals
				 (make-instance 'machine-description
								:name "foo"
								:alphabet '(#\0 #\1 #\.)
								:blank #\.
								:states '(:|running| :|error| :|success|)
								:initial-state :|running|
								:finals '(:|error| :|error| :|success|)
								:transitions transitions))
		
		;; Finals with non-symbol elements
		(signals invalid-finals
				 (make-instance 'machine-description
								:name "foo"
								:alphabet '(#\0 #\1 #\.)
								:blank #\.
								:states '(:|running| :|error| :|success|)
								:initial-state :|running|
								:finals '(:|error| :|success| 1)
								:transitions transitions))

		;; Transitions:
		;; Not an alist
		(signals invalid-transitions
				 (make-instance 'machine-description
								:name "foo"
								:alphabet '(#\0 #\1 #\.)
								:blank #\.
								:states '(:|running| :|error| :|success|)
								:initial-state :|running|
								:finals '(:|error| :|success|)
								:transitions :|running|))
		(signals invalid-transitions
				 (make-instance 'machine-description
								:name "foo"
								:alphabet '(#\0 #\1 #\.)
								:blank #\.
								:states '(:|running| :|error| :|success|)
								:initial-state :|running|
								:finals '(:|error| :|success|)
								:transitions '(:|running|)))
		
		;; Since (eq (cons :|running| nil) (list :|running|)), '((:|running|)) is a
		;; valid set of transitions.
		(make-instance 'machine-description
					   :name "foo"
					   :alphabet '(#\0 #\1 #\.)
					   :blank #\.
					   :states '(:|running| :|error| :|success|)
					   :initial-state :|running|
					   :finals '(:|error| :|success|)
					   :transitions '((:|running|)))
		
		;; Not an alist of alists
		(signals invalid-transitions
				 (make-instance 'machine-description
								:name "foo"
								:alphabet '(#\0 #\1 #\.)
								:blank #\.
								:states '(:|running| :|error| :|success|)
								:initial-state :|running|
								:finals '(:|error| :|success|)
								:transitions '((:foo . :|running|))))
		
		(signals invalid-transitions
				 (make-instance 'machine-description
								:name "foo"
								:alphabet '(#\0 #\1 #\.)
								:blank #\.
								:states '(:|running| :|error| :|success|)
								:initial-state :|running|
								:finals '(:|error| :|success|)
								:transitions '((:foo . '(:|running|)))))
		(signals invalid-transitions
				 (make-instance 'machine-description
								:name "foo"
								:alphabet '(#\0 #\1 #\.)
								:blank #\.
								:states '(:|running| :|error| :|success|)
								:initial-state :|running|
								:finals '(:|error| :|success|)
								:transitions '((:foo . '((:|running|))))))

		(let ((tr (make-instance 'transition-result
								 :to-state :|running|
								 :to-char #\0
								 :action :right)))
		  ;; Invalid state
		  (signals invalid-transitions
				   (make-instance 'machine-description
								  :name "foo"
								  :alphabet '(#\0 #\1 #\.)
								  :blank #\.
								  :states '(:|running| :|error| :|success|)
								  :initial-state :|running|
								  :finals '(:|error| :|success|)
								  :transitions '((:foo . '((#\0 . tr))))))
		  
		  ;; Invalid read char
		  (signals invalid-transitions
				   (make-instance 'machine-description
								  :name "foo"
								  :alphabet '(#\0 #\1 #\.)
								  :blank #\.
								  :states '(:|running| :|error| :|success|)
								  :initial-state :|running|
								  :finals '(:|error| :|success|)
								  :transitions '((:|running| . '((#\2 . tr)))))))

		;; Invalid char in transision
		(let ((tr (make-instance 'transition-result
								 :to-state :|running|
								 :to-char #\2
								 :action :right)))
		  (signals invalid-transitions
				   (make-instance 'machine-description
								  :name "foo"
								  :alphabet '(#\0 #\1 #\.)
								  :blank #\.
								  :states '(:|running| :|error| :|success|)
								  :initial-state :|running|
								  :finals '(:|error| :|success|)
								  :transitions '((:|running| . '((#\0 . tr)))))))
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
								  :states '(:|running| :|error| :|success|)
								  :initial-state :|running|
								  :finals '(:|error| :|success|)
								  :transitions '((:|running| . '((#\0 . tr)))))))))

(test make-machine-description-from-json-tests
	  ;; Empty JSON case (basically a JSON that's missing all the fields)
	  (let ((json ""))
		(signals invalid-json (make-machine-description-from-json json)))
	  (let ((json "{}"))
		(signals invalid-json (make-machine-description-from-json json)))

	  ;; Incorrectly formatted JSON case
	  (let ((json "{"))
		(signals invalid-json (make-machine-description-from-json json)))

	  ;; Valid JSON instance
	  ;; Accepts extra fields
	  (let* ((json "{
  \"name\": \"foo\",
  \"description\": \"foo turing machine\",
  \"alphabet\": [\"0\", \"1\", \".\"],
  \"blank\": \".\",
  \"states\": [\"start\", \"running\", \"success\", \"failure\"],
  \"initial\": \"start\",
  \"finals\": [\"success\", \"failure\"],
  \"transitions\": {
    \"start\": [
      {
        \"read\": \"0\",
        \"to_state\": \"running\",
        \"write\": \"0\",
        \"action\": \"RIGHT\"
      },
      {
        \"read\": \"1\",
        \"to_state\": \"failure\",
        \"write\": \"1\",
        \"action\": \"RIGHT\"
      },
      {
        \"read\": \".\",
        \"to_state\": \"success\",
        \"write\": \".\",
        \"action\": \"RIGHT\"
      }
    ],
    \"running\": [
      {
        \"read\": \"0\",
        \"to_state\": \"running\",
        \"write\": \"0\",
        \"action\": \"RIGHT\"
      },
      {
        \"read\": \"1\",
        \"to_state\": \"failure\",
        \"write\": \"1\",
        \"action\": \"RIGHT\"
      },
      {
        \"read\": \".\",
        \"to_state\": \"success\",
        \"write\": \".\",
        \"action\": \"RIGHT\"
      }
    ]
  }
}")
			 (md (make-machine-description-from-json json)))	
		(is (equal "foo" (name md)))
		(is (and (subsetp '(#\0 #\1 #\.) (alphabet md))
				 (subsetp (alphabet md) '(#\0 #\1 #\.))))
		(is (equal #\. (blank md)))
		(is (and (subsetp '(:|start| :|running| :|success| :|failure|) (states md))
				 (subsetp (states md) '(:|start| :|running| :|success| :|failure|))))
		(is (equal :|start| (initial-state md)))
		(is (and (subsetp (finals md) '(:|success| :|failure|))
				 (subsetp '(:|success| :|failure|) (finals md))))

		;; transitions
		(let ((transitions (transitions md)))
		  ;; transitions root
		  (is (listp transitions))
		  (is (every #'consp transitions))

		  ;; transitions 1st level keys
		  (let ((transition-state-keys (map 'list #'car transitions)))
			(is (and (subsetp '(:|start| :|running|) transition-state-keys)
					 (subsetp transition-state-keys '(:|start| :|running|)))))

		  ;; transitions 1st level values
		  (let ((start-trs-cons (assoc :|start| transitions))
				(running-trs-cons (assoc :|running| transitions)))
			(is-true start-trs-cons)
			(is-true running-trs-cons)
			(let ((start-trs-alist (cdr start-trs-cons))
				  (running-trs-alist (cdr running-trs-cons)))
			  (is (listp start-trs-alist))
			  (is (listp running-trs-alist))
			  (is (every #'consp start-trs-alist))
			  (is (every #'consp running-trs-alist))

			  ;; transitions 2nd level keys
			  (let ((start-keys (map 'list #'car start-trs-alist))
					(running-keys (map 'list #'car running-trs-alist)))
				(is (and (subsetp '(#\0 #\1 #\.) start-keys)
						 (subsetp start-keys '(#\0 #\1 #\.))))
				(is (and (subsetp '(#\0 #\1 #\.) running-keys)
						 (subsetp running-keys '(#\0 #\1 #\.)))))

			  ;; transitions end level values
			  (is (every (lambda (x) (typep x 'transition-result))
						 (map 'list #'cdr start-trs-alist)))
			  (is (every (lambda (x) (typep x 'transition-result))
						 (map 'list #'cdr running-trs-alist)))
			  
			  ;; start-0
			  (let ((tr-cons (assoc #\0 start-trs-alist)))
				(is-true tr-cons)
				(let ((tr (cdr tr-cons)))
				  (is (equal :|running| (to-state tr)))
				  (is (equal #\0 (to-char tr)))
				  (is (equal :right (action tr)))))
			  
			  ;; start-1
			  (let ((tr-cons (assoc #\1 start-trs-alist)))
				(is-true tr-cons)
				(let ((tr (cdr tr-cons)))
				  (is (equal :|failure| (to-state tr)))
				  (is (equal #\1 (to-char tr)))
				  (is (equal :right (action tr)))))

			  ;; start-.
			  (let ((tr-cons (assoc #\. start-trs-alist)))
				(is-true tr-cons)
				(let ((tr (cdr tr-cons)))
				  (is (equal :|success| (to-state tr)))
				  (is (equal #\. (to-char tr)))
				  (is (equal :right (action tr)))))
			  
			  ;; running-0
			  (let ((tr-cons (assoc #\0 running-trs-alist)))
				(is-true tr-cons)
				(let ((tr (cdr tr-cons)))
				  (is (equal :|running| (to-state tr)))
				  (is (equal #\0 (to-char tr)))
				  (is (equal :right (action tr)))))

			  ;; running-1
			  (let ((tr-cons (assoc #\1 running-trs-alist)))
				(is-true tr-cons)
				(let ((tr (cdr tr-cons)))
				  (is (equal :|failure| (to-state tr)))
				  (is (equal #\1 (to-char tr)))
				  (is (equal :right (action tr)))))
			  
			  ;; running-.
			  (let ((tr-cons (assoc #\. running-trs-alist)))
				(is-true tr-cons)
				(let ((tr (cdr tr-cons)))
				  (is (equal :|success| (to-state tr)))
				  (is (equal #\. (to-char tr)))
				  (is (equal :right (action tr)))))))))
	  
	  ;; With missing fields
	  ;; Missing the name
	  (let ((json "{
  \"description\": \"foo turing machine\",
  \"alphabet\": [\"0\", \"1\", \".\"],
  \"blank\": \".\",
  \"states\": [\"start\", \"running\", \"success\", \"failure\"],
  \"initial\": \"start\",
  \"finals\": [\"success\", \"failure\"],
  \"transitions\": {
    \"start\": [
      {
        \"read\": \"0\",
        \"to_state\": \"running\",
        \"write\": \"0\",
        \"action\": \"RIGHT\"
      },
      {
        \"read\": \"1\",
        \"to_state\": \"failure\",
        \"write\": \"1\",
        \"action\": \"RIGHT\"
      },
      {
        \"read\": \".\",
        \"to_state\": \"success\",
        \"write\": \".\",
        \"action\": \"RIGHT\"
      }
    ],
    \"running\": [
      {
        \"read\": \"0\",
        \"to_state\": \"running\",
        \"write\": \"0\",
        \"action\": \"RIGHT\"
      },
      {
        \"read\": \"1\",
        \"to_state\": \"failure\",
        \"write\": \"1\",
        \"action\": \"RIGHT\"
      },
      {
        \"read\": \".\",
        \"to_state\": \"success\",
        \"write\": \".\",
        \"action\": \"RIGHT\"
      }
    ]
  }
}"))
		(signals machine-description:invalid-json (make-machine-description-from-json json)))

	  ;; Missing the alphabet
	  (let ((json "{
  \"name\": \"foo\",
  \"description\": \"foo turing machine\",
  \"blank\": \".\",
  \"states\": [\"start\", \"running\", \"success\", \"failure\"],
  \"initial\": \"start\",
  \"finals\": [\"success\", \"failure\"],
  \"transitions\": {
    \"start\": [
      {
        \"read\": \"0\",
        \"to_state\": \"running\",
        \"write\": \"0\",
        \"action\": \"RIGHT\"
      },
      {
        \"read\": \"1\",
        \"to_state\": \"failure\",
        \"write\": \"1\",
        \"action\": \"RIGHT\"
      },
      {
        \"read\": \".\",
        \"to_state\": \"success\",
        \"write\": \".\",
        \"action\": \"RIGHT\"
      }
    ],
    \"running\": [
      {
        \"read\": \"0\",
        \"to_state\": \"running\",
        \"write\": \"0\",
        \"action\": \"RIGHT\"
      },
      {
        \"read\": \"1\",
        \"to_state\": \"failure\",
        \"write\": \"1\",
        \"action\": \"RIGHT\"
      },
      {
        \"read\": \".\",
        \"to_state\": \"success\",
        \"write\": \".\",
        \"action\": \"RIGHT\"
      }
    ]
  }
}"))
		(signals machine-description:invalid-json (make-machine-description-from-json json)))

	  ;; Missing the blank
	  (let ((json "{
  \"name\": \"foo\",
  \"description\": \"foo turing machine\",
  \"alphabet\": [\"0\", \"1\", \".\"],
  \"states\": [\"start\", \"running\", \"success\", \"failure\"],
  \"initial\": \"start\",
  \"finals\": [\"success\", \"failure\"],
  \"transitions\": {
    \"start\": [
      {
        \"read\": \"0\",
        \"to_state\": \"running\",
        \"write\": \"0\",
        \"action\": \"RIGHT\"
      },
      {
        \"read\": \"1\",
        \"to_state\": \"failure\",
        \"write\": \"1\",
        \"action\": \"RIGHT\"
      },
      {
        \"read\": \".\",
        \"to_state\": \"success\",
        \"write\": \".\",
        \"action\": \"RIGHT\"
      }
    ],
    \"running\": [
      {
        \"read\": \"0\",
        \"to_state\": \"running\",
        \"write\": \"0\",
        \"action\": \"RIGHT\"
      },
      {
        \"read\": \"1\",
        \"to_state\": \"failure\",
        \"write\": \"1\",
        \"action\": \"RIGHT\"
      },
      {
        \"read\": \".\",
        \"to_state\": \"success\",
        \"write\": \".\",
        \"action\": \"RIGHT\"
      }
    ]
  }
}"))
		(signals machine-description:invalid-json (make-machine-description-from-json json)))

	  ;; Missing the states
	  (let ((json "{
  \"name\": \"foo\",
  \"description\": \"foo turing machine\",
  \"alphabet\": [\"0\", \"1\", \".\"],
  \"blank\": \".\",
  \"initial\": \"start\",
  \"finals\": [\"success\", \"failure\"],
  \"transitions\": {
    \"start\": [
      {
        \"read\": \"0\",
        \"to_state\": \"running\",
        \"write\": \"0\",
        \"action\": \"RIGHT\"
      },
      {
        \"read\": \"1\",
        \"to_state\": \"failure\",
        \"write\": \"1\",
        \"action\": \"RIGHT\"
      },
      {
        \"read\": \".\",
        \"to_state\": \"success\",
        \"write\": \".\",
        \"action\": \"RIGHT\"
      }
    ],
    \"running\": [
      {
        \"read\": \"0\",
        \"to_state\": \"running\",
        \"write\": \"0\",
        \"action\": \"RIGHT\"
      },
      {
        \"read\": \"1\",
        \"to_state\": \"failure\",
        \"write\": \"1\",
        \"action\": \"RIGHT\"
      },
      {
        \"read\": \".\",
        \"to_state\": \"success\",
        \"write\": \".\",
        \"action\": \"RIGHT\"
      }
    ]
  }
}"))
		(signals machine-description:invalid-json (make-machine-description-from-json json)))

	  ;; Missing the initial
	  (let ((json "{
  \"name\": \"foo\",
  \"description\": \"foo turing machine\",
  \"alphabet\": [\"0\", \"1\", \".\"],
  \"blank\": \".\",
  \"states\": [\"start\", \"running\", \"success\", \"failure\"],
  \"finals\": [\"success\", \"failure\"],
  \"transitions\": {
    \"start\": [
      {
        \"read\": \"0\",
        \"to_state\": \"running\",
        \"write\": \"0\",
        \"action\": \"RIGHT\"
      },
      {
        \"read\": \"1\",
        \"to_state\": \"failure\",
        \"write\": \"1\",
        \"action\": \"RIGHT\"
      },
      {
        \"read\": \".\",
        \"to_state\": \"success\",
        \"write\": \".\",
        \"action\": \"RIGHT\"
      }
    ],
    \"running\": [
      {
        \"read\": \"0\",
        \"to_state\": \"running\",
        \"write\": \"0\",
        \"action\": \"RIGHT\"
      },
      {
        \"read\": \"1\",
        \"to_state\": \"failure\",
        \"write\": \"1\",
        \"action\": \"RIGHT\"
      },
      {
        \"read\": \".\",
        \"to_state\": \"success\",
        \"write\": \".\",
        \"action\": \"RIGHT\"
      }
    ]
  }
}"))
		(signals machine-description:invalid-json (make-machine-description-from-json json)))
	  
	  ;; Missing finals
	  (let ((json "{
  \"name\": \"foo\",
  \"description\": \"foo turing machine\",
  \"alphabet\": [\"0\", \"1\", \".\"],
  \"blank\": \".\",
  \"states\": [\"start\", \"running\", \"success\", \"failure\"],
  \"initial\": \"start\",
  \"transitions\": {
    \"start\": [
      {
        \"read\": \"0\",
        \"to_state\": \"running\",
        \"write\": \"0\",
        \"action\": \"RIGHT\"
      },
      {
        \"read\": \"1\",
        \"to_state\": \"failure\",
        \"write\": \"1\",
        \"action\": \"RIGHT\"
      },
      {
        \"read\": \".\",
        \"to_state\": \"success\",
        \"write\": \".\",
        \"action\": \"RIGHT\"
      }
    ],
    \"running\": [
      {
        \"read\": \"0\",
        \"to_state\": \"running\",
        \"write\": \"0\",
        \"action\": \"RIGHT\"
      },
      {
        \"read\": \"1\",
        \"to_state\": \"failure\",
        \"write\": \"1\",
        \"action\": \"RIGHT\"
      },
      {
        \"read\": \".\",
        \"to_state\": \"success\",
        \"write\": \".\",
        \"action\": \"RIGHT\"
      }
    ]
  }
}"))
		(signals machine-description:invalid-json (make-machine-description-from-json json)))

	  ;; Missing transitions
	  (let ((json "{
  \"name\": \"foo\",
  \"description\": \"foo turing machine\",
  \"alphabet\": [\"0\", \"1\", \".\"],
  \"blank\": \".\",
  \"states\": [\"start\", \"running\", \"success\", \"failure\"],
  \"initial\": \"start\",
  \"finals\": [\"success\", \"failure\"]
}"))
		(signals machine-description:invalid-json (make-machine-description-from-json json)))

	  ;; Missing parts of transitions
	  ;; Missing read in transitions
	  (let ((json "{
  \"name\": \"foo\",
  \"description\": \"foo turing machine\",
  \"alphabet\": [\"0\", \"1\", \".\"],
  \"blank\": \".\",
  \"states\": [\"start\", \"running\", \"success\", \"failure\"],
  \"initial\": \"start\",
  \"finals\": [\"success\", \"failure\"],
  \"transitions\": {
    \"start\": [
      {
        \"read\": \"0\",
        \"to_state\": \"running\",
        \"write\": \"0\",
        \"action\": \"RIGHT\"
      },
      {
        \"to_state\": \"failure\",
        \"write\": \"1\",
        \"action\": \"RIGHT\"
      },
      {
        \"read\": \".\",
        \"to_state\": \"success\",
        \"write\": \".\",
        \"action\": \"RIGHT\"
      }
    ],
    \"running\": [
      {
        \"read\": \"0\",
        \"to_state\": \"running\",
        \"write\": \"0\",
        \"action\": \"RIGHT\"
      },
      {
        \"read\": \"1\",
        \"to_state\": \"failure\",
        \"write\": \"1\",
        \"action\": \"RIGHT\"
      },
      {
        \"read\": \".\",
        \"to_state\": \"success\",
        \"write\": \".\",
        \"action\": \"RIGHT\"
      }
    ]
  }
}"))
		(signals machine-description:invalid-json (make-machine-description-from-json json)))

	  ;; Missing to_state in transitions
	  (let ((json "{
  \"name\": \"foo\",
  \"description\": \"foo turing machine\",
  \"alphabet\": [\"0\", \"1\", \".\"],
  \"blank\": \".\",
  \"states\": [\"start\", \"running\", \"success\", \"failure\"],
  \"initial\": \"start\",
  \"finals\": [\"success\", \"failure\"],
  \"transitions\": {
    \"start\": [
      {
        \"read\": \"0\",
        \"to_state\": \"running\",
        \"write\": \"0\",
        \"action\": \"RIGHT\"
      },
      {
        \"read\": \"1\",
        \"write\": \"1\",
        \"action\": \"RIGHT\"
      },
      {
        \"read\": \".\",
        \"to_state\": \"success\",
        \"write\": \".\",
        \"action\": \"RIGHT\"
      }
    ],
    \"running\": [
      {
        \"read\": \"0\",
        \"to_state\": \"running\",
        \"write\": \"0\",
        \"action\": \"RIGHT\"
      },
      {
        \"read\": \"1\",
        \"to_state\": \"failure\",
        \"write\": \"1\",
        \"action\": \"RIGHT\"
      },
      {
        \"read\": \".\",
        \"to_state\": \"success\",
        \"write\": \".\",
        \"action\": \"RIGHT\"
      }
    ]
  }
}"))
		(signals machine-description:invalid-json (make-machine-description-from-json json)))

	  ;; Missing write in transitions
	  (let ((json "{
  \"name\": \"foo\",
  \"description\": \"foo turing machine\",
  \"alphabet\": [\"0\", \"1\", \".\"],
  \"blank\": \".\",
  \"states\": [\"start\", \"running\", \"success\", \"failure\"],
  \"initial\": \"start\",
  \"finals\": [\"success\", \"failure\"],
  \"transitions\": {
    \"start\": [
      {
        \"read\": \"0\",
        \"to_state\": \"running\",
        \"write\": \"0\",
        \"action\": \"RIGHT\"
      },
      {
        \"read\": \"1\",
        \"to_state\": \"failure\",
        \"action\": \"RIGHT\"
      },
      {
        \"read\": \".\",
        \"to_state\": \"success\",
        \"write\": \".\",
        \"action\": \"RIGHT\"
      }
    ],
    \"running\": [
      {
        \"read\": \"0\",
        \"to_state\": \"running\",
        \"write\": \"0\",
        \"action\": \"RIGHT\"
      },
      {
        \"read\": \"1\",
        \"to_state\": \"failure\",
        \"write\": \"1\",
        \"action\": \"RIGHT\"
      },
      {
        \"read\": \".\",
        \"to_state\": \"success\",
        \"write\": \".\",
        \"action\": \"RIGHT\"
      }
    ]
  }
}"))
		(signals machine-description:invalid-json (make-machine-description-from-json json)))

	  ;; Missing actions in transitions
	  (let ((json "{
  \"name\": \"foo\",
  \"description\": \"foo turing machine\",
  \"alphabet\": [\"0\", \"1\", \".\"],
  \"blank\": \".\",
  \"states\": [\"start\", \"running\", \"success\", \"failure\"],
  \"initial\": \"start\",
  \"finals\": [\"success\", \"failure\"],
  \"transitions\": {
    \"start\": [
      {
        \"read\": \"0\",
        \"to_state\": \"running\",
        \"write\": \"0\",
        \"action\": \"RIGHT\"
      },
      {
        \"read\": \"1\",
        \"to_state\": \"failure\",
        \"write\": \"1\"
      },
      {
        \"read\": \".\",
        \"to_state\": \"success\",
        \"write\": \".\",
        \"action\": \"RIGHT\"
      }
    ],
    \"running\": [
      {
        \"read\": \"0\",
        \"to_state\": \"running\",
        \"write\": \"0\",
        \"action\": \"RIGHT\"
      },
      {
        \"read\": \"1\",
        \"to_state\": \"failure\",
        \"write\": \"1\",
        \"action\": \"RIGHT\"
      },
      {
        \"read\": \".\",
        \"to_state\": \"success\",
        \"write\": \".\",
        \"action\": \"RIGHT\"
      }
    ]
  }
}"))
		(signals machine-description:invalid-json (make-machine-description-from-json json)))
	  )
