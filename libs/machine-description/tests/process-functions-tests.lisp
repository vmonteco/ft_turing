(in-package :machine-description/tests)

(def-suite process-functions-tests :in machine-description-tests)
(in-suite process-functions-tests)

(test process-name-test
	  (is (equal (process-name "name") "name"))
	  (signals machine-description::invalid-json-name
			   (process-name 42))
	  (signals machine-description::invalid-json-name
			   (process-name "")))

(test process-alphabet-test
	  (let* ((val #("a" "b" "c"))
			 (processed-val (process-alphabet val))
			 (exp '(#\a #\b #\c)))
		(is (and (subsetp processed-val exp)
				 (subsetp exp processed-val))))
	  (signals machine-description::invalid-json-alphabet
			   (process-alphabet #())) 
	  (signals machine-description::invalid-json-alphabet
			   (process-alphabet 1))
	  (signals machine-description::invalid-json-alphabet
			   (process-alphabet #(1 2 3)))
	  (signals machine-description::invalid-json-alphabet
			   (process-alphabet #(#\a #\b #\c)))
	  (signals machine-description::invalid-json-alphabet
			   (process-alphabet #("a" "a" "b" "c"))))

(test process-blank-test
	  (is (eql #\a (process-blank "a")))
	  (signals machine-description::invalid-json-blank (process-blank 'a))
	  (signals machine-description::invalid-json-blank (process-blank nil))
	  (signals machine-description::invalid-json-blank (process-blank 1))
	  (signals machine-description::invalid-json-blank (process-blank "aa")))

(test process-states-test
	  (let* ((val #("foo" "bar"))
			 (processed-val (process-states val))
			 (exp '(:|foo| :|bar|)))
		(is (and (subsetp exp processed-val)
				 (subsetp processed-val exp))))
	  (signals machine-description::invalid-json-states (process-states #("foo" "foo" "bar")))
	  (signals machine-description::invalid-json-states (process-states #(:|foo| :|bar|)))
	  (signals machine-description::invalid-json-states (process-states "foo"))
	  (signals machine-description::invalid-json-states (process-states #(1 2 3))))

(test process-initial-state-test
	  (is (eql :|foo| (process-initial-state "foo")))
	  (signals machine-description::invalid-json-initial-state (process-initial-state 1))
	  (signals machine-description::invalid-json-initial-state (process-initial-state '(#\f #\o #\o))))

(test process-finals-test
	  (let* ((val #("foo" "bar"))
			 (processed-val (process-finals val))
			 (exp '(:|foo| :|bar|)))
		(is (and (subsetp exp processed-val)
				 (subsetp processed-val exp))))
	  (signals machine-description::invalid-json-finals (process-finals #("foo" "foo" "bar")))
	  (signals machine-description::invalid-json-finals (process-finals #(:|foo| :|bar|)))
	  (signals machine-description::invalid-json-finals (process-finals "foo"))
	  (signals machine-description::invalid-json-finals (process-finals #(1 2 3))))

(defmacro transition-result-equalp (tr exp-to-state exp-to-char exp-action)
  `(with-slots ((to-state to-state)
				(to-char to-char)
				(action action))
	   ,tr
	 (is (eql ,exp-to-state to-state))
	 (is (eql ,exp-to-char to-char))
	 (is (eql ,exp-action action))))

(test process-transitions-test
	  (let* ((raw-transitions (com.inuoe.jzon:parse "{
  \"scanright\": [
    {\"read\": \".\", \"to_state\": \"scanright\", \"write\": \".\", \"action\": \"RIGHT\"},
    {\"read\": \"1\", \"to_state\": \"scanright\", \"write\": \"1\", \"action\": \"RIGHT\"},
    {\"read\": \"-\", \"to_state\": \"scanright\", \"write\": \"-\", \"action\": \"RIGHT\"},
    {\"read\": \"=\", \"to_state\": \"eraseone\", \"write\": \".\", \"action\": \"LEFT\"}
  ],
  \"eraseone\": [
    {\"read\": \"1\", \"to_state\": \"subone\", \"write\": \"=\", \"action\": \"LEFT\"},
    {\"read\": \"-\", \"to_state\": \"HALT\", \"write\": \".\", \"action\": \"LEFT\"}
  ]
}"))
			 (transitions (process-transitions raw-transitions)))
		;; Check that transitions is an alist.
		(is (and (listp transitions)
				 (every #'consp transitions)))
		(let ((scanright (cdr (assoc :|scanright| transitions)))
			  (eraseone (cdr (assoc :|eraseone| transitions))))
		  (is (and (listp scanright)
				   (every #'consp scanright)))
		  (is (and (listp eraseone)
				   (every #'consp eraseone)))
		  (let* ((scanright-dot (assoc #\. scanright))
				 (scanright-dot-val (cdr scanright-dot))
				 (scanright-one (assoc #\1 scanright))
				 (scanright-one-val (cdr scanright-one))
				 (scanright-dash (assoc #\- scanright))
				 (scanright-dash-val (cdr scanright-dash))
				 (scanright-equal (assoc #\= scanright))
				 (scanright-equal-val (cdr scanright-equal))
				 (eraseone-one (assoc #\1 eraseone))
				 (eraseone-one-val (cdr eraseone-one))
				 (eraseone-dash (assoc #\- eraseone))
				 (eraseone-dash-val (cdr eraseone-dash)))
			(is (every (lambda (x) (typep x 'transition-result))
					   (list scanright-dot-val
							 scanright-one-val
							 scanright-dash-val
							 scanright-equal-val
							 eraseone-one-val
							 eraseone-dash-val)))
			(transition-result-equalp scanright-dot-val
									  :|scanright|
									  #\.
									  :right)
			(transition-result-equalp scanright-one-val
									  :|scanright|
									  #\1
									  :right)
			(transition-result-equalp scanright-dash-val
									  :|scanright|
									  #\-
									  :right)
			(transition-result-equalp scanright-equal-val
									  :|eraseone|
									  #\.
									  :left)
			(transition-result-equalp eraseone-one-val
									  :|subone|
									  #\=
									  :left)
			(transition-result-equalp eraseone-dash-val
									  :|HALT|
									  #\.
									  :left))))
	  ;; Cases with invalid json:
	  (let ((raw-transitions (com.inuoe.jzon:parse "{
  \"scanright\": [
    {\"in\": \".\", \"to_state\": \"scanright\", \"write\": \".\", \"action\": \"RIGHT\"},
    {\"in\": \"1\", \"to_state\": \"scanright\", \"write\": \"1\", \"action\": \"RIGHT\"},
    {\"in\": \"-\", \"to_state\": \"scanright\", \"write\": \"-\", \"action\": \"RIGHT\"},
    {\"in\": \"=\", \"to_state\": \"eraseone\", \"write\": \".\", \"action\": \"LEFT\"}
  ],
  \"eraseone\": [
    {\"in\": \"1\", \"to_state\": \"subone\", \"write\": \"=\", \"action\": \"LEFT\"},
    {\"in\": \"-\", \"to_state\": \"HALT\", \"write\": \".\", \"action\": \"LEFT\"}
  ]
}")))
		(signals machine-description::invalid-json-transitions
				 (process-transitions raw-transitions)))
	  (let ((raw-transitions (com.inuoe.jzon:parse "{
  \"scanright\": [
    {\"read\": \".\", \"state\": \"scanright\", \"write\": \".\", \"action\": \"RIGHT\"},
    {\"read\": \"1\", \"state\": \"scanright\", \"write\": \"1\", \"action\": \"RIGHT\"},
    {\"read\": \"-\", \"state\": \"scanright\", \"write\": \"-\", \"action\": \"RIGHT\"},
    {\"read\": \"=\", \"state\": \"eraseone\", \"write\": \".\", \"action\": \"LEFT\"}
  ],
  \"eraseone\": [
    {\"read\": \"1\", \"state\": \"subone\", \"write\": \"=\", \"action\": \"LEFT\"},
    {\"read\": \"-\", \"state\": \"HALT\", \"write\": \".\", \"action\": \"LEFT\"}
  ]
}")))
		(signals machine-description::invalid-json-transitions (process-transitions raw-transitions)))
	  (let ((raw-transitions (com.inuoe.jzon:parse "{
  \"scanright\": [
    {\"read\": \".\", \"to_state\": \"scanright\", \"out\": \".\", \"action\": \"RIGHT\"},
    {\"read\": \"1\", \"to_state\": \"scanright\", \"out\": \"1\", \"action\": \"RIGHT\"},
    {\"read\": \"-\", \"to_state\": \"scanright\", \"out\": \"-\", \"action\": \"RIGHT\"},
    {\"read\": \"=\", \"to_state\": \"eraseone\", \"out\": \".\", \"action\": \"LEFT\"}
  ],
  \"eraseone\": [
    {\"read\": \"1\", \"to_state\": \"subone\", \"out\": \"=\", \"action\": \"LEFT\"},
    {\"read\": \"-\", \"to_state\": \"HALT\", \"out\": \".\", \"action\": \"LEFT\"}
  ]
}")))
		(signals machine-description::invalid-json-transitions (process-transitions raw-transitions)))
	  (let ((raw-transitions (com.inuoe.jzon:parse "{
  \"scanright\": [
    {\"read\": \".\", \"to_state\": \"scanright\", \"write\": \".\", \"do\": \"RIGHT\"},
    {\"read\": \"1\", \"to_state\": \"scanright\", \"write\": \"1\", \"do\": \"RIGHT\"},
    {\"read\": \"-\", \"to_state\": \"scanright\", \"write\": \"-\", \"do\": \"RIGHT\"},
    {\"read\": \"=\", \"to_state\": \"eraseone\", \"write\": \".\", \"do\": \"LEFT\"}
  ],
  \"eraseone\": [
    {\"read\": \"1\", \"to_state\": \"subone\", \"write\": \"=\", \"do\": \"LEFT\"},
    {\"read\": \"-\", \"to_state\": \"HALT\", \"write\": \".\", \"do\": \"LEFT\"}
  ]
}")))
		(signals machine-description::invalid-json-transitions (process-transitions raw-transitions))))
