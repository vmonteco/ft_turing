(asdf:defsystem "machine-description"
  :depends-on ("utils" "com.inuoe.jzon")
  :components ((:file "package")
			   (:file "params"
				:depends-on ("package"))
			   (:file "conditions"
				:depends-on ("package" "params"))
			   (:file "process-functions"
				:depends-on ("package" "params" "conditions"))
			   (:file "transition-result"
				:depends-on ("package" "params" "conditions"))
			   (:file "machine-description"
				:depends-on
				("package" "params" "conditions" "transition-result"))
			   (:file "format-functions"
				:depends-on
				("package"
				 "params"
				 "conditions"
				 "transition-result"
				 "machine-description")))
  :in-order-to ((asdf:test-op (asdf:test-op "machine-description/tests"))))

(asdf:defsystem "machine-description/tests"
  :depends-on ("machine-description" "fiveam")
  :components ((:module "tests"
				:components ((:file "package")
							 (:file "tests" :depends-on ("package"))
							 (:file "transition-result-tests"
							  :depends-on ("package" "tests"))
							 (:file "process-functions-tests"
							  :depends-on ("package" "tests")))))
  :perform (asdf:test-op
			(o c)
			(uiop:symbol-call
			 :fiveam :run!
			 (find-symbol* '#:machine-description-tests
						   :machine-description/tests))))
