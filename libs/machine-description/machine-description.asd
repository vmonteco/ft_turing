(asdf:defsystem "machine-description"
  :components ((:file "package")
			   (:file "conditions")
			   (:file "process-functions" :depends-on ("package" "conditions"))
			   (:file "transition-result" :depends-on ("package" "conditions"))
			   (:file "machine-description"
				:depends-on ("package" "conditions" "transition-result"))
			   (:file "format-functions"
				:depends-on ("package" "transition-result" "machine-description")))
  :depends-on ("utils" "com.inuoe.jzon")
  :in-order-to ((asdf:test-op (asdf:test-op "utils/tests")
							  (asdf:test-op "machine-description/tests"))))

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
