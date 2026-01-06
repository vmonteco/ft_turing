(asdf:defsystem "machine-description"
  :components ((:file "package")
			   (:file "transition-result" :depends-on ("package"))
			   (:file "machine-description"
				:depends-on ("package" "transition-result")))
  :in-order-to ((asdf:test-op (asdf:test-op "machine-description/tests"))))

(asdf:defsystem "machine-description/tests"
  :depends-on ("machine-description" "fiveam")
  :components ((:module "tests"
				:components ((:file "package")
							 (:file "tests" :depends-on ("package"))
							 (:file "transition-result-tests"
							  :depends-on ("package" "tests")))))
  :perform (asdf:test-op
			(o c)
			(uiop:symbol-call
			 :fiveam :run!
			 (find-symbol* '#:machine-description-tests
						   :machine-description/tests))))
