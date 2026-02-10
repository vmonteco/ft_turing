(asdf:defsystem "machine-maker"
  :components ((:file "package")
			   (:file "machine-maker" :depends-on ("package")))
  :depends-on ("hardware" "machine-description" "utils")
  :in-order-to ((asdf:test-op (asdf:test-op "hardware/tests")
							  (asdf:test-op "machine-description/tests")
							  (asdf:test-op "machine-maker/tests"))))

(asdf:defsystem "machine-maker/tests"
  :depends-on ("machine-maker" "fiveam")
  :components ((:module "tests"
				:components ((:file "package")
							 (:file "tests" :depends-on ("package")))))
  :perform (asdf:test-op
			(o c)
			(uiop:symbol-call
			 :fiveam :run!
			 (find-symbol* '#:machine-maker-tests :machine-maker/tests))))
