(asdf:defsystem "bonus"
  :depends-on ("sqlite" "kai")
  :components ((:file "package")
			   (:file "db-management" :depends-on ("package")))
  :in-order-to ((asdf:test-op (asdf:test-op "bonus/tests"))))

(asdf:defsystem "bonus/tests"
  :depends-on ("bonus" "fiveam")
  :components ((:module "tests"
				:components ((:file "package")
							 (:file "tests" :depends-on ("package")))))
  :perform (asdf:test-op
			(o c)
			(uiop:symbol-call
			 :fiveam :run!
			 (find-symbol* '#:bonus-tests
						   :bonus/tests))))
