(asdf:defsystem "hardware"
  :components ((:file "package")
			   (:file "hardware" :depends-on ("package")))
  :in-order-to ((asdf:test-op (asdf:test-op "hardware/tests"))))

(asdf:defsystem "hardware/tests"
  :depends-on ("hardware" "fiveam")
  :components ((:module "tests"
				:components ((:file "package")
							 (:file "tests" :depends-on ("package")))))
  :perform (asdf:test-op
	(o c)
	(uiop:symbol-call
	 :fiveam :run!
	 (find-symbol* '#:hardware-tests :hardware/tests))))
