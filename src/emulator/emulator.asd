(asdf:defsystem "emulator"
	:components ((:file "package")
				 (:file "emulate-turing-machine" :depends-on ("package"))
				 (:file "hardware" :depends-on ("package")))
	:in-order-to ((asdf:test-op (asdf:test-op "emulator/tests"))))

(asdf:defsystem "emulator/tests"
	:depends-on ("emulator" "fiveam")
	:components ((:module "tests"
				  :components ((:file "package")
							   (:file "tests" :depends-on ("package")))))
	:perform (asdf:test-op (o c) (uiop:symbol-call :fiveam :run! (find-symbol* '#:emulator-tests :emulator/tests-pkg))))
