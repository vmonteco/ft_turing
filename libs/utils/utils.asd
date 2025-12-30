(asdf:defsystem "utils"
	:components ((:file "package"))
	:in-order-to ((asdf:test-op (asdf:test-op "utils/tests"))))

(asdf:defsystem "utils/tests"
	:depends-on ("utils" "fiveam")
	:components ((:module "tests"
				  :components ((:file "package")
							   (:file "tests" :depends-on ("package")))))
	:perform (asdf:test-op (o c) (uiop:symbol-call :fiveam :run! (find-symbol* '#:utils-tests :utils/tests))))
