(asdf:defsystem "cli"
	:components ((:file "package")
				 (:file "args" :depends-on ("package"))
                 (:file "read-json" :depends-on ("package")))
    :in-order-to ((asdf:test-op (asdf:test-op "cli/tests"))))

(asdf:defsystem "cli/tests"
	:depends-on ("cli" "fiveam")
	:components ((:module "tests"
				  :components ((:file "package")
							   (:file "tests" :depends-on ("package")))))
	:perform (asdf:test-op (o c) (uiop:symbol-call :fiveam :run! (find-symbol* '#:cli-tests :cli/tests-pkg))))
