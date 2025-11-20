(asdf:defsystem "json-parser"
	:components ((:file "package")
				 (:file "parse-json" :depends-on ("package")))
	:in-order-to ((asdf:test-op (asdf:test-op "json-parser/tests"))))

(asdf:defsystem "json-parser/tests"
	:depends-on ("json-parser" "fiveam")
	:components ((:module "tests"
				  :components ((:file "package")
							   (:file "basic-test" :depends-on ("package")))))
	:perform (asdf:test-op (o c) (uiop:symbol-call :fiveam :run! (find-symbol* '#:tests :json-parser/tests-pkg))))
