(asdf:defsystem "utils"
  :components ((:file "package")
			   (:file "sets-utils" :depends-on ("package"))
			   (:file "alists-utils" :depends-on ("package")))
  :in-order-to ((asdf:test-op (asdf:test-op "utils/tests"))))

(asdf:defsystem "utils/tests"
  :depends-on ("utils" "fiveam")
  :components ((:module "tests"
				:components ((:file "package")
							 (:file "tests" :depends-on ("package"))
							 (:file "sets-utils-tests"
							  :depends-on ("package" "tests"))
							 (:file "utils-alist-tests" :depends-on ("package")))))
  :perform (asdf:test-op
			(o c)
			(uiop:symbol-call
			 :fiveam :run! (find-symbol* '#:utils-tests
										 :utils/tests))))
