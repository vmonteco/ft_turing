(asdf:defsystem "ft_turing"
  :depends-on ("utils" "emulator" "com.inuoe.jzon")
  :components ((:file "package")
			   (:file "main" :depends-on ("package")))
  :build-operation program-op
  :build-pathname "../ft_turing"
  :entry-point "ft_turing-pkg:main"
  ;; The `asdf:test-op` operation is what will be triggered by the
  ;; (asdf:test-system :ft_turing) in the `test.lisp` script.
  ;;
  ;; Here, it associates this operation with the form
  ;; `(asdf:test-op "ft_turing/tests")` that will be evaluated when operating
  ;; "test-op" on this system (passing on the test-op operation to the system
  ;; "ft_turing/tests").
  :in-order-to ((asdf:test-op (asdf:test-op "utils/tests")
							  (asdf:test-op "emulator/tests")
							  (asdf:test-op "ft_turing/tests"))))

(asdf:defsystem "ft_turing/tests"
  ;; Apparently, :depends-on can apply to both systems and libraries.
  :depends-on ("ft_turing" "fiveam")
  :components ((:module "tests"
				  :components ((:file "package")
							   (:file "tests" :depends-on ("package")))))
  ;; This will "perform" the test-op operation by evaluating the form
  ;; `(uiop:symbol-call ...)`.
  :perform (asdf:test-op (o c) (uiop:symbol-call :fiveam :run! (find-symbol* '#:ft_turing-tests :ft_turing/tests-pkg))))
