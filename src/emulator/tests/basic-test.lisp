(in-package :emulator/tests-pkg)

(def-suite tests)
(in-suite tests)

(test read-file
	  (is (equal "foo~%" (emulator-pkg:emulate-turing-machine))))
