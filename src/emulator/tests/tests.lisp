(in-package :emulator/tests-pkg)

(def-suite emulator-tests)
(in-suite emulator-tests)

(test read-file
	  (is (equal "foo~%" (emulator-pkg:emulate-turing-machine))))
