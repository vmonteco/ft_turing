(in-package :json-parser/tests-pkg)

(def-suite json-parser-tests)
(in-suite json-parser-tests)

(test parse-json
	  (is (equal "foo~%" (json-parser-pkg:parse-json))))
