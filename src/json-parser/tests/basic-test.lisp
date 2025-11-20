(in-package :json-parser/tests-pkg)

(def-suite tests)
(in-suite tests)

(test parse-json
	  (is (equal "foo~%" (json-parser-pkg:parse-json))))
