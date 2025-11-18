(in-package :utils/tests-pkg)

(def-suite tests)
(in-suite tests)

(test read-file
	  (is (equal "foo~%" (utils-pkg:read-file))))
