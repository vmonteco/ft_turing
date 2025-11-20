(in-package :utils/tests-pkg)

(def-suite utils-tests)
(in-suite utils-tests)

(test read-file
	  (is (equal "foo~%" (utils-pkg:read-file))))
