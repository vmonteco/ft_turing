(in-package :ft_turing/tests-pkg)

(def-suite tests)
(in-suite tests)

(test hw
	  (is (equal "Hello world!~%" (ft_turing-pkg:hw))))
