(in-package :ft_turing/tests-pkg)

(def-suite ft_turing-tests)
(in-suite ft_turing-tests)

(test hw
	  (is (equal "Hello world!~%" (ft_turing-pkg:hw))))
