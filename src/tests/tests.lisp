(in-package :ft_turing/tests-pkg)

(def-suite ft_turing-tests)
(in-suite ft_turing-tests)
 
(test test-parse-args
	  ;; Basic case:
	  (is (equal (ft_turing-pkg::parse-args '("foo" "bar"))
				'("foo" "bar")))
	  ;; -h/--help cases:
	  (signals ft_turing-pkg::help-condition
			   (ft_turing-pkg::parse-args '("-h")))
	  (signals ft_turing-pkg::help-condition
			   (ft_turing-pkg::parse-args '("--help")))
	  (signals ft_turing-pkg::help-condition
			   (ft_turing-pkg::parse-args '("-h" "foo")))
	  (signals ft_turing-pkg::help-condition
			   (ft_turing-pkg::parse-args '("--help" "foo")))
	  (signals ft_turing-pkg::help-condition
			   (ft_turing-pkg::parse-args '("-h" "foo" "bar")))
	  (signals ft_turing-pkg::help-condition
			   (ft_turing-pkg::parse-args '("--help" "foo" "bar")))
	  (signals ft_turing-pkg::help-condition
			   (ft_turing-pkg::parse-args '("foo" "-h" "bar")))
	  (signals ft_turing-pkg::help-condition
			   (ft_turing-pkg::parse-args '("foo" "--help" "bar")))
	  ;; Usage error cases:
	  (signals ft_turing-pkg::usage-error
			   (ft_turing-pkg::parse-args nil))
	  (signals ft_turing-pkg::usage-error
			   (ft_turing-pkg::parse-args '("foo")))
	  (signals ft_turing-pkg::usage-error
			   (ft_turing-pkg::parse-args '("-help")))
	  (signals ft_turing-pkg::usage-error
			   (ft_turing-pkg::parse-args '("foo" "bar" "xyz"))))
