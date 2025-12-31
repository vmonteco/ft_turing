(in-package :ft_turing/tests)

(def-suite ft_turing-tests)
(in-suite ft_turing-tests)
 
(test test-parse-args
	  ;; Basic case:
	  (is (equal (ft_turing::parse-args '("foo" "bar"))
				'("foo" "bar")))
	  ;; -h/--help cases:
	  (signals ft_turing::help-condition
			   (ft_turing::parse-args '("-h")))
	  (signals ft_turing::help-condition
			   (ft_turing::parse-args '("--help")))
	  (signals ft_turing::help-condition
			   (ft_turing::parse-args '("-h" "foo")))
	  (signals ft_turing::help-condition
			   (ft_turing::parse-args '("--help" "foo")))
	  (signals ft_turing::help-condition
			   (ft_turing::parse-args '("-h" "foo" "bar")))
	  (signals ft_turing::help-condition
			   (ft_turing::parse-args '("--help" "foo" "bar")))
	  (signals ft_turing::help-condition
			   (ft_turing::parse-args '("foo" "-h" "bar")))
	  (signals ft_turing::help-condition
			   (ft_turing::parse-args '("foo" "--help" "bar")))
	  ;; Usage error cases:
	  (signals ft_turing::usage-error
			   (ft_turing::parse-args nil))
	  (signals ft_turing::usage-error
			   (ft_turing::parse-args '("foo")))
	  (signals ft_turing::usage-error
			   (ft_turing::parse-args '("-help")))
	  (signals ft_turing::usage-error
			   (ft_turing::parse-args '("foo" "bar" "xyz"))))
