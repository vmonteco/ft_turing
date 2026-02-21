(in-package :utils/tests)

(def-suite truncate-or-complete-list :in utils-tests)
(in-suite truncate-or-complete-list)

(test truncate-or-complete-list-tests
	  (is (null (truncate-or-complete-list () 0 42)))
	  (is (equal '(42 42 42 42) (truncate-or-complete-list () 4 42)))
	  (is (equal '(1 2 3 4) (truncate-or-complete-list '(1 2 3 4) 4 42)))
	  (is (equal '(1 2 42 42) (truncate-or-complete-list '(1 2) 4 42)))
	  (is (equal '(1 2 3 4) (truncate-or-complete-list '(1 2 3 4) 4 42)))
	  (is (null (truncate-or-complete-list '(1 2 3 4) 0 42))))
