(in-package :utils/tests)

(def-suite utils-sets-tests :in utils-tests)
(in-suite utils-sets-tests)

;; NB: circular lists aren't handled.

(test utils-sets-hasduplicatesp-tests
	  (is-false (utils-sets-hasduplicatesp '(1 2 3)))
	  (is-false (utils-sets-hasduplicatesp '()))
	  (is (utils-sets-hasduplicatesp '(1 1)))
	  (is-false (utils-sets-hasduplicatesp '("foo" "foo")))
	  (is (utils-sets-hasduplicatesp '("foo" "foo") :test #'equal))
	  (is-false (utils-sets-hasduplicatesp '("foo" "bar") :test #'equal))
	  (signals type-error (utils-sets-hasduplicatesp "foo"))) ; Expects a list.

(test utils-sets-setp-tests
	  (is (utils-sets-setp ()))
	  (is (utils-sets-setp '(1)))
	  (is (utils-sets-setp '(1 2 3)))
	  (is-false (utils-sets-setp '(1 1)))
	  (is (utils-sets-setp '("foo" "foo")))
	  (is-false (utils-sets-setp '("foo" "foo") :test #'equal))
	  (is (utils-sets-setp '("foo" "bar") :test #'equal))
	  (is-false (utils-sets-setp "foo")))

(test utils-sets-nonemptysetp-tests
	  (is-false (utils-sets-nonemptysetp ()))
	  (is (utils-sets-nonemptysetp '(1)))
	  (is (utils-sets-nonemptysetp '(1 2 3)))
	  (is-false (utils-sets-nonemptysetp '(1 1)))
	  (is (utils-sets-nonemptysetp '("foo" "foo")))
	  (is-false (utils-sets-nonemptysetp '("foo" "foo") :test #'equal))
	  (is (utils-sets-nonemptysetp '("foo" "bar") :test #'equal))
	  (is-false (utils-sets-nonemptysetp "foo")))
