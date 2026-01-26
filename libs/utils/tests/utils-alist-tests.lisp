(in-package :utils/tests)

(def-suite utils-alists-tests :in utils-tests)
(in-suite utils-sets-tests)

(test utils-alist-p-tests
  (is (utils-alist-p nil))
  (is-false (utils-alist-p 1))
  (is-false (utils-alist-p '(1 2 3)))
  (is-false (utils-alist-p '((1 . 2) 3)))
  (is (utils-alist-p '((1 . 2) (3 . 4)))))

(test utils-typed-alist-p-tests
  (is (utils-typed-alist-p nil #'numberp #'characterp))
  (is-false (utils-typed-alist-p '((#\1 . 1)) #'numberp #'characterp))
  (is-false (utils-typed-alist-p '((0 . #\0) (#\1 . 1)) #'numberp #'characterp))
  (is (utils-typed-alist-p '((0 . #\0) (1 . #\1)) #'numberp #'characterp)))
