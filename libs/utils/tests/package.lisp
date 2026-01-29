(defpackage :utils/tests
  (:use :cl :fiveam)
  (:import-from :utils
   :utils-sets-hasduplicatesp
   :utils-sets-setp
   :utils-sets-nonemptysetp
   :utils-sets-equal-pm
   :utils-alist-p
   :utils-typed-alist-p)
  (:export :utils-tests))
