(defpackage :utils/tests
  (:use :cl :fiveam :utils)
  (:import-from :utils
   :utils-sets-hasduplicatesp
   :utils-sets-setp
   :utils-sets-nonemptysetp
   :utils-alist-p
   :utils-typed-alist-p)
  (:export :utils-tests))
