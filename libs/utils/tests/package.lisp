(defpackage :utils/tests
  (:use :cl :fiveam :utils)
  (:import-from :utils
   :utils-sets-hasduplicatesp
   :utils-sets-setp
   :utils-sets-nonemptysetp)
  (:export :utils-tests))
