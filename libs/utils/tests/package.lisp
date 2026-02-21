(defpackage :utils/tests
  (:use :cl :fiveam)
  (:import-from :utils
   #:utils-sets-hasduplicatesp
   #:utils-sets-setp
   #:utils-sets-nonemptysetp
   #:utils-sets-equal-p
   #:utils-alist-p
   #:utils-typed-alist-p
   #:truncate-or-complete-list)
  (:export #:utils-tests))
