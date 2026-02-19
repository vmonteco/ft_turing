(defpackage :utils
  (:use :cl)
  (:export
   #:gethash-or-signal
   #:utils-sets-hasduplicatesp
   #:utils-sets-setp
   #:utils-sets-nonemptysetp
   #:utils-sets-equal-p
   #:utils-alist-p
   #:utils-typed-alist-p
   #:flatten))
