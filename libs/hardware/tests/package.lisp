(defpackage :hardware/tests
  (:use :cl :fiveam :hardware)
  ;; Those aren't exported by hardware. :import-from makes us
  ;; able to use them without the hardware:: prefix in our tests.
  (:import-from :hardware
   #:hardware
   #:*hw-blank*
   #:head
   #:right
   #:left)
  (:export #:hardware-tests))
