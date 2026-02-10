(defpackage :machine-maker
  (:use :cl :machine-description :hardware)
  (:export
   #:machine-invalid-input
   #:machine-runtime-error
   #:make-machine-code))
