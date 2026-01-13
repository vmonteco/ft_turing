(defpackage :machine-maker
  (:nicknames "mm")
  (:documentation "This package mostly provides a higher order function to \
\"generate\" a Turing machine function from a machine description.")
  (:use :cl)
  (:depends-on :machine-description)
  (:import-from
   :machine-description
   :machine-description)
  (:export
   :make-machine))
  
