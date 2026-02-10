(defpackage :machine-description
  (:use :cl)
  (:export
   ;; Basic types
   :machine-description
   :transition-result
   ;; Special constructor
   :make-machine-description-from-json
   ;; Basic conditions
   :json-parsing-error
   :invalid-json						; For both missing and invalid values
   :invalid-machine-description-args	; For instantiation errors
   :invalid-transition-result-args
   ))
