(defpackage :machine-description
  (:use :cl)
  (:export
   ;; Basic types
   :machine-description
   :transition-result
   ;; machine-descriptiion mechods:
   :name
   :alphabet
   :blank
   :initial-state
   :finals
   :transitions
   ;; transition-result methods:
   :to-state
   :to-char
   :action
   ;; format functions:
   :format-step
   ;; Special constructor
   :make-machine-description-from-json
   ;; Basic conditions
   :json-parsing-error
   :invalid-json						; For both missing and invalid values
   :invalid-machine-description-args	; For instantiation errors
   :invalid-transition-result-args
   ))
