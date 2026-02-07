(defpackage :machine-description
  (:use :cl)
  (:intern
   ;; Conditions:
   :invalid-json-alphabet
   :invalid-json-blank
   :invalid-json-states
   :invalid-json-initial
   :invalid-json-finals
   :invalid-json-transitions
   :invalid-json-missing-from-state
   :invalid-json-missing-from-char
   :invalid-json-missing-to-state
   :invalid-json-missing-to-state
   :invalid-json-missing-action
   :invalid-json-from-char
   :invalid-json-from-state
   :invalid-json-to-char
   :invalid-json-to-state
   :invalid-json-action
   :invalid-machine-description
   :invalid-name
   :invalid-alphabet
   :invalid-blank
   :invalid-states
   :invalid-initial-state
   :invalid-finals
   :invalid-transitions
   ;; machine-description
   :name
   :alphabet
   :states
   :blank
   :initial-state
   :finals
   :transitions
   ;; transition-result
   :transition-result
   :to-state
   :to-char
   :action
   :transition-result-equal
   ;; Process functions
   :process-alphabet
   :process-blank
   :process-states
   :process-initial
   :process-finals
   :process-transitions
   :format-machine-description)
  (:export
   ;; Conditions
   :invalid-json
   :machine-description
   :make-machine-description-from-json))
