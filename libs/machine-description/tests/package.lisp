(defpackage :machine-description/tests
  (:use :cl :fiveam :machine-description)
  (:import-from
   :machine-description
   ;; conditions
   :invalid-json
   :invalid-json-alphabet
   :invalid-json-blank
   :invalid-json-states
   :invalid-json-initial
   :invalid-json-finals
   :invalid-json-transitions
   :invalid-json-missing-from-state
   :invalid-json-missing-from-char
   :invalid-json-missing-to-state
   :invalid-json-missing-to-char
   :invalid-json-missing-action
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
   :make-machine-description-from-json
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
   :process-transitions)
  (:export :machine-description-tests))
