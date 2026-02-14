(defpackage :machine-description/tests
  (:use :cl :fiveam :machine-description)
  (:import-from
   :machine-description
   ;; conditions
   :json-parsing-error
   :invalid-json
   :invalid-machine-description-args
   :invalid-transition-result-args
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
   :process-name
   :process-alphabet
   :process-blank
   :process-states
   :process-initial-state
   :process-finals
   :process-transitions)
  (:export :machine-description-tests))
