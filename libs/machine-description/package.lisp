(defpackage :machine-description
  (:use :cl)
  (:intern
   ;; machine-description
   :name
   :alphabet
   :states
   :initial-state
   :finals
   :transitions
   :invalid-machine-description
   :invalid-name
   :invalid-alphabet
   :invalid-blank
   :invalid-states
   :invalid-initial-state
   :invalid-finals
   :invalid-transitions
   ;; transition-result
   :transition-result
   :to-state
   :to-char
   :action
   :transition-result-equal)
  (:export
   :machine-description))
