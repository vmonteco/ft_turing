(defpackage :machine-description
  (:use :cl)
  (:intern
   :name
   :alphabet
   :states
   :initial-state
   :finals
   :transitions
   :transition-result
   :to-state
   :to-char
   :action
   :transition-result-equal)
  (:export
   :machine-description))
