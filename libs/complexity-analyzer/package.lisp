(defpackage :complexity-analyzer
  (:use :cl :machine-description)
  (:documentation "Time complexity analyzer for Turing machines.

This module provides a simple API to analyze the time complexity of a Turing machine
using a hybrid approach combining structural analysis (graph patterns) and dynamic
analysis (runtime behavior).

MAIN API:
  (analyze-time-complexity machine history input-size initial-tape-length)
  
  Returns a complexity-result with:
  - complexity: keyword (:o1, :on, :on2, etc.)
  - confidence: :high, :medium, or :low
  - total-steps: total steps executed
  - steps-per-n: ratio steps/n
  - steps-per-n2: ratio steps/n²")
  (:export
   ;; Complexity types:
   :time-complexity
   :+o1+ :+olog-n+ :+on+ :+on-log-n+ :+on2+ :+o2n+ :+on-fact+
   
   ;; Result structure:
   :complexity-result
   :result-complexity
   :result-confidence
   :result-total-steps
   :result-input-size
   :result-steps-per-n
   :result-steps-per-n2
   
   ;; Main API function:
   :analyze-time-complexity
   
   ;; Utility:
   :format-complexity))
