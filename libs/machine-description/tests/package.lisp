(defpackage :machine-description/tests
  (:use :cl :fiveam :machine-description)
  (:import-from
   :machine-description
   :transition-result
   :to-state
   :to-char
   :action
   :transition-result-equal)
  (:export :machine-description-tests))
