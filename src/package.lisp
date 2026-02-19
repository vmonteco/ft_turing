(defpackage :ft_turing
  (:use :cl :utils :machine-maker)
  ;; Exporting :main would work too.
  ;; But it would also intern (create) a new keyword symbol.
  (:export #:main))
