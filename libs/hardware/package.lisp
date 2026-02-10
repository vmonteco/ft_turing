(defpackage :hardware
  (:nicknames "hw")
  (:documentation "The hardware package provides the modelization of the \
infinite tape and reading/writing head necessary for implementing a Turing \
machine.")
  (:use :cl)
  (:intern
   :head
   :right
   :left
   :hardware)
  (:export
   ;; We only need that for creating an instance:
   :init-hardware
   ;; Those are the only thing the turing machine will need.
   :read-head
   :write-head
   :move-right
   :move-left
   :*hw-blank*))
