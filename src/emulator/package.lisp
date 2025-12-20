(defpackage :emulator-pkg
  (:use :cl)
  (:export
   :emulate-turing-machine
   :hardware
   :init-hardware
   :head
   :right
   :left
   :move-right
   :move-left
   :read-head
   :write-head
   :*hw-blank*))
