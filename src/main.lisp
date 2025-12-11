(in-package :ft_turing-pkg)

(defun hw ()
  "Hello world!~%")

(defun main ()
  (format t "~A~%" (com.inuoe.jzon:parse "{}"))
  (format t (emulate-turing-machine)))
