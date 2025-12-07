(in-package :ft_turing-pkg)

(defun hw ()
  "Hello world!~%")

(defun main ()
  (format t (hw))
  (format t (read-file))
  (format t (emulate-turing-machine)))
