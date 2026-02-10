(in-package :utils)

(defun truncate-or-complete-list (list n e)
  "Return a truncated version of LIST if longer than N.
omplete it with E if too short to make it N-long"
  (let ((element-to-add (if list (car list) e)))
	(if (zerop n)
		()
		(cons element-to-add
			  (truncate-or-complete-list (cdr list)
										 (1- n)
										 e)))))
