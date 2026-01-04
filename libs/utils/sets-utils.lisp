(in-package :utils)

;;; Utils for working with sets:

(defun utils-sets-hasduplicatesp (list &key (test #'eql)) ;; (test #'eql))
  "Return non-nil if LIST contains duplicate elements."
  (if list
	  (let ((head (car list))
			(tail (cdr list)))
		(or (member head tail :test test)
			(utils-sets-hasduplicatesp tail :test test)))))

(defun utils-sets-setp (obj &key (test #'eql))
  "Return non-nil if OBJ is a set (i-e a list without duplicate elements)."
  (and (listp obj)
	   (not (utils-sets-hasduplicatesp obj :test test))))

(defun utils-sets-nonemptysetp (obj &key (test #'eql))
  "Return non-nil if OBJ is a non-nil set."
  (and (utils-sets-setp obj :test test) obj))
