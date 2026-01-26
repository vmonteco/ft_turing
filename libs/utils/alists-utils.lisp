(in-package :utils)

(defun utils-alist-p (obj)
  (and (listp obj)
	   (every #'consp obj)))

(defun utils-typed-alist-p (obj pred1 pred2)
  (and (utils-alist-p obj)
	   (every (lambda (c) (and (funcall pred1 (car c))
							   (funcall pred2 (cdr c)))) obj)))
