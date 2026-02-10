(in-package :utils)

(defun flatten (res remaining-lists)
  "Flatten REMAINING-LISTS into RES"
  (if remaining-lists
	  (flatten (append res (car remaining-lists))
			   (cdr remaining-lists))
	  res))
