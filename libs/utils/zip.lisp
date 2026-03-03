(in-package :utils)

(defun append-elem (l e)
  (reverse (cons e (reverse l))))

(defun zip (lists &key (combiner #'append-elem))
  (reduce (lambda (a b) (mapcar combiner a b)) lists
		  :initial-value (make-list
						  (reduce #'max
								  (mapcar #'length lists))
						  :initial-element ())))
