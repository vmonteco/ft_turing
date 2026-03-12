(in-package :utils)

(defun zip (lists &key (combiner #'concat))
  (reduce (lambda (a b)
            (mapcar combiner a b))
          lists))
