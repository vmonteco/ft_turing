(in-package :bonus)

(defparameter *enable-analysis* t)
(defparameter *enable-plotting* nil)

(defun dummy_heuristic (results)
  '(:upper-bound (lambda (x) (expt x 3))
	:lower-bound (lambda (x) (expt x 2))))

(defun analyse (db-path machine-name)
  ;; Create tables if necessary
  (create-tables-if-not-exists db-path)
  ;; Gather data
  (let ((results (get-machine-results db-path machine-name)))
	(format t "~A~%" (make-string 80 :initial-element #\*))
	(format t "Results: ~A~%" (utils:zip results))
	(when *enable-plotting*
	  ;; (kai:line x y)
	  ;; (kai:show)
	  )))

(defun store-result (db-path
					 machine-description
					 md5sum
					 input
					 steps-number
					 hw
					 history)
  ;; Create tables
  (create-tables-if-not-exists db-path)
  ;; Create machine
  (create-or-replace-machine db-path machine-description md5sum)
  ;; Create result
  (insert-result-if-not-exists db-path
							   (machine-description:name machine-description)
							   input
							   hw
							   history
							   (caar (reverse history))
							   steps-number))
