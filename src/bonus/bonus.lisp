(in-package :bonus)

(defun plot (db-path machine-name)
  (create-tables-if-not-exists db-path)
  (let ((resuts (get-results machine-name)))
	(zip results)
	(kai:line x y)
	(kai:show)))

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
  (add-result db-path
			  machine-name
			  input
			  steps-number
			  hw
			  history))
