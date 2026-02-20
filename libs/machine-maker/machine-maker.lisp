(in-package :machine-maker)

(define-condition machine-invalid-input (error)
  ((input :initarg :input
		  :initform nil
		  :reader input)
   (alphabet :initarg :alphabet
			 :initform nil
			 :reader alphabet)
   (blank :initarg :blank
		  :initform nil
		  :reader blank))
  (:documentation "Error when the input isn't valid")
  (:report (lambda (condition stream)
			 (format stream
					 "Invalid input ~S: should only be made of characters among: ~S."
					 (input condition)
					 (remove-if (lambda (c) (eq c (blank condition)))
								(alphabet condition))))))
(define-condition machine-runtime-error (error)
  ((from-state :initarg :from-state
			   :initform nil
			   :reader from-state)
   (from-char :initarg :from-char
			  :initform nil
			  :reader from-char))
  (:documentation "Error when no transition is associated to current state and char.")
  (:report (lambda (condition stream)
			 (format stream
					 "No defined transition for state \"~A\" and character '~C'."
					 (from-state condition)
					 (from-char condition)))))

(defun make-machine-code (machine-description)
  "Build a the code for a Turing machine out of MACHINE-DESCRIPTION"
  `(lambda (input &key (streams '(*standard-output*)))
	 ,(format nil "The machine ~A"
			  (machine-description:name machine-description))
	 ;; Input checks.
	 (unless (every (lambda (c)
					  (and (member c (list ,@(machine-description:alphabet machine-description)))
						   (not (eql c ,(machine-description:blank machine-description)))))
					input)
	   (signal 'machine-invalid-input
			   :input input
			   :alphabet (list ,@(machine-description:alphabet machine-description))
			   :blank ,(machine-description:blank machine-description)))
	 (flet ((format-to-streams (streams control-string &rest other-keys)
			  (loop for s in streams do (apply #'format s control-string other-keys))))
	   (let ((hardware:*hw-blank* ,(machine-description:blank machine-description))
			 (transitions-alist
			   ',(map 'list
					  (lambda (l)
						(let ((from-state (car l)))
						  `(,from-state .
										,(map 'list
											  (lambda (m)
												(let ((from-char (car m))
													  (transition (cdr m)))
												  `(,from-char . (,(machine-description:to-state transition)
																  ,(machine-description:to-char transition)
																  ,(machine-description:action transition)))))
											  (cdr l)))))
					  (machine-description:transitions machine-description))))
		 (flet ((get-tr (from-state from-char)
				  (cdr (assoc from-char (cdr (assoc from-state transitions-alist))))))
		   (do*
			;; Loop over the following statements
			;; For each statement:
			;; - first element is the local variable name.
			;; - Second one is the initialization form.
			;;  - Third one is the iteration form.
			((number-of-steps 0 (1+ number-of-steps))
			 (hardware (hardware:init-hardware input)
					   (funcall (if (eql action :right)
									#'hardware:move-right
									#'hardware:move-left)
								(hardware:write-head hardware
													 to-char)))
			 (from-state ,(initial-state machine-description) to-state)
			 (from-char (hardware:read-head hardware) (hardware:read-head hardware))
			 (history (list (cons from-state from-char)) (cons (cons from-state from-char) history))
			 (transition (get-tr from-state from-char) (get-tr from-state from-char))
			 (to-state (first transition) (first transition))
			 (to-char (second transition) (second transition))
			 (action (third transition) (third transition)))
			((member from-state (list ,@(machine-description:finals machine-description))) ; End condition
			 ;; Return value
			 (progn (format-to-streams streams "~4d: ~A Reached state ~A.~%"
									   number-of-steps
									   hardware
									   to-state)
					(list hardware (reverse history) number-of-steps)))
			 ;; Body for each iteration
			 (unless transition
			   (signal 'machine-runtime-error
					   :from-state from-state
					   :from-char from-char))
			 (format-to-streams streams "~4d: ~A (~A, ~A) -> (~A, ~A, ~A)~%"
								number-of-steps
								hardware
								from-state from-char
								to-state to-char action)))))))
