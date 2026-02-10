(in-package :machine-maker)

(define-condition machine-invalid-input (error) ())
(define-condition machine-runtime-error (error) ())

(defun make-lambda (from-state from-char transition)
  "Make the lambda for a step.

The lambda will display the line and return the modified hardware
and state (multiple values)."
  `(lambda (hw)
	 ;; Display step line
	 (format t ,(format nil "~~A ~A~~%"
						(machine-description:format-step from-state
														 from-char
														 transition))
			 hw)
	 ,(if (eq (machine-description:action transition)
			  :right)
		  `(hardware:move-right (hardware:write-head hw ,(to-char transition)))
		  `(hardware:move-left (hardware:write-head hw ,(to-char transition))))))

(defun make-machine-code (machine-description)
  "Build a the code for a Turing machine out of MACHINE-DESCRIPTION"
  `(lambda (input)

	 ,(format nil "The machine ~A"
			  (machine-description:name machine-description))

	 ;; Input checks.
	 (unless (every (lambda (c)
					  (and (member c (list ,@(alphabet machine-description)))
						   (not (eq c ,(blank machine-description)))))
					input)
	   (signal 'machine-invalid-input))
	 (let ((hardware:*hw-blank* ,(blank machine-description)))
	   (flet ((get-result (from-state from-char)
				(cond
				  ,@(utils:flatten
					 nil
					 (loop for state.char-tr in (machine-description:transitions machine-description)
						   collect
						   (let ((from-state (car state.char-tr)))
							 (loop for char.tr in (cdr state.char-tr)
								   collect
								   (let ((from-char (car char.tr))
										 (transition (cdr char.tr)))
									 `((equal (list from-state from-char)
											  (list ,from-state ,from-char))
									   (values
										,(make-lambda from-state from-char transition)
										,(machine-description:to-state transition))))))))
				  (t (signal 'machine-runtime-error)))))
		 (labels
			 ((run (hw state n)
				"Recursively runs the machine steps.

It returns a list containing the hardware at the end, the final
state and the number of steps that were done."
				;; Check if the state is among the finals
				(if (member state
							(list ,@(machine-description:finals machine-description)))
					(list hw state n)
					(multiple-value-bind (get-new-hw new-state)
						(get-result state (hardware:read-head hw))
					  (run (funcall get-new-hw hw)
						   new-state 
						   (1+ n))))))
		   (run (hardware:init-hardware input)
				,(initial-state machine-description)
				1))))))
