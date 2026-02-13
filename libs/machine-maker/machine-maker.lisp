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
						   (not (eq c ,(machine-description:blank machine-description)))))
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
				  (let ((state.char-tr (assoc from-state transitions-alist)))
					(unless state.char-tr
					  (signal 'machine-runtime-error
							  :from-state from-state
							  :from-char from-char))
					(let ((char.tr (assoc from-char (cdr state.char-tr))))
					  (unless char.tr
						(signal 'machine-runtime-error
								:from-state from-state
								:from-char from-char))
					  (cdr char.tr)))))
		   (labels ((run (hw state n)
					  "Recursively runs the machine steps.

It returns a list containing the hardware at the end, the final
state and the number of steps that were done."
					  ;; Check if the state is among the finals
					  (let ((from-state state)
							(from-char (hardware:read-head hw)))
						(if (member from-state
									(list ,@(machine-description:finals machine-description)))
							(progn
							  (format-to-streams streams "~d: ~A Reached state ~A.~%" n hw from-state)
							  (list hw state n))
							(destructuring-bind (to-state to-char action)
								(get-tr from-state from-char)
							  (format-to-streams streams "~d: ~A (~A, ~A) -> (~A, ~A, ~A)~%"
												 n
												 hw
												 from-state from-char
												 to-state to-char action)
							  (run (funcall (if (eq action :right)
												#'hardware:move-right
												#'hardware:move-left)
											(hardware:write-head hw
																 to-char))
								   to-state
								   (1+ n)))))))
			 (run (hardware:init-hardware input)
				  ,(initial-state machine-description)
				  0)))))))
