(in-package :machine-description)

(defparameter +header-width+ 80)
(defparameter +header-char+ #\*)

(defun format-as-header (string
						 &key
						   (char +header-char+)
						   (width +header-width+))
  "Embed STRING into a header"
  (let ((full-line (make-string width :initial-element char))
		(hollow-line (format nil "~C~A~C"
							 char
							 (make-string (- width 2)
										  :initial-element #\Space)
							 char))
		(string-format-string
		  (let* ((string-len (length string))
				 (space-len-1 (floor (max (- width string-len 2) 0) 2))
				 (space-len-2 (max (- width string-len space-len-1 2) 0)))
			(format nil "~C~A~A~A~C"
					char
					(make-string space-len-1 :initial-element #\Space)
					string
					(make-string space-len-2 :initial-element #\Space)
					char))))
	(concatenate 'string
				 full-line (format nil "~%")
				 hollow-line (format nil "~%")
				 (format nil string-format-string string) (format nil "~%")
				 hollow-line (format nil "~%")
				 full-line)))

(defun footer (&key (char +header-char+) (width +header-width+))
  (make-string width :initial-element char))

(defun format-as-array (list)
  (format nil "[ ~{~A~^, ~} ]" list))

(defun format-transition (transition)
  (format nil "(~A, ~c, ~A)"
		  (to-state transition)
		  (to-char transition)
		  (action transition)))

(defun format-step (from-state from-char transition)
  (format nil "(~A, ~C) -> ~A"
		  from-state
		  from-char
		  (format-transition transition)))

(defun format-transitions (transitions)
	(let ((formatted-transitions
			(loop for state.char-tr in transitions
				  collect (loop for char.tr in (cdr state.char-tr)
								collect (format-step (car state.char-tr)
													 (car char.tr)
													 (cdr char.tr))))))
	  (format nil "~{~A~^~%~}" (utils:flatten nil formatted-transitions))))
  
(defun format-machine-description (md)
  (let ((head (format-as-header (name md)))
		(alphabet (format-as-array (alphabet md)))
		(blank (blank md))
		(states (format-as-array (states md)))
		(initial (initial-state md))
		(finals (format-as-array (finals md)))
		(transitions (format-transitions (transitions md))))
	(format nil "~A
Alphabet: ~A
Blank character: ~A
States: ~A
Initial: ~A
Finals: ~A
~A
~A"
			head
			alphabet
			blank
			states
			initial
			finals
			transitions
			(footer))))
