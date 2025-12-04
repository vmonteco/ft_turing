(in-package emulator)

(defclass description ()
  ((name
	:initarg			:name
	:type				string
	:accessor			machine-name)
   (alphabet
	:initarg			:name
	:type				alphabet
	:accessor			machine-alphabet)
   (states
	:initarg			:states
	:type
	:accessor			machine-states)
   (initial-state
	:initarg			:initial
	:type				state
	:accessor			machine-initial-state)
   (finals
	:initarg			:final
	:type
	:accessor			machine-final-states)
   (transitions
	:initarg			:transitions
	:type
	:accessor			machine-transitions9
	))
  (:documentation "A Turing machine description"))

(defgeneric initialize-instance :after (d description)
  (format t "Description ~A created~%" d))


(defun json-to-description (json)
  (make-instance 'description
				 :name (gethash "name" name)
				 :alphabet alphabet
				 :blank blank
				 :states states
				 :initial initial
				 :finals finals
				 :transitions transitions))

(defun build-machine (description)
  
  )
