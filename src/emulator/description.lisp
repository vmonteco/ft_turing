(deftype 9
	

(defclass state ()
  ((name
	:initarg			:name
	

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
