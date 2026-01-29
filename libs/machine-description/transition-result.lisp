(in-package :machine-description)

(defclass transition-result ()
  ((to-state
    :initarg :to-state
    :type :symbol
    :accessor to-state
    :documentation "The machine state following this transition.")
   (to-char
    :initarg :to-char
    :type :char
    :accessor to-char
    :documentation "The char that will be written by this transition.")
   (action
    :initarg :action
    :type :symbol
    :accessor action
    :documentation "The direction the head will move to after this transition.\
Either :right or :left."))
  (:documentation "The depiction of a Turing machine transition."))

(defmethod initialize-instance :before ((obj transition-result)
										&key to-char to-state action)
  (unless (or (equal action :right) (equal action :left))
	(signal 'invalid-action-error))
  (unless (characterp to-char) (signal 'invalid-to-char))
  (unless (symbolp to-state) (signal 'invalid-to-state)))

(defmethod transition-result-equal (a b)
  (and (equal (to-state a) (to-state b))
	   (equal (to-char a) (to-char b))
	   (equal (action a) (action b))))
