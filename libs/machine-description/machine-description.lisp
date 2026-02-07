(in-package :machine-description)

;;; A class to describe a turing machine.
(defclass machine-description ()
	((name
	  :initarg :name
	  :type :string
	  :accessor name
	  :documentation "The name of a machine is a non-empty string.")
	 (alphabet
	  :initarg :alphabet
	  :type list
	  :accessor alphabet
	  :documentation
	  "The alphabet of a machine is a non-empty set of characters.")
	 (blank
	  :initarg :blank
	  :type character
	  :accessor blank
	  :documentation "An element of alphabet")
	 (states
	  :initarg :states
	  :type list
	  :accessor states
	  :documentation
	  "The states of the machine is a non-empty set of symbols.")
	 (initial-state
	  :initarg :initial-state
	  :type symbol
	  :accessor initial-state
	  :documentation
	  "The initial state is an element of the states slot.")
	 (finals
	  :initarg :finals
	  :type list
	  :accessor finals
	  :documentation
	  "The set of final states is a non-empty subset of states.")
	 (transitions
	  :initarg :transitions
	  :type hashtable
	  :accessor transitions
	  :documentation ""))
  (:documentation "A Turing machine description"))

(defmethod initialize-instance :before ((obj machine-description)
										&key
										  name
										  alphabet
										  blank
										  states
										  initial-state
										  finals
										  transitions)
  ;; Checks here
  ;; name
  ;; Non-string name
  (unless (stringp name) (error 'invalid-name))
  (unless (> (length name) 0) (error 'invalid-name))
  ;; Alphabet
  (unless (and (utils:utils-sets-nonemptysetp alphabet)
			   (every #'characterp alphabet))
	(error 'invalid-alphabet))
  ;; Blank
  (unless (member blank alphabet) (error 'invalid-blank))
  ;; States
  (unless (and (utils:utils-sets-nonemptysetp states)
			   (every #'symbolp states))
	(error 'invalid-states))
  ;; Initial state
  (unless (member initial-state states)
	(error 'invalid-initial-state))
  ;; Finals:
  (unless (and (utils:utils-sets-nonemptysetp finals)
			   (subsetp finals states))
	(error 'invalid-finals))

  ;; transitions
  (unless
	  (utils:utils-typed-alist-p
	   transitions
	   (lambda (c) (member c states))
	   (lambda (c) (utils:utils-typed-alist-p
					c
					(lambda (c) (member c alphabet))
					(lambda (c) (and (typep c 'transition-result)
									 (member (to-state c) states)
									 (member (to-char c) alphabet)
									 (member (action c) '(:left :right)))))))
	(error 'invalid-transitions)))


(defun make-machine-description-from-json (json)
  "Build machine description instance from JSON string."
  (handler-case
	  (let* ((hashtable (com.inuoe.jzon:parse json))
			 (name (utils:gethash-or-signal hashtable "name" 'invalid-json))
			 (alphabet (process-alphabet
						(utils:gethash-or-signal hashtable "alphabet" 'invalid-json)))
			 (blank (process-blank
					 (utils:gethash-or-signal hashtable "blank" 'invalid-json)))
			 (states (process-states
					  (utils:gethash-or-signal hashtable "states" 'invalid-json)))
			 (initial-state (process-initial
							 (utils:gethash-or-signal hashtable "initial" 'invalid-json)))
			 (finals (process-finals
					  (utils:gethash-or-signal hashtable "finals" 'invalid-json)))
			 (transitions (process-transitions
						   (utils:gethash-or-signal hashtable
													"transitions"
													'invalid-json))))
		(make-instance 'machine-description
					   :name name
					   :alphabet alphabet
					   :blank blank
					   :states states
					   :initial-state initial-state
					   :finals finals
					   :transitions transitions))
	;; Workaround on a JZON bug whose fix has not yet been released.
	(type-error (c)
	  (signal com.inuoe.jzon:json-parse-error))))
