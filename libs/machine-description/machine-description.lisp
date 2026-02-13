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
  (unless (stringp name)
	(signal 'invalid-machine-description-name))
  (unless (> (length name) 0)
	(signal 'invalid-machine-description-name))
  ;; Alphabet
  (unless (and (utils:utils-sets-nonemptysetp alphabet)
			   (every #'characterp alphabet))
	(signal 'invalid-machine-description-alphabet))
  ;; Blank
  (unless (member blank alphabet)
	(signal 'invalid-machine-description-blank
			:blank blank
			:alphabet alphabet))
  ;; States
  (unless (and (utils:utils-sets-nonemptysetp states)
			   (every #'symbolp states))
	(signal 'invalid-machine-description-states))
  ;; Initial state
  (unless (member initial-state states)
	(signal 'invalid-machine-description-initial-state
			:initial-state initial-state
			:states states))
  ;; Finals:
  (unless (and (utils:utils-sets-nonemptysetp finals)
			   (subsetp finals states))
	(signal 'invalid-machine-description-finals
			:finals finals
			:states states))

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
	(signal 'invalid-machine-description-transitions)))

(defun make-machine-description-from-json (json)
  "Build machine description instance from JSON string."
  (handler-case
	  (let ((hashtable
			  ;; Workaround for a JZON bug whose fix isn't deployed yet.
			  (handler-case (com.inuoe.jzon:parse json)
				(type-error (c)
				  (signal 'json-parsing-error
						  :msg (format
								nil
								"Workaround of JZON type-error bug (file probably empty):~%~A"
								c))))))
		(unless (hash-table-p hashtable)
		  (signal 'invalid-json-not-an-object))
		;; Raw values from parsed JSON
		;; Can signal invalid-json conditions for missing fields.
		(let* ((raw-name
				 (utils:gethash-or-signal hashtable
										  "name"
										  'invalid-json-missing-name))
			   (raw-alphabet
				 (utils:gethash-or-signal hashtable
										  "alphabet"
										  'invalid-json-missing-alphabet))
			   (raw-blank
				 (utils:gethash-or-signal hashtable
										  "blank"
										  'invalid-json-missing-blank))
			   (raw-states
				 (utils:gethash-or-signal hashtable
										  "states"
										  'invalid-json-missing-states))
			   (raw-initial-state
				 (utils:gethash-or-signal hashtable
										  "initial"
										  'invalid-json-missing-initial-state))
			   (raw-finals
				 (utils:gethash-or-signal hashtable
										  "finals"
										  'invalid-json-missing-finals))
			   (raw-transitions
				 (utils:gethash-or-signal hashtable
										  "transitions"
										  'invalid-json-missing-transitions))

			   ;; Processing values
			   ;; Can signal invalid-json for type error
			   (transitions (process-transitions raw-transitions))
			   (name (process-name raw-name))
			   (alphabet (process-alphabet raw-alphabet))
			   (blank (process-blank raw-blank))
			   (states (process-states raw-states))
			   (initial-state (process-initial-state raw-initial-state))
			   (finals (process-finals raw-finals)))

		  ;; Can signal invalid-machine-finition
		  (make-instance 'machine-description
						 :name name
						 :alphabet alphabet
						 :blank blank
						 :states states
						 :initial-state initial-state
						 :finals finals
						 :transitions transitions)))

	;; Workaround on a JZON bug whose fix has not yet been released.
	(com.inuoe.jzon:json-parse-error (c)
	  (signal 'json-parsing-error :msg c))))
