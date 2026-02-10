(in-package :machine-description)

;; Errors during JSON parsing
(define-condition json-parsing-error (error)
  ((msg :initarg :msg
		:initform "A parsing error occured"
		:reader msg))
  (:documentation "Error when JSON parsing fails")
  (:report (lambda (condition stream)
			 (format stream "JSON parsing error: ~A"
					 (msg condition)))))

;; Errors during retrieving of raw values
(define-condition invalid-json (error)
  ()
  (:documentation "Error when JSON is unfit for machine description"))

;; Error of JSON root type:
(define-condition invalid-json-not-an-object (invalid-json)
  ()
  (:documentation "Error when JSON isn't an object ({})")
  (:report (lambda (condition stream)
			 (format stream "Root of JSON isn't an object ({...})"))))

;; Errors during processing of raw values (type checking)
(define-condition invalid-json-name (invalid-json)
  ((msg :initarg :msg
		:initform "Invalid JSON name"
		:reader msg))
  (:documentation "Error for invalid name")
  (:report (lambda (condition stream)
			 (format stream "Invalid JSON name: ~A"
					 (msg condition)))))
(define-condition invalid-json-alphabet (invalid-json)
  ((msg :initarg :msg
		:initform "Invalid JSON alphabet"
		:reader msg))
  (:documentation "Error for invalid alphabet")
  (:report (lambda (condition stream)
 			 (format stream "Invalid JSON alphabet: ~A"
					 (msg condition)))))
(define-condition invalid-json-blank (invalid-json)
  ((msg :initarg :msg
		:initform "Invalid JSON blank"
		:reader msg))
  (:documentation "Error for invalid blank")
  (:report (lambda (condition stream)
 			 (format stream "Invalid JSON blank: ~A"
					 (msg condition)))))
(define-condition invalid-json-states (invalid-json)
  ((msg :initarg :msg
		:initform "Invalid JSON states"
		:reader msg))
  (:documentation "Error for invalid states")
  (:report (lambda (condition stream)
 			 (format stream "Invalid JSON states: ~A"
					 (msg condition)))))
(define-condition invalid-json-initial-state (invalid-json)
  ((msg :initarg :msg
		:initform "Invalid JSON initial state"
		:reader msg))
  (:documentation "Error for invalid initial state")
  (:report (lambda (condition stream)
			 (format stream "Invalid JSON initial state: ~A"
					 (msg condition)))))
(define-condition invalid-json-finals (invalid-json)
  ((msg :initarg :msg
		:initform "Invalid JSON finals"
		:reader msg))
  (:documentation "Error for invalid final states")
  (:report (lambda (condition stream)
			 (format stream "Invalid JSON finals: ~A"
					 (msg condition)))))
(define-condition invalid-json-transitions (invalid-json)
  ((msg :initarg :msg
		:initform "Invalid JSON transitions"
		:reader msg))
  (:documentation "Error for invalid transitions")
  (:report (lambda (condition stream)
			 (format stream "Invalid JSON transitions: ~A"
					 (msg condition)))))
(define-condition invalid-json-transition (invalid-json-transitions)
  ()
  (:documentation "Error for one invalid transition (for instance with invalid TR fields)"))


(define-condition invalid-json-missing-name (invalid-json)
  ()
  (:documentation "Error when parsed JSON lacks field name")
  (:report (lambda (condition stream)
			 (format stream "JSON object lacks a \"name\" field"))))
(define-condition invalid-json-missing-alphabet (invalid-json)
  ()
  (:documentation "Error when parsed JSON lacks field alphabet")
  (:report (lambda (condition stream)
			 (format stream "JSON object lacks an \"alphabet\" field"))))
(define-condition invalid-json-missing-blank (invalid-json)
  ()
  (:documentation "Error when parsed JSON lacks field blank")
  (:report (lambda (condition stream)
			 (format stream "Machine description lacks a \"blank\" field"))))
(define-condition invalid-json-missing-states (invalid-json)
  ()
  (:documentation "Error when parsed JSON lacks field states")
  (:report (lambda (condition stream)
			 (format stream "Machine description lacks a \"states\" field"))))
(define-condition invalid-json-missing-initial-state (invalid-json)
  ()
  (:documentation "Error when parsed JSON lacks field initial")
  (:report (lambda (condition stream)
			 (format stream "Machine description lacks a \"initial-state\" field"))))
(define-condition invalid-json-missing-finals (invalid-json)
  ()
  (:documentation "Error when parsed JSON lacks field finals")
  (:report (lambda (condition stream)
			 (format stream "Machine description lacks a \"finals\" field"))))
(define-condition invalid-json-missing-transitions (invalid-json)
  ()
  (:documentation "Error when parsed JSON lacks field transitions")
  (:report (lambda (condition stream)
			 (format stream "Machine description lacks a \"transitions\" field"))))

;; Those will be under an invalid-json-transition
(define-condition invalid-json-missing-from-char (invalid-json-transition)
  ()
  (:documentation "Error when a transition has no from-char field")
  (:report (lambda (condition stream)
			 (format stream "Some transition lacks the \"read\" field"))))
(define-condition invalid-json-missing-to-state (invalid-json-transition)
  ()
  (:documentation "Error when a transition has no to-state field")
  (:report (lambda (condition stream)
			 (format stream "Some transition lacks the \"to_state\" field"))))
(define-condition invalid-json-missing-to-char (invalid-json-transition)
  ()
  (:documentation "Error when a transition has no to-char field")
    (:report (lambda (condition stream)
			   (format stream "Some transition lacks the \"write\" field"))))
(define-condition invalid-json-missing-action (invalid-json-transition)
  ()
  (:documentation "Error when a transition has no action field")
  (:report (lambda (condition stream)
			 (format stream "Some transition lacks the \"action\" field"))))

(define-condition invalid-json-from-state (invalid-json-transition)
  ()
  (:documentation "Error when a transition has an invalid from-state"))

(define-condition invalid-json-from-char (invalid-json-transition)
  ((msg :initarg :msg
		:initform "Invalid \"read\" field"
		:reader msg))
  (:documentation "Error when a transition has an invalid from-char")
  (:report (lambda (condition stream)
			 (format stream "Invalid \"read\" field: ~A"
					 (msg condition)))))

(define-condition invalid-json-to-state (invalid-json-transition)
  ((msg :initarg :msg
		:initform "Invalid \"to_state\" field"
		:reader msg))
  (:documentation "Error when a transition has an invalid to-state")
  (:report (lambda (condition stream)
			 (format stream "Invalid \"to_state\" field: ~A"
					 (msg condition)))))

(define-condition invalid-json-to-char (invalid-json-transition)
  ((msg :initarg :msg
		:initform "Invalid \"write\" field"
		:reader msg))
  (:documentation "Error when a transition has an invalid to-char")
  (:report (lambda (condition stream)
			 (format stream "Invalid \"write\" field: ~A"
					 (msg condition)))))

(define-condition invalid-json-action (invalid-json-transition)
  ((msg :initarg :msg
		:initform "Invalid \"action\" field"
		:reader msg))
  (:documentation "Error when a transition has an invalid action")
  (:report (lambda (condition stream)
			 (format stream "Invalid \"write\" field: ~A"
					 (msg condition)))))

;; Errors during instantiation of machine description
(define-condition invalid-machine-description-args (error)
  ()
  (:documentation "Error when arguments for machine-description are inconsistent"))
(define-condition invalid-machine-description-name (invalid-machine-description-args)
  ()
  (:documentation "Error when trying to instantiate machine-description with invalid name"))
(define-condition invalid-machine-description-alphabet (invalid-machine-description-args)
  ()
  (:documentation "Error when trying to instantiate machine-description with invalid alphabet"))

(define-condition invalid-machine-description-blank (invalid-machine-description-args)
  ((blank :initarg :blank
		  :initform nil
		  :reader blank)
   (alphabet :initarg :alphabet
			 :initform nil
			 :reader alphabet))
  (:documentation "Error when trying to instantiate machine-description with invalid blank")
  (:report (lambda (condition stream)
			 (format stream "Blank ~S not part of alphabet ~S"
					 (blank condition)
					 (alphabet condition)))))

(define-condition invalid-machine-description-states (invalid-machine-description-args)
  ()
  (:documentation "Error when trying to instantiate machine-description with invalid states"))

(define-condition invalid-machine-description-initial-state (invalid-machine-description-args)
  ((initial-state :initarg :initial-state
				  :initform nil
				  :reader initial-state)
   (states :initarg :states
		   :initform nil
		   :reader states))
  (:documentation "Error when trying to instantiate machine-description with invalid initial state")
  (:report (lambda (condition stream)
			 (format stream "Initial state ~A not in states ~A"
					 (initial-state condition)
					 (states condition)))))

(define-condition invalid-machine-description-finals (invalid-machine-description-args)
  ((finals :initarg :finals
		   :initform nil
		   :reader finals)
   (states :initarg :states
		   :initform nil
		   :reader states))
  (:documentation "Error when trying to instantiate machine-description with invalid finals")
  (:report (lambda (condition stream)
			 (format stream "Finals ~A not a subset of states ~A"
					 (finals condition)
					 (states condition)))))

(define-condition invalid-machine-description-transitions (invalid-machine-description-args)
  ()
  (:documentation "Error when trying to instantiate machine-description with invalid transitions"))

;; For transition-result errors:
(define-condition invalid-transition-result-args (error)
  ()
  (:documentation "Error when trying to instantiate a transition-result with invalid args"))
(define-condition invalid-transition-result-to-state (invalid-transition-result-args)
  ()
  (:documentation "Error when trying to instantiate a transition-result with an invalid to-state"))
(define-condition invalid-transition-result-to-char (invalid-transition-result-args)
  ()
  (:documentation "Error when trying to instantiate a transition-result with an invalid to-char"))
(define-condition invalid-transition-result-action (invalid-transition-result-args)
  ()
  (:documentation "Error when trying to instantiate a transition-result with an invalid action"))
