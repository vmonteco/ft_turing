(in-package :machine-description)

;;; A class to describe a turing machine.

;; (define-condition invalid-description (error))
;; (define-condition invalid-machine-name (invalid-description))
;; (define-condition invalid-machine-alphabet (invalid-description))
;; (define-condition invalid-machine-blank (invalid-description))
;; (define-condition invalid-machine-states (invalid-description))
;; (define-condition invalid-machine-initial (invalid-description))
;; (define-condition invalid-machine-finals (invalid-description))
;; (define-condition invalid-machine-transitions (invalid-description))
;; 

(define-condition invalid-machine-description (error) ())
(define-condition invalid-name (invalid-machine-description) ())
(define-condition invalid-alphabet (invalid-machine-description) ())
(define-condition invalid-blank (invalid-machine-description) ())
(define-condition invalid-states (invalid-machine-description) ())
(define-condition invalid-initial-state (invalid-machine-description) ())
(define-condition invalid-finals (invalid-machine-description) ())
(define-condition invalid-transitions (invalid-machine-description) ())

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
										  states
										  initial
										  finals
										  transitions)
  ;; Checks here.
  )
