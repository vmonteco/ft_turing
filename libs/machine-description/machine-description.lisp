(in-package :machine-description)

;; (define-condition invalid-description (error))
;; (define-condition invalid-machine-name (invalid-description))
;; (define-condition invalid-machine-alphabet (invalid-description))
;; (define-condition invalid-machine-blank (invalid-description))
;; (define-condition invalid-machine-states (invalid-description))
;; (define-condition invalid-machine-initial (invalid-description))
;; (define-condition invalid-machine-finals (invalid-description))
;; (define-condition invalid-machine-transitions (invalid-description))

(defclass machine-description ()
	((name
	  :initarg :name
	  :type :string
	  :accessor name
	  :documentation "The name of a machine is a non-empty string.")
	 (alphabet
	  :initarg :name
	  :type list
	  :accessor alphabet
	  :documentation
	  "The alphabet of a machine is a non-empty set of characters.")
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
	  :accessor machine-transitions
	  :documentation ""))
  (:documentation "A Turing machine description"))
