(in-package :machine-description)

;;; A class to describe a turing machine.


;; A machine description is made of the following fields:
;; - A name.
;;   The name is a non-empty string.
;;   The only error case is when an empty string is provided.
;; - An alphabet.
;;   The alphabet is a non-empty set of characters.
;;   There are several error cases:
;;   - If it's empty.
;;   - If it's not a list.
;;   - If it contains something else than characters.
;;   - If it contains duplicate elements.
;; - States.
;;   The states is a non-empty set of symbols.
;;   The error cases are:
;;   - If it's empty.
;;   - If it's not a list.
;;   - If it contains something else than symbols.
;;   - If it contains duplicates.
;; - Initial.
;;   A symbol contained in states.
;;   The error case is if it's not contained in states.
;; - finals.
;;   A non-empty subset of states.
;;   The error cases are:
;;   - If it's empty.
;;   - If it's not a list.
;;   - If it contains duplicates.
;;   - If it contains elements that aren't contained in states.
;; - transitions.
;;   A hashtable of hashtables of the form:
;;   Dict[State (non-final), Dict[character, Tuple(Action (right/left), character, state)]]

;; (define-condition invalid-description (error))
;; (define-condition invalid-machine-name (invalid-description))
;; (define-condition invalid-machine-alphabet (invalid-description))
;; (define-condition invalid-machine-blank (invalid-description))
;; (define-condition invalid-machine-states (invalid-description))
;; (define-condition invalid-machine-initial (invalid-description))
;; (define-condition invalid-machine-finals (invalid-description))
;; (define-condition invalid-machine-transitions (invalid-description))
;; 

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
	  :accessor transitions
	  :documentation ""))
  (:documentation "A Turing machine description"))
