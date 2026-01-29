(in-package :machine-description)

;;; This file contains conditions used in the whole machine-description lib.

;; JSON input conditions:
(define-condition invalid-json (error) ())
(define-condition invalid-json-alphabet (invalid-json) ())
(define-condition invalid-json-blank (invalid-json) ())
(define-condition invalid-json-states (invalid-json) ())
(define-condition invalid-json-initial (invalid-json) ())
(define-condition invalid-json-finals (invalid-json) ())
(define-condition invalid-json-transitions (invalid-json) ())
(define-condition invalid-json-missing-from-state (invalid-json-transitions) ())
(define-condition invalid-json-missing-from-char (invalid-json-transitions) ())
(define-condition invalid-json-missing-to-state (invalid-json-transitions) ())
(define-condition invalid-json-missing-to-char (invalid-json-transitions) ())
(define-condition invalid-json-missing-action (invalid-json-transitions) ())
(define-condition invalid-json-from-char (invalid-json-transitions) ())
(define-condition invalid-json-from-state (invalid-json-transitions) ())
(define-condition invalid-json-to-char (invalid-json-transitions) ())
(define-condition invalid-json-to-state (invalid-json-transitions) ())
(define-condition invalid-json-action (invalid-json-transitions) ())

;; Transition result:
(define-condition invalid-action-error (error) ())
(define-condition invalid-to-char (error) ())

;; machine-description instantiation:
(define-condition invalid-machine-description (error) ())
(define-condition invalid-name (invalid-machine-description) ())
(define-condition invalid-alphabet (invalid-machine-description) ())
(define-condition invalid-blank (invalid-machine-description) ())
(define-condition invalid-states (invalid-machine-description) ())
(define-condition invalid-initial-state (invalid-machine-description) ())
(define-condition invalid-finals (invalid-machine-description) ())
(define-condition invalid-transitions (invalid-machine-description) ())
