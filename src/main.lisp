(require :uiop)

(in-package :ft_turing-pkg)

;;; Errors (conditions):
(define-condition help-condition (condition) ())
(define-condition usage-error (error) ())

;;; Usage:
(defparameter usage-msg
  "usage: ft_turing [-h] jsonfile input

positional arguments:
  jsonfile            json description of the machine

  input               input of the machine

optional arguments:
  -h, --help          show this help message and exit~%")
(defparameter usage-error-msg
  (concatenate 'string "Usage error:~%" usage-msg))

(defun print-usage ()
  "Print usage to stdout"
  (format uiop:*stdout* usage-msg))

(defun print-usage-error ()
  "Print usage error message to stderr"
  (format uiop:*stderr* usage-error-msg))

;;; Arguments parsing:
(defun parse-args (args)
  "Parse arguments and return the two expected parameters."
  ;; Checking if either -h or --help was provided.
  (unless (and (not (member "-h" args :test #'equal))
			   (not (member "--help" args :test #'equal)))
	(signal 'help-condition))
  ;; Checking number of arguments.
  (unless (eq (length args) 2) (error 'usage-error))
  (values (first args) (second args)))

(defun main ()
  (handler-case
	  (multiple-value-bind
			(jsonfile input) (parse-args uiop:*command-line-arguments*)
		(format uiop:*stdout*
				"JSON file path: ~a~%Input: ~a~%"
				jsonfile
				input))
	(help-condition () (print-usage) (uiop:quit 0))
	(usage-error () (print-usage-error) (uiop:quit 1))))
