(in-package :ft_turing)

;;; Errors and conditions:
(define-condition help-condition (condition) ())
(define-condition usage-error (error) ())

;;; Usage texts:
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
  (format t usage-msg))


(defun print-usage-error ()
  "Print usage error message to stderr"
  (format *error-output*  usage-error-msg))


;;; Arguments parsing:
(defun parse-args (args)
  "Parse arguments and return the two expected parameters."
  ;; Checking if either -h or --help was provided.
  (unless (and (not (member "-h" args :test #'equal))
			   (not (member "--help" args :test #'equal)))
	(signal 'help-condition))
  ;; Checking number of arguments.
  (unless (eq (length args) 2) (error 'usage-error))
  args)

;;; Main function:
(defun main ()
  ;; Here we set handlers for various conditions to handle.
  (handler-case
	  ;; Since parse-args returns 2 values, we use it in a multiple-value-bind.
	  (destructuring-bind (jsonfile input) (parse-args (uiop:command-line-arguments))

		;; Now in the current scope, jsonfile and input are bound to returned
		;; values.

		;; The following block in this scope is just code placeholder.
		(let ((md (machine-description:make-machine-description-from-json
				   (uiop:read-file-string jsonfile))))
		  (format *standard-output* "~A~%"
				  (machine-description::format-machine-description md))))

	;; Here start the handlers definitions.
	(help-condition () (print-usage) (uiop:quit 0))
	(usage-error () (print-usage-error) (uiop:quit 1))
	(file-error (c) (format *error-output* "File error: ~A~%" c) (uiop:quit 1))
	(stream-error (c) (format *error-output* "Stream error: ~A~%" C) (uiop:quit 1))
	;; This is a quick workaround for a JZON bug whose fix hasn't been
	;; released on quicklisp yet.
	(type-error (c)
	  (format *error-output* "Workaround for JZON bug (probably empty JSON)~%") (uiop:quit 1))
	(com.inuoe.jzon:json-parse-error (c)
	  (format *error-output* "JSON parsing error: ~A~%" c) (uiop:quit 1))
	(machine-description:invalid-json (c)
	  (format *error-output* "Unsuitable JSON error: ~A~%" c) (uiop:quit 1))
	(machine-description:invalid-machine-description (c)
	  (format *error-output* "Invalid machine description error: ~A~%" c) (uiop:quit 1))))
