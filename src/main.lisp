(in-package :ft_turing-pkg)

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
  (values (first args) (second args)))


;;; Main function:
(defun main ()
  (handler-case
	  (multiple-value-bind
			(jsonfile input) (parse-args (uiop:command-line-arguments))
		(format *standard-output*
				"JSON file content: ~A~%"
				(uiop:read-file-string jsonfile)))
	(help-condition () (print-usage) (uiop:quit 0))
	(usage-error () (print-usage-error) (uiop:quit 1))
	(file-error (c) (format *error-output* "File error: ~A~%" c))
	(stream-error (c) (format *error-output* "Stream error: ~A~%" C))
	(com.inuoe.jzon:json-parse-error (c)
	  (format *error-output* "JSON parse error: ~A~%" c))))
