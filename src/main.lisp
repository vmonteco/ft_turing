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
		(let* ((json-file-content (uiop:read-file-string jsonfile))
			   (json-hashtable (com.inuoe.jzon:parse json-file-content)))
		  (format uiop:*stdout*
				  (concatenate
				   'string
				   "JSON file path: ~a~%"
				   "Input: ~A~%"
				   "File content: ~A~%"
				   "parsed JSON: ~A~%"
				   "Machine name: ~S~%")
				  jsonfile
				  input
				  json-file-content
				  json-hashtable
				  (gethash "name" json-hashtable))))
	(help-condition () (print-usage) (uiop:quit 0))
	(usage-error () (print-usage-error) (uiop:quit 1))
	(file-error (c) (format uiop:*stderr* "File error: ~A~%" c))
	(stream-error (c) (format uiop:*stderr* "Stream error: ~A~%" c))
	(com.inuoe.jzon:json-parse-error (c) (format uiop:*stderr* "JSON parse error: ~A~%" c))
	(error (c) (format uiop:*stderr* "Generic handler, condition ~A of type ~A~%" c (type-of c)))))
