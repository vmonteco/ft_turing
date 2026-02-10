(in-package :ft_turing)

;; Adjust accordingly to needs
(defparameter +show-code+ nil)
(defparameter +generated-code-file+ "lambda.lisp")
(defparameter +machine-output-file+ "output.log")

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
	  (destructuring-bind (jsonfile input) (parse-args (uiop:command-line-arguments))
		(let ((md (machine-description:make-machine-description-from-json
				   (uiop:read-file-string jsonfile))))
		  ;; (format *standard-output* "~A~%"
		  ;; 		  (machine-description::format-machine-description md))
		  (let ((machine-code (machine-maker:make-machine-code md)))
			(if +show-code+
				(format *standard-output* "The machine code:~%~S~%" machine-code))
			(if +generated-code-file+
				(with-open-file (s +generated-code-file+ :direction :output
														 :if-exists :supersede
														 :if-does-not-exist :create)
				  (format s "~S~%" machine-code)))
			(if +machine-output-file+
				(with-open-file (s +machine-output-file+ :direction :output
														 :if-exists :supersede
														 :if-does-not-exist :create)
				  (funcall (eval machine-code) input :streams (list s t)))
				(funcall (eval machine-code) input)))))
	;; Here start the handlers definitions.
	(help-condition () (print-usage) (uiop:quit 0))
	(usage-error () (print-usage-error) (uiop:quit 1))
	(file-error (c) (format *error-output* "File error: ~A~%" c) (uiop:quit 1))
	(stream-error (c) (format *error-output* "Stream error: ~A~%" C) (uiop:quit 1))
	;; Conditions that can be signaled by make-machine-description-from-json
	(machine-description:json-parsing-error (c)
	  (format *error-output* "JSON parsing error: ~A~%" c) (uiop:quit 1))
	(machine-description:invalid-json (c)
	  (format *error-output* "Invalid parsed JSON error: ~A~%" c) (uiop:quit 1))
	(machine-description:invalid-machine-description-args (c)
	  (format *error-output* "Invalid machine definition: ~A~%" c) (uiop:quit 1))
	;; Conditions that can be signaled during the running of the machine.	
	(machine-maker:machine-invalid-input (c)
	  (format *error-output* "~A~%" c) (uiop:quit 1))
	(machine-maker:machine-runtime-error (c)
	  (format *error-output* "Machine runtime error: ~A~%" c) (uiop:quit 1))
	;; CTRL-C:
	(sb-sys:interactive-interrupt (c)
	  (format *error-output* "Interractive interrupt: ~A~%" c) (uiop:quit 1))
	))
