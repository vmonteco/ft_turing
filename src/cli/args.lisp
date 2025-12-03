(in-package :cli-pkg)

(defun parse-cli-args (args)
  "Parse CLI arguments and return (values params error)."
  (cond
    ((or (null args) (member "--help" args :test #'string=) (member "-h" args :test #'string=))
     (values nil "usage: ft_turing [-h] jsonfile input\n\npositional arguments:\n    jsonfile json description of the machine\n    input input of the machine\n\noptional arguments:\n    -h, --help show this help message and exit"))
    ((not (= (length args) 2))
     (values nil "Error: Expected 2 arguments: jsonfile input"))
    (t
     (values (list :jsonfile (first args) :input (second args)) nil))))
