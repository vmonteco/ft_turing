(in-package cli-pkg)

(defun read-json-file (filepath)
  "Open a file and return (values stream error)."
  (if (not (probe-file filepath))
      (values nil (format nil "Error: File ~A not found" filepath))
      (handler-case
          (values (open filepath :direction :input :if-does-not-exist nil) nil)
        (error (e)
          (values nil (format nil "Error opening file: ~A" e))))))
