(in-package :cli/tests-pkg)

(def-suite cli-tests)
(in-suite cli-tests)

(test parse-cli-args-help
  (multiple-value-bind (params err) (parse-cli-args '("--help"))
    (is (null params))
    (is (search "usage:" err))))

(test parse-cli-args-invalid
  (multiple-value-bind (params err) (parse-cli-args '("foo"))
    (is (null params))
    (is (search "Error:" err))))

(test parse-cli-args-valid
  (multiple-value-bind (params err) (parse-cli-args '("file.json" "input"))
    (is (null err))
    (is (equal (getf params :jsonfile) "file.json"))
    (is (equal (getf params :input) "input"))))

(test read-json-file-notfound
  (multiple-value-bind (stream err) (read-json-file "nope.json")
    (is (null stream))
    (is (search "not found" err))))

(test read-json-file-open-close
  (let ((tmpfile "tmp-test.json"))
    (with-open-file (out tmpfile :direction :output :if-exists :supersede)
      (write-line "{}" out))
    (multiple-value-bind (stream err) (read-json-file tmpfile)
      (is (null err))
      (is (streamp stream))
      (when stream (close stream)))
    (delete-file tmpfile)))
