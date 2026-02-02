(in-package :complexity-analyzer/tests)

(def-suite complexity-analyzer-tests)
(in-suite complexity-analyzer-tests)

(test format-complexity-o1
  "Test formatting O(1) complexity."
  (is (string= (format-complexity :o1) "O(1)")
      "O(1) should format correctly"))

(test format-complexity-on
  "Test formatting O(n) complexity."
  (is (string= (format-complexity :on) "O(n)")
      "O(n) should format correctly"))

(test format-complexity-on2
  "Test formatting O(n²) complexity."
  (is (string= (format-complexity :on2) "O(n²)")
      "O(n²) should format correctly"))

(test format-complexity-olog-n
  "Test formatting O(log n) complexity."
  (is (string= (format-complexity :olog-n) "O(log n)")
      "O(log n) should format correctly"))

(test format-complexity-on-log-n
  "Test formatting O(n log n) complexity."
  (is (string= (format-complexity :on-log-n) "O(n log n)")
      "O(n log n) should format correctly"))

(defun make-linear-machine-hash ()
  "Create a simple linear O(n) scanner machine."
  (let ((h (make-hash-table :test #'equal))
        (trans (make-hash-table :test #'equal)))
    (setf (gethash "name" h) "linear")
    (setf (gethash "alphabet" h) #("0" "."))
    (setf (gethash "blank" h) ".")
    (setf (gethash "states" h) #("scan" "done"))
    (setf (gethash "initial" h) "scan")
    (setf (gethash "finals" h) #("done"))
    (setf (gethash "scan" trans)
          (vector (let ((t1 (make-hash-table :test #'equal)))
                    (setf (gethash "read" t1) "0")
                    (setf (gethash "to_state" t1) "scan")
                    (setf (gethash "write" t1) "0")
                    (setf (gethash "action" t1) "RIGHT")
                    t1)
                  (let ((t2 (make-hash-table :test #'equal)))
                    (setf (gethash "read" t2) ".")
                    (setf (gethash "to_state" t2) "done")
                    (setf (gethash "write" t2) ".")
                    (setf (gethash "action" t2) "RIGHT")
                    t2)))
    (setf (gethash "transitions" h) trans)
    h))

(defun make-mock-step (state read-char write-char action head-pos)
  "Create a mock execution step for testing."
  (make-instance 'machine-maker:execution-step
                 :current-state state
                 :read-char read-char
                 :write-char write-char
                 :action action
                 :head-position head-pos
                 :to-state state
                 :tape-state ""))

(test result-creation
  "Test creating complexity-result objects."
  (let ((result (make-instance 'complexity-result
                               :complexity :on
                               :confidence :high
                               :steps 100
                               :input-size 10
                               :steps-per-n 10.0
                               :steps-per-n2 1.0)))
    (is (eq (result-complexity result) :on)
        "Complexity should be stored correctly")
    (is (eq (result-confidence result) :high)
        "Confidence should be stored correctly")
    (is (= (result-total-steps result) 100)
        "Steps should be stored correctly")
    (is (= (result-input-size result) 10)
        "Input size should be stored correctly")
    (is (= (result-steps-per-n result) 10.0)
        "Steps-per-n should be stored correctly")
    (is (= (result-steps-per-n2 result) 1.0)
        "Steps-per-n2 should be stored correctly")))

(test analyze-simple-linear-machine
  "Test analyzing a simple linear machine that scans input once."
  (let* ((machine (machine-description:parse-machine-description (make-linear-machine-hash)))
         (history (loop for i from 0 below 10
                        collect (make-mock-step "scan" #\0 #\0 :right i)))
         (result (analyze-time-complexity machine history 10 10)))
    (is (eq (result-complexity result) :on)
        "Should detect O(n) complexity")
    (is (member (result-confidence result) '(:high :medium))
        "Should have reasonable confidence")
    (is (= (result-total-steps result) 10)
        "Should count steps correctly")
    (is (= (result-input-size result) 10)
        "Should store input size correctly")
    (is (< 0.9 (result-steps-per-n result) 1.1)
        "Steps-per-n should be around 1.0 for linear scan")))

(test analyze-constant-time
  "Test analyzing a machine with constant time O(1)."
  (let* ((machine (machine-description:parse-machine-description (make-linear-machine-hash)))
         (history (loop for i from 0 below 3
                        collect (make-mock-step "scan" #\0 #\0 :right 0)))
         (result (analyze-time-complexity machine history 100 100)))
    (is (eq (result-complexity result) :o1)
        "Should detect O(1) complexity when steps << n")
    (is (< (result-steps-per-n result) 0.1)
        "Steps-per-n should be very small for O(1)")))

(test analyze-quadratic-pattern
  "Test analyzing a machine with quadratic O(n²) pattern."
  (let* ((machine (machine-description:parse-machine-description (make-linear-machine-hash)))
         (n 5)
         (steps (* n n))
         (history (loop for i from 0 below steps
                        for pos = (mod i n)
                        for action = (if (evenp (floor i n)) :right :left)
                        collect (make-mock-step "scan" 
                                                 (if (< i 10) #\0 #\.)
                                                 (if (< i 10) #\. #\0)
                                                 action pos)))
         (result (analyze-time-complexity machine history n n)))
    (is (eq (result-complexity result) :on2)
        "Should detect O(n²) complexity")
    (is (< 0.8 (result-steps-per-n2 result) 1.2)
        "Steps-per-n² should be around 1.0 for quadratic")))
