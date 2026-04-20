(in-package :complexity-analyzer)

(deftype time-complexity ()
  "Valid time complexity classes."
  '(member :o1 :olog-n :on :on-log-n :on2 :o2n :on-fact))

(defconstant +o1+ :o1)
(defconstant +olog-n+ :olog-n)
(defconstant +on+ :on)
(defconstant +on-log-n+ :on-log-n)
(defconstant +on2+ :on2)
(defconstant +o2n+ :o2n)
(defconstant +on-fact+ :on-fact)

(defparameter *complexity-order*
  '(:o1 :olog-n :on :on-log-n :on2 :o2n :on-fact))

(defparameter *complexity-names*
  '((:o1 . "O(1)")
    (:olog-n . "O(log n)")
    (:on . "O(n)")
    (:on-log-n . "O(n log n)")
    (:on2 . "O(n²)")
    (:o2n . "O(2^n)")
    (:on-fact . "O(n!)")))


(defclass complexity-result ()
  ((complexity
    :initarg :complexity
    :accessor result-complexity
    :type time-complexity
    :documentation "The determined complexity class: :o1, :on, :on2, etc.")
   (confidence
    :initarg :confidence
    :accessor result-confidence
    :type keyword
    :documentation "Confidence level: :high, :medium, or :low.")
   (steps
    :initarg :steps
    :accessor result-total-steps
    :type integer
    :documentation "Total number of steps executed.")
   (input-size
    :initarg :input-size
    :accessor result-input-size
    :type integer
    :documentation "Input size (n).")
   (steps-per-n
    :initarg :steps-per-n
    :accessor result-steps-per-n
    :type number
    :documentation "Ratio steps/n.")
   (steps-per-n2
    :initarg :steps-per-n2
    :accessor result-steps-per-n2
    :type number
    :documentation "Ratio steps/n²."))
  (:documentation "Result of time complexity analysis.

FIELDS:
  complexity  - Complexity class (:o1, :olog-n, :on, :on-log-n, :on2, :o2n, :on-fact)
  confidence  - Confidence level (:high, :medium, :low)
  steps       - Total steps executed
  input-size  - Input size n
  steps-per-n - steps/n ratio
  steps-per-n2 - steps/n² ratio

EXAMPLE:
  (let ((r (analyze-time-complexity machine history 5 5)))
    (format t \"Complexity: ~A~%\" (format-complexity (result-complexity r)))
    (format t \"Steps: ~D~%\" (result-total-steps r))
    (format t \"Steps/n: ~,2F~%\" (result-steps-per-n r)))"))

(defclass graph-edge ()
  ((from-state :initarg :from-state :accessor edge-from-state)
   (to-state :initarg :to-state :accessor edge-to-state)
   (read-sym :initarg :read :accessor edge-read)
   (write-sym :initarg :write :accessor edge-write)
   (action :initarg :action :accessor edge-action)
   (erasure-p :initarg :erasure-p :accessor edge-erasure-p)
   (extension-p :initarg :extension-p :accessor edge-extension-p)))

(defclass cycle-info ()
  ((states :initarg :states :accessor cycle-states)
   (edges :initarg :edges :accessor cycle-edges)
   (net-movement :initarg :net-movement :accessor cycle-net-movement)
   (has-erasure :initarg :has-erasure :accessor cycle-has-erasure)
   (has-extension :initarg :has-extension :accessor cycle-has-extension)
   (back-and-forth-p :initarg :back-and-forth-p :accessor cycle-back-and-forth-p)))

(defclass structural-analysis ()
  ((edges :initarg :edges :accessor analysis-edges)
   (cycles :initarg :cycles :accessor analysis-cycles)
   (has-cycles-p :initarg :has-cycles-p :accessor analysis-has-cycles-p)
   (has-back-and-forth-cycles-p :initarg :has-back-and-forth-cycles-p 
    :accessor analysis-has-back-and-forth-cycles-p)
   (has-erasure-p :initarg :has-erasure-p :accessor analysis-has-erasure-p)
   (has-extension-p :initarg :has-extension-p :accessor analysis-has-extension-p)
   (max-cycle-depth :initarg :max-cycle-depth :accessor analysis-max-cycle-depth)
   (states-in-cycles :initarg :states-in-cycles :accessor analysis-states-in-cycles)))

(defclass runtime-metrics ()
  ((steps :initarg :steps :accessor metrics-steps)
   (input-size :initarg :input-size :accessor metrics-input-size)
   (max-head-amplitude :initarg :max-head-amplitude :accessor metrics-max-head-amplitude)
   (direction-changes :initarg :direction-changes :accessor metrics-direction-changes)
   (erasure-count :initarg :erasure-count :accessor metrics-erasure-count)
   (extension-count :initarg :extension-count :accessor metrics-extension-count)
   (tape-growth :initarg :tape-growth :accessor metrics-tape-growth)))

(defun factorial (n)
  "Compute n factorial."
  (if (<= n 1)
      1
      (loop for i from 2 to n
            with result = 1
            do (setf result (* result i))
            finally (return result))))

(defun complexity-function (complexity n)
  "Return the expected number of steps for complexity class and input size n."
  (case complexity
    (:o1 1)
    (:olog-n (max 1 (log (max 1 n) 2)))
    (:on n)
    (:on-log-n (* n (max 1 (log (max 1 n) 2))))
    (:on2 (* n n))
    (:o2n (expt 2 n))
    (:on-fact (factorial n))
    (otherwise n)))

(defun format-complexity (complexity)
  "Format a complexity keyword as a human-readable string."
  (or (cdr (assoc complexity *complexity-names*))
      "Unknown"))

(defun build-edges (machine)
  "Build the edge list from a machine's transitions."
  (let ((edges nil)
        (blank (blank machine)))
    (maphash
     (lambda (from-state trans-list)
       (dolist (tr trans-list)
         (let* ((read-sym (transition-read tr))
                (write-sym (transition-write tr))
                (erasure-p (and (not (char= read-sym blank))
                                (char= write-sym blank)))
                (extension-p (and (char= read-sym blank)
                                  (not (char= write-sym blank)))))
           (push (make-instance 'graph-edge
                                :from-state from-state
                                :to-state (transition-to-state tr)
                                :read read-sym
                                :write write-sym
                                :action (transition-action tr)
                                :erasure-p erasure-p
                                :extension-p extension-p)
                 edges))))
     (transitions machine))
    (nreverse edges)))

(defun build-adjacency-list (machine edges)
  "Build an adjacency list from edges."
  (let ((adj (make-hash-table :test #'equal)))
    (dolist (state (states machine))
      (setf (gethash state adj) nil))
    (dolist (edge edges)
      (push edge (gethash (edge-from-state edge) adj)))
    adj))

(defun find-cycles (machine edges)
  "Find all cycles in the state graph using DFS."
  (let ((adj (build-adjacency-list machine edges))
        (finals (finals machine))
        (cycles nil)
        (visited (make-hash-table :test #'equal))
        (rec-stack (make-hash-table :test #'equal))
        (current-path nil)
        (current-edges nil))
    (labels ((dfs (state path-index)
               (when (member state finals :test #'string=)
                 (return-from dfs))
               (setf (gethash state visited) t)
               (setf (gethash state rec-stack) path-index)
               (push state current-path)
               (dolist (edge (gethash state adj))
                 (let ((next-state (edge-to-state edge)))
                   (if (gethash next-state rec-stack)
                       (let* ((cycle-start-idx (gethash next-state rec-stack))
                              (cycle-states (append
                                             (nthcdr (- (length current-path) 1 cycle-start-idx)
                                                     (reverse current-path))
                                             (list next-state)))
                              (cycle-edges (append
                                            (nthcdr (- (length current-edges) cycle-start-idx)
                                                    (reverse current-edges))
                                            (list edge))))
                         (push (make-cycle-info cycle-states cycle-edges) cycles))
                       (unless (gethash next-state visited)
                         (push edge current-edges)
                         (dfs next-state (1+ path-index))
                         (pop current-edges)))))
               (pop current-path)
               (remhash state rec-stack)))
      (dolist (state (states machine))
        (unless (or (gethash state visited)
                    (member state finals :test #'string=))
          (dfs state 0))))
    cycles))

(defun make-cycle-info (states edges)
  "Create a cycle-info object from states and edges."
  (let ((net-movement 0)
        (has-erasure nil)
        (has-extension nil)
        (left-count 0)
        (right-count 0))
    (dolist (edge edges)
      (if (eq (edge-action edge) :left)
          (progn (decf net-movement) (incf left-count))
          (progn (incf net-movement) (incf right-count)))
      (when (edge-erasure-p edge) (setf has-erasure t))
      (when (edge-extension-p edge) (setf has-extension t)))
    (let* ((edge-count (length edges))
           (is-back-and-forth (and (> left-count 0)
                                   (> right-count 0)
                                   (<= (abs net-movement)
                                       (max 1 (floor edge-count 4))))))
      (make-instance 'cycle-info
                     :states states
                     :edges edges
                     :net-movement net-movement
                     :has-erasure has-erasure
                     :has-extension has-extension
                     :back-and-forth-p is-back-and-forth))))

(defun calculate-cycle-depth (cycles)
  "Calculate the maximum cycle nesting depth."
  (when (null cycles) (return-from calculate-cycle-depth 0))
  (when (= (length cycles) 1) (return-from calculate-cycle-depth 1))
  (let ((cycle-state-sets
          (mapcar (lambda (c)
                    (let ((set (make-hash-table :test #'equal)))
                      (dolist (state (cycle-states c))
                        (setf (gethash state set) t))
                      set))
                  cycles))
        (max-overlap 0))
    (loop for i from 0 below (length cycle-state-sets)
          do (let ((overlap-count 0))
               (loop for j from 0 below (length cycle-state-sets)
                     when (/= i j)
                       do (let ((set-i (nth i cycle-state-sets))
                                (set-j (nth j cycle-state-sets)))
                            (maphash (lambda (state val)
                                       (declare (ignore val))
                                       (when (gethash state set-j)
                                         (incf overlap-count)
                                         (return)))
                                     set-i)))
               (setf max-overlap (max max-overlap overlap-count))))
    (+ 1 (min max-overlap (1- (length cycles))))))

(defun analyze-structure (machine)
  "Perform structural analysis of a Turing machine."
  (let* ((edges (build-edges machine))
         (cycles (find-cycles machine edges))
         (states-in-cycles-set (make-hash-table :test #'equal)))
    (dolist (cycle cycles)
      (dolist (state (cycle-states cycle))
        (setf (gethash state states-in-cycles-set) t)))
    (make-instance 'structural-analysis
                   :edges edges
                   :cycles cycles
                   :has-cycles-p (> (length cycles) 0)
                   :has-back-and-forth-cycles-p (some #'cycle-back-and-forth-p cycles)
                   :has-erasure-p (some #'edge-erasure-p edges)
                   :has-extension-p (some #'edge-extension-p edges)
                   :max-cycle-depth (calculate-cycle-depth cycles)
                   :states-in-cycles (hash-table-count states-in-cycles-set))))

(defun collect-runtime-metrics (history input-size blank initial-tape-length)
  "Collect runtime metrics from execution history."
  (when (null history)
    (return-from collect-runtime-metrics
      (make-instance 'runtime-metrics
                     :steps 0
                     :input-size input-size
                     :max-head-amplitude 0
                     :direction-changes 0
                     :erasure-count 0
                     :extension-count 0
                     :tape-growth 0)))
  (let ((min-head 0)
        (max-head 0)
        (direction-changes 0)
        (erasure-count 0)
        (extension-count 0)
        (prev-action nil)
        (final-tape-length initial-tape-length))
    (dolist (step history)
      (let ((pos (machine-maker:step-head-position step)))
        (setf min-head (min min-head pos))
        (setf max-head (max max-head pos)))
      (let ((action (machine-maker:step-action step)))
        (when (and prev-action (not (eq action prev-action)))
          (incf direction-changes))
        (setf prev-action action))
      (let ((read-char (machine-maker:step-read-char step))
            (write-char (machine-maker:step-write-char step)))
        (when (and (not (char= read-char blank))
                   (char= write-char blank))
          (incf erasure-count))
        (when (and (char= read-char blank)
                   (not (char= write-char blank)))
          (incf extension-count)
          (incf final-tape-length))))
    (make-instance 'runtime-metrics
                   :steps (length history)
                   :input-size input-size
                   :max-head-amplitude (- max-head min-head)
                   :direction-changes direction-changes
                   :erasure-count erasure-count
                   :extension-count extension-count
                   :tape-growth (- final-tape-length initial-tape-length))))

(defun analyze-complexity-internal (structural runtime)
  "Internal complexity analysis using structural and runtime metrics.
   Returns (values complexity confidence)."
  (let* ((n (metrics-input-size runtime))
         (steps (metrics-steps runtime)))
    ;; When input too small
    (when (< n 2)
      (when (<= steps 3)
        (return-from analyze-complexity-internal (values :o1 :medium)))
      (return-from analyze-complexity-internal (values :on :low)))
    (let* ((linear-ratio (/ steps n))
           (log-n (max 1 (log (max 2 n) 2)))
           (n2-ratio (/ steps (* n n)))
           (n-log-n (* n log-n))
           (n-log-n-ratio (/ steps n-log-n))
           (direction-change-ratio (/ (metrics-direction-changes runtime) (max 1 steps)))
           (direction-changes-per-n (/ (metrics-direction-changes runtime) n))
           (has-frequent-direction-changes (> direction-change-ratio 0.1))
           (has-quadratic-direction-changes (or (>= direction-changes-per-n 0.5)
                                                 (>= (metrics-direction-changes runtime) (/ n 2))))
           (erasure-ratio (/ (metrics-erasure-count runtime) n))
           (has-significant-erasure (> erasure-ratio 0.3)))
      ;; O(1)
      (when (and (<= steps 5) (< linear-ratio 1.0))
        (return-from analyze-complexity-internal (values :o1 :high)))
      (when (and (< steps n) (>= n 5))
        (return-from analyze-complexity-internal (values :o1 :high)))
      ;; O(n)
      (when (and (>= linear-ratio 0.5) (<= linear-ratio 3.0) (<= n 10))
        (return-from analyze-complexity-internal (values :on :high)))
      ;; O(n²)
      (let ((is-n2-ratio-valid (and (>= n2-ratio 0.5) (<= n2-ratio 10)))
            (has-n2-behavior has-significant-erasure))
        (when (and is-n2-ratio-valid has-n2-behavior)
          (return-from analyze-complexity-internal (values :on2 :high))))
      ;; O(n)
      (let* ((is-linear-ratio-valid (and (>= linear-ratio 0.5) (<= linear-ratio 20)))
             (is-multi-pass-linear (and is-linear-ratio-valid
                                        (not has-significant-erasure)
                                        (< erasure-ratio 0.1))))
        (when is-multi-pass-linear
          (return-from analyze-complexity-internal (values :on :high))))
      ;; O(n)
      (let* ((is-linear-ratio-valid (and (>= linear-ratio 0.5) (<= linear-ratio 20)))
             (not-quadratic (< n2-ratio 0.8)))
        (when (and is-linear-ratio-valid not-quadratic)
          (return-from analyze-complexity-internal (values :on :high))))
      ;; O(n log n)
      (when (and (> linear-ratio 3) (< n2-ratio 0.5)
                 (> n-log-n-ratio 0.3) (< n-log-n-ratio 30))
        (return-from analyze-complexity-internal (values :on-log-n :medium)))
      ;; O(n!)
      (when (and (<= n 7) (>= n 3))
        (let* ((fact (factorial n))
               (fact-ratio (/ steps fact))
               (exp2n (expt 2 n))
               (exp-ratio (/ steps exp2n))
               (is-factorial-candidate (and (>= fact-ratio 1) (<= fact-ratio 100)))
               (exp-ratio-too-large (and (>= n 4) (> exp-ratio 20)))
               (n2-ratio-very-large (and (= n 3) (> n2-ratio 15))))
          (when (and is-factorial-candidate
                     (or exp-ratio-too-large n2-ratio-very-large))
            (return-from analyze-complexity-internal 
              (values :on-fact (if (>= n 4) :high :medium))))))
      ;; O(2^n)
      (let* ((exp2n (expt 2 n))
             (exp-ratio (/ steps exp2n))
             (is-exp-candidate (and (< n 12) (>= exp-ratio 0.5) (<= exp-ratio 20)))
             (has-no-erasure (and (not has-significant-erasure) (< erasure-ratio 0.1)))
             (is-binary-counting-pattern (and has-quadratic-direction-changes has-no-erasure)))
        (when (and is-exp-candidate is-binary-counting-pattern)
          (return-from analyze-complexity-internal 
            (values :o2n (if (>= n 5) :high :medium)))))
      ;; O(n²) (secondary pattern)
      (when has-quadratic-direction-changes
        (when (and (> n2-ratio 0.05) (< n2-ratio 200))
          (return-from analyze-complexity-internal (values :on2 :medium))))
      ;; O(2^n) (tape growth pattern)
      (let* ((exp2n (expt 2 n))
             (exp-ratio (/ steps exp2n)))
        (when (or (> (metrics-tape-growth runtime) (* n 2))
                  (and (< n 15) (> steps (* exp2n 0.1))))
          (when (and (< n 20) (> exp-ratio 0.01) (< exp-ratio 100))
            (return-from analyze-complexity-internal (values :o2n :medium)))))
      ;; If not good, just find with the best fit by ratio
      (let ((best-fit :on)
            (best-score most-positive-fixnum))
        (dolist (complexity *complexity-order*)
          (let* ((expected (complexity-function complexity n))
                 (ratio (/ steps (max 1 expected))))
            (when (and (> expected 0)
                       (> ratio 0.01)
                       (< ratio 10000))
              (let* ((ratio-score (abs (log (/ ratio 5) 10)))
                     (complexity-penalty (* (position complexity *complexity-order*) 0.3))
                     (score (+ ratio-score complexity-penalty)))
                (when (< score best-score)
                  (setf best-score score)
                  (setf best-fit complexity))))))
        (values best-fit :medium)))))

(defun analyze-time-complexity (machine history input-size initial-tape-length)
  "Analyze the time complexity of a Turing machine execution.

PARAMETERS:
  machine (machine-description)       - Parsed machine structure from machine-description module
  history (list of execution-step)    - Execution trace from machine-maker:run-turing-machine
  input-size (integer)                - Size of the input (n)
  initial-tape-length (integer)       - Initial tape length before execution

RETURNS:
  complexity-result object with fields:
    - complexity: :o1 | :olog-n | :on | :on-log-n | :on2 | :o2n | :on-fact
    - confidence: :high | :medium | :low
    - steps: total steps executed
    - input-size: n
    - steps-per-n: steps/n ratio
    - steps-per-n2: steps/n² ratio

EXAMPLE:
  (let* ((machine (parse-machine-description json-hash))
         (result (machine-maker:run-turing-machine machine \"101\"))
         (history (machine-maker:execution-result-history result))
         (complexity (analyze-time-complexity machine history 3 3)))
    (format t \"Complexity: ~A~%\" (format-complexity (result-complexity complexity)))
    (format t \"Confidence: ~A~%\" (result-confidence complexity))
    (format t \"Steps: ~D~%\" (result-total-steps complexity))
    (format t \"Steps/n: ~,2F~%\" (result-steps-per-n complexity)))"
  (let* ((structural (analyze-structure machine))
         (runtime (collect-runtime-metrics history input-size
                                           (blank machine)
                                           initial-tape-length))
         (steps (metrics-steps runtime))
         (n (max 1 input-size)))
    (multiple-value-bind (complexity confidence)
        (analyze-complexity-internal structural runtime)
      (make-instance 'complexity-result
                     :complexity complexity
                     :confidence confidence
                     :steps steps
                     :input-size input-size
                     :steps-per-n (/ steps (float n))
                     :steps-per-n2 (/ steps (float (* n n)))))))
