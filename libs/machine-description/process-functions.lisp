(in-package :machine-description)

(defun process-alphabet (raw-alphabet)
  "Turn RAW-ALPHABET into a list of chars"
  (if (and (vectorp raw-alphabet)
		   (every #'stringp raw-alphabet)
		   (every (lambda (s) (eq (length s) 1)) raw-alphabet)
		   (utils:utils-sets-setp
			(map 'list #'identity raw-alphabet) :test #'equal))
	  (map 'list (lambda (s) (aref s 0)) raw-alphabet)
	  (signal 'invalid-json-alphabet)))

(defun process-blank (raw-blank)
  "Turn RAW-BLANK in a char"
  (if (and (stringp raw-blank)
		   (eq (length raw-blank) 1))
	  (aref raw-blank 0)
	  (signal 'invalid-json-blank)))

(defun process-states (raw-states)
  "Turn RAW-STATES into a set of symbols"
  (unless (and (vectorp raw-states)
			   (every (lambda (e) (and (stringp e))) raw-states))
	(signal 'invalid-json-states))
  (let ((raw-states-as-list (map 'list #'identity raw-states)))
	(unless (utils:utils-sets-setp raw-states-as-list :test #'equal)
	  (signal 'invalid-json-states))
	(map 'list
		 (lambda (s) (intern s (find-package 'keyword)))
		 raw-states-as-list)))

(defun process-initial-state (raw-initial)
  "Turn RAW-INITIAL into a symbol"
  (if (and (stringp raw-initial)
		   (> (length raw-initial) 0))
	  (intern raw-initial (find-package 'keyword))
	  (signal 'invalid-json-initial-state)))

(defun process-finals (raw-finals)
  "Turn RAW-FINALS into a set of symbols"
  (unless (and (vectorp raw-finals)
			   (every (lambda (e) (and (stringp e))) raw-finals))
	(signal 'invalid-json-finals))
  (let ((raw-finals-as-list (map 'list #'identity raw-finals)))
	(unless (utils:utils-sets-setp raw-finals-as-list :test #'equal)
	  (signal 'invalid-json-finals))
	(map 'list
		 (lambda (s) (intern s (find-package 'keyword)))
		 raw-finals-as-list)))

(defun process-from-char (raw-from-char)
  (unless (and (stringp raw-from-char)
			   (= (length raw-from-char) 1))
	(signal 'invalid-json-from-char))
  (aref raw-from-char 0))

(defun process-to-state (raw-to-state)
  (unless (and (stringp raw-to-state)
			   (> (length raw-to-state) 0))
	(signal 'invalid-json-to-state))
  (intern raw-to-state (find-package 'keyword)))

(defun process-to-char (raw-to-char)
  (unless (and (stringp raw-to-char)
			   (= (length raw-to-char) 1))
	(signal 'invalid-json-to-char))
  (aref raw-to-char 0))

(defun process-action (action)
  (if (equal action "RIGHT")
	  :right
	  (if (equal action "LEFT")
		  :left
		  (signal 'invalid-json-action))))

(defun process-string (string)
  "Turn STRING into a case-preserved keyword symbol"
  (unless (and (stringp string)
			   (> (length string) 0))
	(signal 'invalid-json-string))
  (intern string (find-package 'keyword)))

(defun process-transitions (raw-transitions)
  "Turn RAW-TRANSITIONS into an alist of alists of transitions"
  (unless (hash-table-p raw-transitions)
	(signal 'invalid-json-transitions))
  (flet ((extract-state-transitions (state-transitions-hts)
		   ;; Check type of state-transitions-hts. Vector is expected.
		   (unless (and (vectorp state-transitions-hts)
						(every #'hash-table-p state-transitions-hts))
			 (signal 'invalid-json-transitions))
		   ;; Map should work on vector.
		   (map 'list (lambda (t-raw)
						(let ((read (process-from-char
									 (utils:gethash-or-signal
									  t-raw
									  "read"
									  'invalid-json-missing-from-char)))
							  (to-state
								(process-to-state (utils:gethash-or-signal
												   t-raw
												   "to_state"
												   'invalid-json-missing-to-state)))
							  (write (process-to-char
									  (utils:gethash-or-signal
									   t-raw
									   "write"
									   'invalid-json-missing-to-char)))
							  (action (process-action
									   (utils:gethash-or-signal
										t-raw
										"action"
										'invalid-json-missing-action))))
						  (cons read (make-instance 'transition-result
													:to-state to-state
													:to-char write
													:action action)) ))
				state-transitions-hts)))
	(loop :for k :being :the :hash-key
			:using (hash-value v) :of raw-transitions
		  :collect (cons (intern k (find-package 'keyword))
						 (extract-state-transitions v)))))
