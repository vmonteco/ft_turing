(in-package :machine-description)

(defun process-name (raw-name)
  "Checks RAW-NAME"
  (unless (stringp raw-name)
	(signal 'invalid-json-name
			:msg "name is not of type string"))
  (unless (> (length raw-name) 0)
	(signal 'invalid-json-name
			:msg "Empty name"))
  raw-name)

(defun process-alphabet (raw-alphabet)
  "Turn RAW-ALPHABET into a list of chars"
  (unless (vectorp raw-alphabet)
	(signal 'invalid-json-alphabet
			:msg "Alphabet of wrong type (expecting array)"))
  (unless (> (length raw-alphabet) 0)
	(signal 'invalid-json-alphabet
			:msg "Alphabet should not be empty"))
  (unless (every #'stringp raw-alphabet)
	(signal 'invalid-json-alphabet
			:msg "Alphabet should only contain strings"))
  (unless (every (lambda (s) (= (length s) 1)) raw-alphabet)
	(signal 'invalid-json-alphabet
			:msg "Strings in alphabet should all be of length 1"))
  (unless (utils:utils-sets-setp (map 'list #'identity raw-alphabet)
								 :test #'equal)
	(signal 'invalid-json-alphabet
			:msg "Alphabet should not have duplicates"))
  (map 'list (lambda (s) (aref s 0)) raw-alphabet))

(defun process-blank (raw-blank)
  "Turn RAW-BLANK in a char"
  (unless (stringp raw-blank)
	(signal 'invalid-json-blank
			:msg "blank should be a string"))
  (unless (= (length raw-blank) 1)
	(signal 'invalid-json-blank
			:msg "blank should be of length 1"))
  (aref raw-blank 0))

(defun process-states (raw-states)
  "Turn RAW-STATES into a set of symbols"
  (unless (vectorp raw-states)
	(signal 'invalid-json-states
			:msg "states should be an array"))
  (unless (> (length raw-states) 0)
	(signal 'invalid-json-states
			:msg "states shouldn't be empty"))
  (unless (every (lambda (e) (stringp e)) raw-states)
	(signal 'invalid-json-states
			:msg "states should only contain strings."))
  (let ((raw-states-as-list (map 'list #'identity raw-states)))
	(unless (utils:utils-sets-setp raw-states-as-list :test #'equal)
	  (signal 'invalid-json-states
			  :msg "states shouldn't contain duplicates"))
	(map 'list
		 (lambda (s) (intern s (find-package 'keyword)))
		 raw-states-as-list)))

(defun process-initial-state (raw-initial)
  "Turn RAW-INITIAL into a symbol"
  (unless (stringp raw-initial)
	(signal 'invalid-json-initial-state
			:msg "initial should be a string"))
  (intern raw-initial (find-package 'keyword)))

(defun process-finals (raw-finals)
  "Turn RAW-FINALS into a set of symbols"
  (unless (vectorp raw-finals)
	(signal 'invalid-json-finals
			:msg "finals should be an array"))
  (unless (every #'stringp raw-finals)
	(signal 'invalid-json-finals
			:msg "finals should only contain strings"))
  (let ((raw-finals-as-list (map 'list #'identity raw-finals)))
	(unless raw-finals-as-list
	  (signal 'invalid-json-finals
			  :msg "finals shouldn't be empty"))
	(unless (utils:utils-sets-setp raw-finals-as-list :test #'equal)
	  (signal 'invalid-json-finals
			  :msg "finals shouldn't contain duplicates"))
	(map 'list
		 (lambda (s) (intern s (find-package 'keyword)))
		 raw-finals-as-list)))

(defun process-from-char (raw-from-char)
  (unless (stringp raw-from-char)
	(signal 'invalid-json-from-char
			:msg "Expecting \"read\" fields to be strings"))
  (unless (= (length raw-from-char) 1)
	(signal 'invalid-json-from-char
			:msg "Expecting \"read\" fields to be strings of length 1"))
  (aref raw-from-char 0))

(defun process-to-state (raw-to-state)
  (unless (stringp raw-to-state)
	(signal 'invalid-json-to-state
			:msg "\"to_state\" fields should be strings"))
  (unless (> (length raw-to-state) 0)
	(signal 'invalid-json-to-state
			:msg "\"to_state\" fields should not be empty"))
  (intern raw-to-state (find-package 'keyword)))

(defun process-to-char (raw-to-char)
  (unless (stringp raw-to-char)
	(signal 'invalid-json-to-char
			:msg "\"write\" fields should be strings"))
  (unless (= (length raw-to-char) 1)
	(signal 'invalid-json-to-char
			:msg "\"write\" fields should be strings of length 1"))
  (aref raw-to-char 0))

(defun process-action (action)
  (cond ((equal action "RIGHT") :right)
		((equal action "LEFT") :left)
		(t (signal 'invalid-json-action
				   :msg "\"action\" fields should be either \"RIGHT\" or \"LEFT\""))))

(defun process-string (string)
  "Turn STRING into a case-preserved keyword symbol"
  (unless (and (stringp string)
			   (> (length string) 0))
	(signal 'invalid-json-string))
  (intern string (find-package 'keyword)))

(defun process-transitions (raw-transitions)
  "Turn RAW-TRANSITIONS into an alist of alists of transitions"
  (unless (hash-table-p raw-transitions)
	(signal 'invalid-json-transitions
			:msg "transitions should be an object"))
  (flet ((extract-state-transitions (state-transitions-hts)
		   ;; Check type of state-transitions-hts. Vector is expected.
		   (unless (vectorp state-transitions-hts)
			 (signal 'invalid-json-transitions
					 :msg "For each from-state, transitions should be stored in arrays"))
		   (unless (every #'hash-table-p state-transitions-hts)
			 (signal 'invalid-json-transitions
					 :msg "Transitions should be stored as JSON objects"))
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
	(let ((res (loop :for k :being :the :hash-key
					   :using (hash-value v) :of raw-transitions
					 :collect (cons (intern k (find-package 'keyword))
									(extract-state-transitions v)))))
	  (unless
		  (every
		   (lambda (l)
			 (utils:utils-sets-setp (map 'list #'car (cdr l))))
		   res)
		(signal 'invalid-json-transitions
				:msg "Duplicate \"read\" fields for one state."))
	  res)))
