(in-package :utils)

(defmacro gethash-or-signal (hashtable name condition)
  "Access value associated with key NAME in HASHTABLE or signal CONDITION"
  `(multiple-value-bind (val present)
	   (gethash ,name ,hashtable)
	 (unless present
	   (signal ,condition))
	 val))
