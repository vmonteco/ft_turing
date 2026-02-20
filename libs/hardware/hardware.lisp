(in-package :hardware)

;; For now, We hardcode a blank character.
;; It should be made dynamic later.

(defparameter *hw-blank* #\.
  "Blank character hardawre will use")
(defparameter *hw-side-display-size* 10
  "Number of characters to display on each size of the hardware head")

(defclass hardware ()
  ((head
	:accessor head
	:initarg :head
	:initform *hw-blank*)
   (left
	:accessor left
	:initarg :left
	:initform nil)
   (right
	:accessor right
	:initarg :right
	:initform nil)))

(defmethod print-object ((hw hardware) stream)
  (with-accessors ((head head)
				   (left left)
				   (right right))
	  hw
	(if (null *hw-side-display-size*)
		(format stream "[~{~c~}<~c>~{~c~}]"
				(reverse left) head right)
		(format stream "[~{~c~}<~c>~{~c~}]"
				(reverse (utils:truncate-or-complete-list
						  left *hw-side-display-size* *hw-blank*))
				head
				(utils:truncate-or-complete-list
				 right *hw-side-display-size* *hw-blank*)))))

(defun init-hardware (input)
  "Hardware constructor, takes an input string for initialization"
  (assert (stringp input))
  ;; We convert the input to a list of chars.
  (let ((input-as-list nil))
	(loop for i across (reverse input) do (push i input-as-list))
	(let ((head (if input-as-list (car input-as-list) *hw-blank*))
		  (right (cdr input-as-list))
		  (left nil))
	  (make-instance 'hardware
					 :head head
					 :left left
					 :right right))))

(defmethod move-right ((hw hardware))
  (with-accessors ((head head)
				   (left left)
				   (right right))
	  hw
	(when (or left (not (eql *hw-blank* head)))
		(push head left))
	(setq head (if right (pop right) *hw-blank*))
	hw))

(defmethod move-left ((hw hardware))
  (with-accessors ((head head)
				   (left left)
				   (right right))
	  hw
	(when (or right (not (eql *hw-blank* head)))
		(push head right))
	(setq head (if left (pop left) *hw-blank*))
	hw))

(defmethod read-head ((hw hardware))
  (head hw))

(defmethod write-head ((hw hardware) char)
  (setf (head hw) char)
  hw)
