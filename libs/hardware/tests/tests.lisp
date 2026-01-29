(in-package :hardware/tests)

(def-suite hardware-tests)
(in-suite hardware-tests)

(let ((*hw-blank* #\.))

  ;; Make instance usage:
  (test hardware-intance
		;; Without parameters on instanciation:
		(let ((hw (make-instance 'hardware)))
		  (is (eq *hw-blank* (head hw)))
		  (is-false (left hw))
		  (is-false (right hw)))
		;; With parameters:
		(let ((hw (make-instance 'hardware
								 :head #\0
								 :right '(#\1 #\2 #\3)
								 :left '(#\4 #\5 #\6))))
		  (is (eql #\0 (head hw)))
		  (is (equal '(#\1 #\2 #\3) (right hw)))
		  (is (equal '(#\4 #\5 #\6) (left hw)))))

  ;; init-hardware constructor:
  (test init-hardware
		;; Empty input:
		(let ((hw (init-hardware "")))
		  (is (eql *hw-blank* (head hw)))
		  (is-false (right hw))
		  (is-false (left hw)))
		;; Non-empty input:
		(let ((hw (init-hardware "abcd")))
		  (is (eql #\a (head hw)))
		  (is (equal '(#\b #\c #\d) (right hw)))
		  (is-false (left hw))))

  ;; move-right and move-left:
  (test move-right
		;; With default slots values input:
		(let ((hw (move-right (make-instance 'hardware))))
		  (is (eql *hw-blank* (head hw)))
		  (is-false (right hw))
		  (is-false (left hw)))		; We don't push blank to a nil.
		;; With only a value for head:
		(let ((hw (move-right (make-instance 'hardware :head #\0))))
		  (is (eql *hw-blank* (head hw)))
		  (is-false (right hw))
		  (is (equal (list #\0) (left hw))))
		;; With non-nil left only:
		(let ((hw (move-right (make-instance 'hardware :left '(#\0)))))
		  (is (eql *hw-blank* (head hw)))
		  (is-false (right hw))
		  (is (equal (list *hw-blank* #\0) (left hw))))
		;; With non-nil right only:
		(let ((hw (move-right (make-instance 'hardware :right '(#\0)))))
		  (is (eql #\0 (head hw)))
		  (is-false (right hw))
		  (is-false (left hw)))
		;; With every parameter
		(let ((hw (move-right (make-instance 'hardware
											 :head #\0
											 :right '(#\1 #\2 #\3)
											 :left '(#\4 #\5 #\6)))))
		  (is (eql #\1 (head hw)))
		  (is (equal '(#\2 #\3) (right hw)))
		  (is (equal '(#\0 #\4 #\5 #\6) (left hw)))))

  (test move-left
		;; With default slots values input:
		(let ((hw (move-left (make-instance 'hardware))))
		  (is (eql (head hw) *hw-blank*))
		  (is-false (right hw))		; We don't push blank to a nil.
		  (is-false (left hw)))
		;; With only a value for head:
		(let ((hw (move-left (make-instance 'hardware :head #\0))))
		  (is (eql *hw-blank* (head hw)))
		  (is (equal (list #\0) (right hw)))
		  (is-false (left hw)))
		;; With non-nil right only:
		(let ((hw (move-left (make-instance 'hardware :right '(#\0)))))
		  (is (eql *hw-blank* (head hw)))
		  (is (equal (list *hw-blank* #\0) (right hw)))
		  (is-false (left hw)))
		;; With non-nil left only:
		(let ((hw (move-left (make-instance 'hardware :left '(#\0)))))
		  (is (eql #\0 (head hw)))
		  (is-false (right hw))
		  (is-false (left hw)))
		;; With every parameter
		(let ((hw (move-left (make-instance 'hardware
											:head #\0
											:right '(#\4 #\5 #\6)
											:left '(#\1 #\2 #\3)))))
		  (is (eql #\1 (head hw)))
		  (is (equal '(#\0 #\4 #\5 #\6) (right hw)))
  		  (is (equal '(#\2 #\3) (left hw)))))

  (test read-head
		(let ((hw (make-instance 'hardware :head #\1)))
		  (is (eql #\1 (read-head hw)))))

  (test write-head
		(let ((hw (write-head (make-instance 'hardware) #\1)))
		  (is (eql #\1 (head hw))))))
