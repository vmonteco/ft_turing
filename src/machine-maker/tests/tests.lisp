(in-package :machine-maker/tests-pkg)

(def-suite machine-maker-tests)

(def-suite hardware-tests :in machine-maker-tests)
(in-suite hardware-tests)

(let ((*hw-blank* #\.))

  ;; Make instance usage:
  (test hardware-intance-tests
		;; Without parameters on instanciation:
		(let ((hw (make-instance 'hardware)))
		  (is (eq (head hw) *hw-blank*))
		  (is (null (left hw)))
		  (is (null (right hw))))
		;; With parameters:
		(let ((hw (make-instance 'hardware
								 :head #\0
								 :right '(#\1 #\2 #\3)
								 :left '(#\4 #\5 #\6))))
		  (is (eql (head hw) #\0))
		  (is (equal (right hw) '(#\1 #\2 #\3)))
		  (is (equal (left hw) '(#\4 #\5 #\6)))))

  ;; init-hardware constructor:
  (test init-hardware-tests
		;; Empty input:
		(let ((hw (init-hardware "")))
		  (is (eql (head hw) *hw-blank*))
		  (is (null (right hw)))
		  (is (null (left hw))))
		;; Non-empty input:
		(let ((hw (init-hardware "abcd")))
		  (is (eql (head hw) #\a))
		  (is (equal (right hw) '(#\b #\c #\d)))
		  (is (null (left hw)))))

  ;; move-right and move-left:
  (test move-right-tests
		;; With default slots values input:
		(let ((hw (move-right (make-instance 'hardware))))
		  (is (eql (head hw) *hw-blank*))
		  (is (null (right hw)))
		  (is (null (left hw))))		; We don't push blank to a nil.
		;; With only a value for head:
		(let ((hw (move-right (make-instance 'hardware :head #\0))))
		  (is (eql (head hw) *hw-blank*))
		  (is (null (right hw)))
		  (is (equal (left hw) (list #\0))))
		;; With non-nil left only:
		(let ((hw (move-right (make-instance 'hardware :left '(#\0)))))
		  (is (eql (head hw) *hw-blank*))
		  (is (null (right hw)))
		  (is (equal (left hw) (list *hw-blank* #\0))))
		;; With non-nil right only:
		(let ((hw (move-right (make-instance 'hardware :right '(#\0)))))
		  (is (eql (head hw) #\0))
		  (is (null (right hw)))
		  (is (null (left hw))))
		;; With every parameter
		(let ((hw (move-right (make-instance 'hardware
											 :head #\0
											 :right '(#\1 #\2 #\3)
											 :left '(#\4 #\5 #\6)))))
		  (is (eql (head hw) #\1))
		  (is (equal (right hw) '(#\2 #\3)))
		  (is (equal (left hw) '(#\0 #\4 #\5 #\6)))))

  (test move-left-tests
		;; With default slots values input:
		(let ((hw (move-left (make-instance 'hardware))))
		  (is (eql (head hw) *hw-blank*))
		  (is (null (right hw)))		; We don't push blank to a nil.
		  (is (null (left hw))))
		;; With only a value for head:
		(let ((hw (move-left (make-instance 'hardware :head #\0))))
		  (is (eql (head hw) *hw-blank*))
		  (is (equal (right hw) (list #\0)))
		  (is (null (left hw))))
		;; With non-nil right only:
		(let ((hw (move-left (make-instance 'hardware :right '(#\0)))))
		  (is (eql (head hw) *hw-blank*))
		  (is (equal (right hw) (list *hw-blank* #\0)))
		  (is (null (left hw))))
		;; With non-nil left only:
		(let ((hw (move-left (make-instance 'hardware :left '(#\0)))))
		  (is (eql (head hw) #\0))
		  (is (null (right hw)))
		  (is (null (left hw))))
		;; With every parameter
		(let ((hw (move-left (make-instance 'hardware
											:head #\0
											:right '(#\4 #\5 #\6)
											:left '(#\1 #\2 #\3)))))
		  (is (eql (head hw) #\1))
		  (is (equal (right hw) '(#\0 #\4 #\5 #\6)))
  		  (is (equal (left hw) '(#\2 #\3)))))

  (test read-head
		(let ((hw (make-instance 'hardware :head #\1)))
		  (is (eql (read-head hw) #\1))))

  (test write-head
		(let ((hw (write-head (make-instance 'hardware) #\1)))
		  (is (eql (head hw) #\1)))))
