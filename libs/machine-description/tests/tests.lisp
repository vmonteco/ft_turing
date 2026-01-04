(in-package :machine-description/tests)

(def-suite machine-description-tests)
(in-suite machine-description-tests)

;; machine description instanciation:
;; (test machine-description-instanciation
;; 	  (let ((md (make-instance 'machine-description)
;; 				:name "foo"
;; 				:alphabet '(#\0 #\1 #\.)
;; 				:states '(:start :error :success)
;; 				:initial :start
;; 				:finals '(:error :success)
;; 				:transitions '((:start . ((#\0 . (:start #\. :right))
;; 										  (#\1 . (:error #\. :right))
;; 										  (#\. . (:success #\. :right)))))))
;; 		(is (equal (name md) "foo"))
;; 		(is (equal (alphabet md) '(#\0 #\1 #\.)))
;; 		(is (equal (states md) '(:start :error :success)))
;; 		(is (equal (initial md) :start))
;; 		(is (equal (finals md) '(:error :success)
;; 		)

											   
