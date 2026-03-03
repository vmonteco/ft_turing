(in-package :bonus/tests)

(def-suite bonus-db-management-tests :in bonus-tests)
(in-suite bonus-db-management-tests)

(test init-db-test
	  ;; Ensure the lack of tables:
	  (with-open-database (db ":memory:")
		;; Ensure that there's no table
		(null (sqlite:execute-non-query
			   db
			   "SELECT name FROM sqlite_schema WHERE type='table' AND name not like 'sqlite_%';"))
		(bonus:initiate-db db)
		;; Ensure there are tables
		(let ((tables (sqlite:execute-to-list
					   db
					   "SELECT name FROM sqlite_schema WHERE type 
		))
