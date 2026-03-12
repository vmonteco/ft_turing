(in-package :bonus)

;; Constants
(defparameter *naximum-machine-name-length* 30)
(defparameter *maximum-state-name-length* 30)
(defparameter *maximum-result-input-length* 1024)
(defparameter *maximum-result-output-length* 1024)

;; Queries:
;; Tables creation statements:
(defparameter *create-machine-table-statement*
  (format
   nil
   "CREATE TABLE IF NOT EXISTS machine (
    name VARCHAR(~d) PRIMARY KEY,
    md5sum CHAR(32) UNIQUE
);"
   *maximum-machine-name-length*))

(defparameter *create-state-table-statement*
  (format
   nil
   "CREATE TABLE IF NOT EXISTS state (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    machine_name VARCHAR(~d) NOT NULL REFERENCES machine ON DELETE CASCADE,
    name VARCHAR(~d) NOT NULL,
    UNIQUE (machine_name, name)
);"
   *maximum-machine-name-length*
   *maximum-state-name-length*))

(defparameter *create-character-table-statement*
  (format
   nil
   "CREATE TABLE IF NOT EXISTS character (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    machine_name VARCHAR(~d) NOT NULL REFERENCES machine ON DELETE CASCADE,
    value CHAR(1) NOT NULL,
    UNIQUE (machine_name, value)
);"))

(defparameter *create-result-table-statement*
  (format
   nil
   "CREATE TABLE IF NOT EXISTS result (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    machine_name VARCHAR(~d) NOT NULL REFERENCES machine ON DELETE CASCADE,
    input VARCHAR(~d) NOT NULL,
    output VARCHAR(~d) NOT NULL,
    steps_number INTEGER NOT NULL,
    end_state_id INTEGER REFERENCES state ON DELETE CASCADE NOT NULL,
    UNIQUE (machine_name, input)
);"
   *maximum-machine-name-length*
   *maximum-result-input-length*
   *maximum-result-output-length*))
 
(defparameter *create-step-table-statement*
  "CREATE TABLE IF NOT EXISTS step (
    result_id INTEGER NOT NULL REFERENCES result ON DELETE CASCADE,
    idx INTEGER NOT NULL,
    character_id INTEGER NOT NULL REFERENCES character ON DELETE CASCADE,
    state_id INTEGER NOT NULL REFERENCES state ON DELETE CASCADE,
    PRIMARY KEY (result_id, idx)
);")

(defparameter *db-file* nil
  "Path to SQLITE database")

(defun create-tables-if-not-exists (db)
  "Initialize tables"
  (sqlite:execute-non-query
   db
   (concatenate
	'string
	*create-machine-table-statement*
	*create-character-table-statement*
	*create-state-table-statement*
	*create-resullt-table-statement*
	*create-step-table-statement*)))

(defun drop-tables (db)
  "Delete tables"
  (sqlite:execute-non-query
   db
   "DROP TABLE IF EXISTS machine, character, state, result, step"))

;; Util DB functions:
(defun create-alphabet (db machine-description)
  (let ((alphabet (machine-description:alphabet machine-description)))
	(sqlite:execute-non-query
	 db
	 (format nil "INSERT INTO characters VALUES ~{(:machine-name, '~c')~^,~};"
			 (machine-description:alphabet machine-description))
	 :machine-name (machine-description:name machine-description))))

(defun create-states (db machine-description)
  (let ((states (machine-description:states machine-description)))
	(sqlite:execute-non-query
	 db
	 (format nil "INSERT INTO characters VALUES ~{(:machine-name, '~c')~^,~};"
			 (machine-description:states machine-description))
	 :machine-name (machine-description:name machine-description))))

;; Bonus DB API:
(defun create-or-replace-machine-in-db (db machine-description md5sum)
  ;; Check existing machine
  (let ((machine-name (machine-description:name machine-description))
		(existing-md5sum (sqlite:execute-single/named
				 db "SELECT md5sum FROM machine WHERE name=:name"
				 :name (machine-description:name machine-description))))
	;; If exists and must be deleted:
	(when (and existing-md5sum (not (equal nd5sum)))
		(delete-machine-row db machine-name)) ; This should delete all related entries in other tables.
	;; If the entry is to be created or replaced:
	(when (or (null existing-md5sum) (not (equal md5sum)))
		(insert-machine-raw db machine-name md5sum)
		(create-alphabet db machine-description)
		(create-states db machine-description))))

(defun get-machine-results (db machine-name)
  (sqlite:execute-to-list/named
   db
   "SELECT step_number, LENGTH(input) FROM result WHERE machine_name = :machine-name;"
   :machine-name machine-name))

;; (defun create-result-if-not-exists (db machine-name input hardware history number-of-steps)
;;   (let ((state-to-id-table (sqlite:execute-to-list/named db "SELECT )
;; 		(character-to-id-table ))
;; 	(sqlite:execute-non-query/named
;; 	 db
;; 	 "INSERT INTO result (machine_name, input output, steps_number, end_state_id)")
