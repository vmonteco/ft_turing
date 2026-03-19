(in-package :bonus)

;; Constants
(defparameter *maximum-result-input-length* 1024)
(defparameter *maximum-result-output-length* 1024)
(defparameter *db-path* "ft_turing-db.sqlite"
  "Path to SQLITE database")

;; Queries:
;; Tables creation statements:
(defparameter *create-machine-table-statement*
  (format
   nil
   "CREATE TABLE IF NOT EXISTS machine (
    name VARCHAR(~d) PRIMARY KEY,
    md5sum CHAR(32) UNIQUE
);"
   machine-description:*maximum-machine-name-length*))

(defparameter *create-state-table-statement*
  (format
   nil
   "CREATE TABLE IF NOT EXISTS state (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    machine_name VARCHAR(~d) NOT NULL REFERENCES machine ON DELETE CASCADE,
    name VARCHAR(~d) NOT NULL,
    UNIQUE (machine_name, name)
);"
   machine-description:*maximum-machine-name-length*
   machine-description:*maximum-state-name-length*))

(defparameter *create-character-table-statement*
  (format
   nil
   "CREATE TABLE IF NOT EXISTS character (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    machine_name VARCHAR(1) NOT NULL REFERENCES machine ON DELETE CASCADE,
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
   machine-description:*maximum-machine-name-length*
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

(defun delete-machine (db machine-name)
  (sqlite:execute-non-query/named
   db
   "DELETE FROM machine WHERE name=:machine_name;"
   ":machine_name" machine-name))

(defun insert-alphabet (db machine-description)
  (sqlite:execute-non-query/named
   db
   (format nil "INSERT INTO character (machine_name, value) VALUES ~{(:machine_name, '~c')~^, ~};"
		   (machine-description:alphabet machine-description))
   ":machine_name" (machine-description:name machine-description)))

(defun insert-states (db machine-description)
  (sqlite:execute-non-query/named
   db
   (format nil "INSERT INTO state (machine_name, name) VALUES ~{(:machine_name, '~A')~^,~};"
		   (mapcar #'string (machine-description:states machine-description)))
   ":machine_name" (machine-description:name machine-description)))

;; API to expose:
(defun create-tables-if-not-exists (db-path)
  "Initialize tables"
  (sqlite:with-open-database (db db-path)
	(sqlite:execute-non-query
	 db *create-machine-table-statement*)
	(sqlite:execute-non-query
	 db *create-character-table-statement*)
	(sqlite:execute-non-query
	 db *create-state-table-statement*)
	(sqlite:execute-non-query
	 db *create-result-table-statement*)
	(sqlite:execute-non-query
	 db *create-step-table-statement*)))

(defun create-or-replace-machine (db-path
								  machine-description
								  md5sum)
  (sqlite:with-open-database (db db-path)
	(let* ((machine-name (machine-description:name machine-description))
		   (current-machine-md5sum
			 (sqlite:execute-single/named
			  db
			  "SELECT md5sum FROM machine WHERE name=:machine_name;"
			  ":machine_name" machine-name)))
	  (unless (equalp md5sum current-machine-md5sum)
		;; (when current-machine-md5sum
		;;   (sqlite:execute-non-query/named
		;;    db
		;;    "DELETE FROM machine WHERE md5sum=:md5sum;"
		;;    ":md5sum" current-machine-md5sum))
		(sqlite:execute-non-query/named
		 db
		 "INSERT OR REPLACE INTO machine (name, md5sum) VALUES (:name, :md5sum);"
		 ":name" machine-name
		 ":md5sum" md5sum)
		(insert-alphabet db machine-description)
		(insert-states db machine-description)))))

(defun insert-result-if-not-exists
	(db-path machine-name input hw history end-state number-of-steps)
  (sqlite:with-open-database (db db-path)
	(let ((alphabet-char-to-id-alist
			(loop :for l :in (sqlite:execute-to-list/named
							  db
							  "SELECT value, id FROM character WHERE machine_name = :machine_name;"
							  ":machine_name" machine-name)
				  :collect (list (aref (car l) 0) (second l))))
		  (states-name-to-id-alist
			(sqlite:execute-to-list/named
			 db
			 "SELECT name, id FROM state WHERE machine_name = :machine_name;"
			 ":machine_name" machine-name))
		  (current-result (sqlite:execute-single/named
						   db
						   "SELECT id FROM result WHERE machine_name=:machine_name AND input=:input;"
						   ":machine_name" machine-name
						   ":input" input)))
	  (unless current-result
		;; Add result
		(sqlite:execute-non-query/named
		 db
		 "INSERT INTO result (
    machine_name,
    input,
    output,
    steps_number,
    end_state_id
) VALUES (
    :machine_name,
    :input,
    :output,
    :steps_number,
    :end_state_id
);"
		 ":machine_name" machine-name
		 ":input" input
		 ":output" (hardware:get-output hw)
		 ":steps_number" number-of-steps
		 ":end_state_id" (second (assoc (string end-state) states-name-to-id-alist :test #'equal)))

		;; Add steps (history)
		(sqlite:execute-non-query/named
		 db
		 (format nil "INSERT INTO step (result_id, idx, character_id, state_id) VALUES ~:{(:result_id, ~A, ~A, ~A)~:^, ~};"
				 (let ((l (loop :for idx :from 0 :for e :in history
								:collect (list idx
													  (second (assoc (cdr e) alphabet-char-to-id-alist))
													  (second (assoc (string (car e)) states-name-to-id-alist :test #'equal))))))
				   l))
		 ":result_id" (sqlite:last-insert-rowid db))))))

(defun drop-tables (db-name)
  "Delete tables"
  (sqlite:with-open-database (db db-name)
	(sqlite:execute-non-query
	 db
	 "DROP TABLE IF EXISTS step;")
	(sqlite:execute-non-query
	 db
	 "DROP TABLE IF EXISTS result;")
	(sqlite:execute-non-query
	 db
	 "DROP TABLE IF EXISTS state;")
	(sqlite:execute-non-query
	 db
	 "DROP TABLE IF EXISTS character;")
	(sqlite:execute-non-query
	 db
	 "DROP TABLE IF EXISTS machine;")))

;; Util DB functions:
(defun create-alphabet (db machine-description)
  (let ((alphabet (machine-description:alphabet machine-description)))
	(sqlite:execute-non-query/named
	 db
	 (format nil "INSERT INTO characters VALUES ~{(:machine_name, '~c')~^,~};"
			 (machine-description:alphabet machine-description))
	 ":machine_name" (machine-description:name machine-description))))

(defun create-states (db machine-description)
  (let ((states (machine-description:states machine-description)))
	(sqlite:execute-non-query/named
	 db
	 (format nil "INSERT INTO characters VALUES ~{(:machine_name, '~c')~^,~};"
			 (machine-description:states machine-description))
	 ":machine_name" (machine-description:name machine-description))))

;; Bonus DB API:
(defun create-or-replace-machine-in-db (db machine-description md5sum)
  ;; Check existing machine
  (let ((machine-name (machine-description:name machine-description))
		(existing-md5sum (sqlite:execute-single/named
				 db "SELECT md5sum FROM machine WHERE name=:name"
				 ":name" (machine-description:name machine-description))))
	;; If exists and must be deleted:
	(when (and existing-md5sum (not (equal nd5sum)))
		(delete-machine-row db machine_name)) ; This should delete all related entries in other tables.
	;; If the entry is to be created or replaced:
	(when (or (null existing-md5sum) (not (equal md5sum)))
		(insert-machine-raw db machine-name md5sum)
		(create-alphabet db machine-description)
		(create-states db machine-description))))

(defun get-machine-results (db-path machine-name)
  (sqlite:with-open-database (db db-path)
	(sqlite:execute-to-list/named
	 db
	 "SELECT LENGTH(input), steps_number FROM result WHERE machine_name = :machine_name;"
	 ":machine_name" machine-name)))
