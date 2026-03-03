-- A machine can be referred to by its name.
-- We don't need to store more in that table for results study.
CREATE TABLE machine (
       name VARCHAR(30) PRIMARY KEY
);

-- The state is an element of a machine's possible states.
-- It's mainly defined by its machine and its name.
-- Note that this allows duplicate states (by names).
-- Maybe 
CREATE TABLE state (
       id INT PRIMARY KEY AUTOINCREMENT,
       machine_name VARCHAR(30),
       name VARCHAR(30),
       FOREIGN KEY (machine_name) REFERENCES (machine),
       UNIQUE (machine_name, name)
);

-- A character is an element of a machine's alphabet.
CREATE TABLE character (
       id SMALLINT PRIMARY KEY AUTOINCREMENT,
       machine VARCHAR(30),
       value CHAR(1),
       FOREIGN KEY (machine) REFERENCES (machine),
       UNIQUE (machine, value)
);

-- A result is identified by the input and the machine name.
CREATE TABLE result (
       id INT PRIMARY KEY AUTOINCREMENT,
       machine_name VARCHAR(30),
       input VARCHAR(1024),
       output VARCHAR(1024),
       steps_number INT,
       end_stateID INT,
       FOREIGN KEY (machine_name) REFERENCES (machine),
       FOREIGN KEY (end_state_id) REFERENCES (state),
       PRIMARY KEY (machine, input)
);

-- A step is just a pair of a character and a state.
-- It's identified by the result it's a part of and its index.
CREATE TABLE step (
       result_id INT,
       index INT,
       character_id SMALLINT,
       state_id INT,
       FOREIGN KEY (result_id) REFERENCES (result),
       FOREIGN KEY (character_id) REFERENCES (character),
       FOREIGN KEY (state_id) REFERENCES (state),
       PRIMARY KEY (result_id, index)
)
