-- A machine can be referred to by its name.
-- We don't need to store more in that table for results study.
CREATE TABLE IF NOT EXISTS machine (
       name VARCHAR(30) PRIMARY KEY
);

-- The state is an element of a machine's possible states.
-- It's mainly defined by its machine and its name.
-- Note that this allows duplicate states (by names) which seems acceptable
-- for the intended use.
CREATE TABLE IF NOT EXISTS state (
       id INTEGER PRIMARY KEY AUTOINCREMENT,
       machine_name VARCHAR(30) NOT NULL REFERENCES machine ON DELETE CASCADE,
       name VARCHAR(30) NOT NULL,
       UNIQUE (machine_name, name)
);

-- A character is an element of a machine's alphabet.
CREATE TABLE IF NOT EXISTS character (
       id INTEGER PRIMARY KEY AUTOINCREMENT, -- Apparently AUTOINCREMENT isn't possible with SMALLINT.
       machine VARCHAR(30) NOT NULL REFERENCES machine ON DELETE CASCADE,
       value CHAR(1) NOT NULL,
       UNIQUE (machine, value)
);

-- A result is identified by the input and the machine name.
CREATE TABLE IF NOT EXISTS result (
       id INTEGER PRIMARY KEY AUTOINCREMENT,
       machine_name VARCHAR(30) NOT NULL REFERENCES machine ON DELETE CASCADE,
       input VARCHAR(1024) NOT NULL,
       output VARCHAR(1024),
	   exited BOOLEAN NOT NULL DEFAULT TRUE,
       steps_number INT,
       end_state_id INTEGER REFERENCES state ON DELETE CASCADE,
	   UNIQUE (machine_name, input)
);

-- A step is just a pair of a character and a state.
-- It's an element of a result history.
-- It's identified by the result it's a part of and its index.
CREATE TABLE step (
       result_id INTEGER NOT NULL REFERENCES result ON DELETE CASCADE,
       idx INT, -- The index of the step _in the history of the result_.
       character_id INTEGER NOT NULL REFERENCES character ON DELETE CASCADE,
       state_id INTEGER NOT NULL REFERENCES state ON DELETE CASCADE,
	   -- (1) What's the right way to implement this constraint?
	    -- This doesn't work:
		-- CHECK ((SELECT machine_id FROM state WHERE id == state_id) == (SELECT machine_id FROM result WHERE id == result_id)
	   -- 		 AND (SELECT character_id FROM character WHERE id == character_id) == (SELECT machine_id FROM result WHERE id == result_id))
 	   -- (2) Does a multi-columns PK implies NOT NULL on each column?
       PRIMARY KEY (result_id, idx)
)

-- (3) Is there a better way than the many to one between result and step tables to implement an ordered list?
