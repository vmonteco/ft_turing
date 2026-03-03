CREATE TABLE machine (name VARCHAR(30) PRIMARY KEY);
CREATE TABLE state (
	   machine VARCHAR(30),
	   name VARCHAR(30),
	   FOREIGN KEY (machine) REFERENCES (machine),
	   PRIMARY KEY (machine, name)
);
CREATE TABLE character (
	   machine VARCHAR(30),
	   value CHAR(1),
	   FOREIGN KEY (machine) REFERENCES (machine),
	   PRIMARY KEY (machine, name)
);
CREATE TABLE result (
	   machine VARCHAR(30),
	   input VARCHAR(500),
	   steps_number INT,
	   end_state VARCHAR(30),
	   output VARCHAR(500),
	   FOREIGN KEY (machine) REFERENCES (machine),
	   PRIMARY KEY (machine, input)
);
CREATE TABLE step (
	   result_machine VARCHAR(30),
	   result_input VARCHAR(500),
	  ! 
)

