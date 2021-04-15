DROP DATABASE IF EXISTS haskell_sports;
CREATE DATABASE IF NOT EXISTS haskell_sports;
USE haskell_sports;


DROP TABLE IF EXISTS Students;

CREATE TABLE IF NOT EXISTS Students (
id INT NOT NULL AUTO_INCREMENT,
name VARCHAR(256) NOT NULL,
surname VARCHAR(256) NOT NULL,
PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS Teachers(
id INT NOT NULL AUTO_INCREMENT,
name VARCHAR(256) NOT NULL,
surname VARCHAR(256) NOT NULL,
PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS Teacher_Student(
student_id INT NOT NULL,
teacher_id INT NOT NULL,
FOREIGN KEY (student_id) REFERENCES Students(id),
FOREIGN KEY (teacher_id) REFERENCES Teachers(id)
);

CREATE TABLE IF NOT EXISTS Sport_section(
id INT NOT NULL AUTO_INCREMENT,
name VARCHAR(256) NOT NULL,
PRIMARY KEY (id)
);


CREATE TABLE IF NOT EXISTS Training_schedule(
id INT NOT NULL AUTO_INCREMENT,
section_id INT NOT NULL,
teacher_id INT NOT NULL,
weekday VARCHAR(256) NOT NULL,
time_start TIME NOT NULL,
time_end TIME NOT NULL,
PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS Competition(
id INT NOT NULL AUTO_INCREMENT,
name VARCHAR(256) NOT NULL,
time_start DATETIME NOT NULL,
time_end DATETIME NOT NULL,
PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS Competitor(
student_id INT NOT NULL,
competition_id INT NOT NULL,

FOREIGN KEY (student_id) REFERENCES Students(id),
FOREIGN KEY (competition_id) REFERENCES Competition(id)
);


INSERT INTO Students(name, surname) VALUES ('st1', 'st1_s');
INSERT INTO Students(name, surname) VALUES ('st2', 'st2_s');

INSERT INTO Teachers(name, surname) VALUES ('teacher1', 'teacher1_s');
INSERT INTO Teachers(name, surname) VALUES ('teacher2', 'teacher2_s');

INSERT INTO Sport_section(name) VALUES ('section_1');
INSERT INTO Sport_section(name) VALUES ('section_2');


INSERT INTO Training_schedule(section_id, teacher_id, weekday, time_start, time_end) VALUES (1,1, 'Monday','10:30:00','12:20:00');

INSERT INTO Competition(name, time_start, time_end) VALUES ('classic competition', '2021-10-10 10:10:00', '2021-12-10 12:10:00');

