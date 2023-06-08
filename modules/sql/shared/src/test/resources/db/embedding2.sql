CREATE TABLE t_program (
  c_program_id  VARCHAR NOT NULL PRIMARY KEY
);

CREATE TABLE t_observation (
  c_program_id         VARCHAR    NOT NULL REFERENCES t_program(c_program_id),
  c_observation_id     VARCHAR    NOT NULL PRIMARY KEY
);

COPY t_program (c_program_id) FROM STDIN WITH DELIMITER '|';
foo
bar
\.

COPY t_observation (c_program_id, c_observation_id) FROM STDIN WITH DELIMITER '|';
foo|fo1
foo|fo2
bar|bo1
bar|bo2
\.
