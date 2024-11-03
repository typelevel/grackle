CREATE TABLE t_program (
  c_program_id  VARCHAR(100) NOT NULL PRIMARY KEY
);

CREATE TABLE t_observation (
  c_program_id         VARCHAR(100)    NOT NULL REFERENCES t_program(c_program_id),
  c_observation_id     VARCHAR(100)    NOT NULL PRIMARY KEY
);

INSERT INTO t_program (c_program_id) VALUES
('foo'),
('bar');

INSERT INTO t_observation (c_program_id, c_observation_id) VALUES
('foo', 'fo1'),
('foo', 'fo2'),
('bar', 'bo1'),
('bar', 'bo2');
