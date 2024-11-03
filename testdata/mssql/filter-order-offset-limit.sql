CREATE TABLE root (
  id VARCHAR(100) PRIMARY KEY
);

CREATE TABLE lista (
  id VARCHAR(100) PRIMARY KEY,
  root_id VARCHAR(100),
  a_elem VARCHAR(100)
);

CREATE TABLE listb (
  id VARCHAR(100) PRIMARY KEY,
  root_id VARCHAR(100),
  b_elem INTEGER
);

INSERT INTO root (id) VALUES
('r0'),
('r1');

INSERT INTO lista (id, root_id, a_elem) VALUES
('a0', 'r0', 'foo'),
('a1', 'r0', 'bar'),
('a2', 'r0', 'baz'),
('a3', 'r0', 'quux'),
('a4', 'r1', 'foo1'),
('a5', 'r1', 'bar1'),
('a6', 'r1', 'baz1'),
('a7', 'r1', 'quux1');

INSERT INTO listb (id, root_id, b_elem) VALUES
('b0', 'r0', 23),
('b1', 'r0', 13),
('b2', 'r0', 17),
('b3', 'r0', 11),
('b4', 'r1', 231),
('b5', 'r1', 131),
('b6', 'r1', 171),
('b7', 'r1', 111);

GO
