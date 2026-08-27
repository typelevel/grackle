CREATE TYPE string_array AS VARRAY(100) OF VARCHAR2(100);
/

CREATE TABLE array_join_root (
  id VARCHAR2(100) PRIMARY KEY
);

CREATE TABLE array_join_list_a (
  id VARCHAR2(100) PRIMARY KEY,
  root_id VARCHAR2(100),
  a_elem string_array
);

CREATE TABLE array_join_list_b (
  id VARCHAR2(100) PRIMARY KEY,
  root_id VARCHAR2(100),
  b_elem INTEGER
);

INSERT INTO array_join_root (id) VALUES
('r0'),
('r1');

INSERT INTO array_join_list_a (id, root_id, a_elem) VALUES
('a0', 'r0', string_array('foo1', 'foo2')),
('a1', 'r0', string_array('bar1', 'bar2')),
('a2', 'r0', string_array('baz1', 'baz2')),
('a3', 'r0', string_array('quux1', 'quux2')),
('a4', 'r1', string_array('foo11', 'foo22')),
('a5', 'r1', string_array('bar11', 'bar22')),
('a6', 'r1', string_array('baz11', 'baz22')),
('a7', 'r1', string_array('quux11', 'quux22'));


INSERT INTO array_join_list_b (id, root_id, b_elem) VALUES
('b0', 'r0', '23'),
('b1', 'r0', '13'),
('b2', 'r0', '17'),
('b3', 'r0', '11'),
('b4', 'r1', '231'),
('b5', 'r1', '131'),
('b6', 'r1', '171'),
('b7', 'r1', '111');
