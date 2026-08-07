CREATE TABLE nullable_parent_c (
  id INTEGER PRIMARY KEY,
  name VARCHAR(64) NOT NULL
);

CREATE TABLE nullable_parent_b (
  id INTEGER PRIMARY KEY,
  c_id INTEGER NOT NULL,
  name VARCHAR(64) NOT NULL
);

CREATE TABLE nullable_parent_a (
  id INTEGER PRIMARY KEY,
  b_id INTEGER,
  name VARCHAR(64) NOT NULL
);

INSERT INTO nullable_parent_c (id, name) VALUES
(1, 'cat-1');

INSERT INTO nullable_parent_b (id, c_id, name) VALUES
(10, 1, 'b-with-c'),
(20, 999, 'b-with-dangling-c');

INSERT INTO nullable_parent_a (id, b_id, name) VALUES
(100, 10, 'a-with-good-b'),
(200, 20, 'a-with-dangling-b'),
(300, NULL, 'a-without-b');

CREATE TABLE nullable_parent_f (
  id INTEGER PRIMARY KEY,
  name VARCHAR(64) NOT NULL
);

CREATE TABLE nullable_parent_e (
  id INTEGER PRIMARY KEY,
  d_id INTEGER NOT NULL,
  f_id INTEGER NOT NULL,
  name VARCHAR(64) NOT NULL
);

CREATE TABLE nullable_parent_d (
  id INTEGER PRIMARY KEY,
  name VARCHAR(64) NOT NULL
);

INSERT INTO nullable_parent_f (id, name) VALUES
(1, 'fish-1'),
(2, 'fish-2');

INSERT INTO nullable_parent_e (id, d_id, f_id, name) VALUES
(10, 100, 1, 'e-with-f'),
(11, 100, 2, 'e-with-another-f');

INSERT INTO nullable_parent_d (id, name) VALUES
(100, 'd-with-es'),
(200, 'd-without-es');

GO
