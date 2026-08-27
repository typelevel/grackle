CREATE TABLE nullable_parent_c (
  id INTEGER PRIMARY KEY,
  name TEXT NOT NULL
);

CREATE TABLE nullable_parent_b (
  id INTEGER PRIMARY KEY,
  c_id INTEGER NOT NULL,
  name TEXT NOT NULL
);

CREATE TABLE nullable_parent_a (
  id INTEGER PRIMARY KEY,
  b_id INTEGER,
  name TEXT NOT NULL
);

CREATE TABLE nullable_parent_f (
  id INTEGER PRIMARY KEY,
  name TEXT NOT NULL
);

CREATE TABLE nullable_parent_e (
  id INTEGER PRIMARY KEY,
  d_id INTEGER NOT NULL,
  f_id INTEGER NOT NULL,
  other_d_id INTEGER NOT NULL,
  name TEXT NOT NULL
);

CREATE TABLE nullable_parent_d (
  id INTEGER PRIMARY KEY,
  name TEXT NOT NULL
);
