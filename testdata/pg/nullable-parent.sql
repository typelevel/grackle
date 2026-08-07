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

COPY nullable_parent_c (id, name) FROM STDIN WITH DELIMITER '|';
1|cat-1
\.

COPY nullable_parent_b (id, c_id, name) FROM STDIN WITH DELIMITER '|';
10|1|b-with-c
20|999|b-with-dangling-c
\.

COPY nullable_parent_a (id, b_id, name) FROM STDIN WITH DELIMITER '|';
100|10|a-with-good-b
200|20|a-with-dangling-b
300|\N|a-without-b
\.

CREATE TABLE nullable_parent_f (
  id INTEGER PRIMARY KEY,
  name TEXT NOT NULL
);

CREATE TABLE nullable_parent_e (
  id INTEGER PRIMARY KEY,
  d_id INTEGER NOT NULL,
  f_id INTEGER NOT NULL,
  name TEXT NOT NULL
);

CREATE TABLE nullable_parent_d (
  id INTEGER PRIMARY KEY,
  name TEXT NOT NULL
);

COPY nullable_parent_f (id, name) FROM STDIN WITH DELIMITER '|';
1|fish-1
2|fish-2
\.

COPY nullable_parent_e (id, d_id, f_id, name) FROM STDIN WITH DELIMITER '|';
10|100|1|e-with-f
11|100|2|e-with-another-f
\.

COPY nullable_parent_d (id, name) FROM STDIN WITH DELIMITER '|';
100|d-with-es
200|d-without-es
\.
