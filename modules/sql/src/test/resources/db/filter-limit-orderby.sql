CREATE TABLE root (
  id VARCHAR PRIMARY KEY
);

CREATE TABLE lista (
  id VARCHAR PRIMARY KEY,
  root_id VARCHAR,
  a_elem VARCHAR
);

CREATE TABLE listb (
  id VARCHAR PRIMARY KEY,
  root_id VARCHAR,
  b_elem INTEGER
);

COPY root (id) FROM STDIN WITH DELIMITER '|';
r0
r1
\.

COPY lista (id, root_id, a_elem) FROM STDIN WITH DELIMITER '|';
a0|r0|foo
a1|r0|bar
a2|r0|baz
a3|r0|quux
a4|r1|foo1
a5|r1|bar1
a6|r1|baz1
a7|r1|quux1
\.

COPY listb (id, root_id, b_elem) FROM STDIN WITH DELIMITER '|';
b0|r0|23
b1|r0|13
b2|r0|17
b3|r0|11
b4|r1|231
b5|r1|131
b6|r1|171
b7|r1|111
\.
