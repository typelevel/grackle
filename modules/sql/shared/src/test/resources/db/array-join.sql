CREATE TABLE array_join_root (
  id VARCHAR PRIMARY KEY
);

CREATE TABLE array_join_list_a (
  id VARCHAR PRIMARY KEY,
  root_id VARCHAR,
  a_elem VARCHAR[]
);

CREATE TABLE array_join_list_b (
  id VARCHAR PRIMARY KEY,
  root_id VARCHAR,
  b_elem INTEGER
);

COPY array_join_root (id) FROM STDIN WITH DELIMITER '|';
r0
r1
\.

COPY array_join_list_a (id, root_id, a_elem) FROM STDIN WITH DELIMITER '|';
a0|r0|{foo1,foo2}
a1|r0|{bar1,bar2}
a2|r0|{baz1,baz2}
a3|r0|{quux1,quux2}
a4|r1|{foo11,foo22}
a5|r1|{bar11,bar22}
a6|r1|{baz11,baz22}
a7|r1|{quux11,quux22}
\.

COPY array_join_list_b (id, root_id, b_elem) FROM STDIN WITH DELIMITER '|';
b0|r0|23
b1|r0|13
b2|r0|17
b3|r0|11
b4|r1|231
b5|r1|131
b6|r1|171
b7|r1|111
\.
