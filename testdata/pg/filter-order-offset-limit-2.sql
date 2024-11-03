CREATE TABLE root_2 (
  id VARCHAR PRIMARY KEY
);

CREATE TABLE containers_2 (
  id VARCHAR PRIMARY KEY,
  root_id VARCHAR
);

CREATE TABLE lista_2 (
  id VARCHAR PRIMARY KEY,
  container_id VARCHAR
);

CREATE TABLE listb_2 (
  id VARCHAR PRIMARY KEY,
  container_id VARCHAR
);

COPY root_2 (id) FROM STDIN WITH DELIMITER '|';
r0
r1
\.

COPY containers_2 (id, root_id) FROM STDIN WITH DELIMITER '|';
c0|r0
c1|r0
c2|r1
c3|r1
\.

COPY lista_2 (id, container_id) FROM STDIN WITH DELIMITER '|';
a00|c0
a01|c0
a10|c1
a11|c1
a20|c2
a21|c2
a30|c3
a31|c3
\.

COPY listb_2 (id, container_id) FROM STDIN WITH DELIMITER '|';
b00|c0
b01|c0
b10|c1
b11|c1
b20|c2
b21|c2
b30|c3
b31|c3
\.
