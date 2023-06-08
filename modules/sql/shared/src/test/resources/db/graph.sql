CREATE TABLE graph_node (
  id INTEGER PRIMARY KEY
);

CREATE TABLE graph_edge (
  id INTEGER PRIMARY KEY,
  a INTEGER,
  b INTEGER
);

COPY graph_node (id) FROM STDIN WITH DELIMITER '|';
1
2
3
4
\.

COPY graph_edge (id, a, b) FROM STDIN WITH DELIMITER '|';
12|1|2
13|1|3
24|2|4
34|3|4
\.
