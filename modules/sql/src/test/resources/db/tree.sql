CREATE TABLE bintree (
  id INTEGER PRIMARY KEY,
  left_child INTEGER,
  right_child INTEGER
);

COPY bintree (id, left_child, right_child) FROM STDIN WITH DELIMITER '|';
0|1|2
1|3|4
2|5|6
3|\N|\N
4|\N|\N
5|\N|\N
6|\N|\N
\.
