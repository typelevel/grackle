CREATE TABLE bintree (
  id INTEGER PRIMARY KEY,
  left_child INTEGER,
  right_child INTEGER
);

INSERT INTO bintree (id, left_child, right_child) VALUES
(0, 1, 2),
(1, 3, 4),
(2, 5, 6),
(3, NULL, NULL),
(4, NULL, NULL),
(5, NULL, NULL),
(6, NULL, NULL);
