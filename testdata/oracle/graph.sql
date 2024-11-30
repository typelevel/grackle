CREATE TABLE graph_node (
  id INTEGER PRIMARY KEY
);

CREATE TABLE graph_edge (
  id INTEGER PRIMARY KEY,
  a INTEGER,
  b INTEGER
);

INSERT INTO graph_node (id) VALUES
('1'),
('2'),
('3'),
('4');

INSERT INTO graph_edge (id, a, b) VALUES
('12', '1', '2'),
('13', '1', '3'),
('24', '2', '4'),
('34', '3', '4');
