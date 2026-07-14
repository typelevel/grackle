CREATE TABLE null_ordering (
  id INTEGER PRIMARY KEY,
  v INTEGER
);

INSERT INTO null_ordering (id, v) VALUES
('1', '10'),
('2', NULL),
('3', '5'),
('4', NULL);

GO
