CREATE TABLE composite_key_parent (
  key_1 INTEGER NOT NULL,
  key_2 VARCHAR(100) NOT NULL,
  PRIMARY KEY (key_1, key_2)
);

CREATE TABLE composite_key_child (
  id INTEGER PRIMARY KEY,
  parent_1 INTEGER NOT NULL,
  parent_2 VARCHAR(100) NOT NULL,
  FOREIGN KEY (parent_1, parent_2) REFERENCES composite_key_parent (key_1, key_2)
);

INSERT INTO composite_key_parent (key_1, key_2) VALUES
(1, 'foo'),
(1, 'bar'),
(2, 'foo'),
(2, 'bar');

INSERT INTO composite_key_child (id, parent_1, parent_2) VALUES
(1, 1, 'foo'),
(2, 1, 'bar'),
(3, 2, 'foo'),
(4, 2, 'bar');

GO
