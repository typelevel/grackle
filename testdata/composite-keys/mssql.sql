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

GO
