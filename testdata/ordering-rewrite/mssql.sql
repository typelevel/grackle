CREATE TABLE numbered_groups (
  id INTEGER PRIMARY KEY
);

CREATE TABLE numbered_items (
  code VARCHAR(100) PRIMARY KEY,
  group_id INTEGER,
  threshold VARCHAR(100)
);

GO
