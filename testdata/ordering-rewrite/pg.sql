CREATE TABLE numbered_groups (
  id INTEGER PRIMARY KEY
);

CREATE TABLE numbered_items (
  code VARCHAR PRIMARY KEY,
  group_id INTEGER
);
