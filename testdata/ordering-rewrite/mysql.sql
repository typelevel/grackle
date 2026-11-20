CREATE TABLE numbered_groups (
  id INT PRIMARY KEY
);

CREATE TABLE numbered_items (
  code VARCHAR(100) PRIMARY KEY,
  group_id INT
);
