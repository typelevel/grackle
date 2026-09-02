CREATE TABLE array_join_root (
  id VARCHAR(100) PRIMARY KEY
);

CREATE TABLE array_join_list_a (
  id VARCHAR(100) PRIMARY KEY,
  root_id VARCHAR(100),
  a_elem JSON
);

CREATE TABLE array_join_list_b (
  id VARCHAR(100) PRIMARY KEY,
  root_id VARCHAR(100),
  b_elem INT
);
