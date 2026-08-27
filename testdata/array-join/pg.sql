CREATE TABLE array_join_root (
  id VARCHAR PRIMARY KEY
);

CREATE TABLE array_join_list_a (
  id VARCHAR PRIMARY KEY,
  root_id VARCHAR,
  a_elem VARCHAR[]
);

CREATE TABLE array_join_list_b (
  id VARCHAR PRIMARY KEY,
  root_id VARCHAR,
  b_elem INTEGER
);
