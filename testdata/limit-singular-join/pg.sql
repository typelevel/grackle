CREATE TABLE limit_singular_join_root (
  id VARCHAR PRIMARY KEY,
  name VARCHAR,
  code VARCHAR NOT NULL
);

CREATE TABLE limit_singular_join_list_a (
  id VARCHAR PRIMARY KEY,
  root_id VARCHAR
);

CREATE TABLE limit_singular_join_list_b (
  id VARCHAR PRIMARY KEY,
  root_id VARCHAR
);

CREATE TABLE limit_singular_join_target (
  id VARCHAR PRIMARY KEY,
  name VARCHAR
);

CREATE TABLE limit_singular_join_assoc (
  root_id VARCHAR NOT NULL,
  target_id VARCHAR NOT NULL
);
