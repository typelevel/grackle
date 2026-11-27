CREATE TABLE limit_singular_join_root (
  id VARCHAR(64) PRIMARY KEY,
  name VARCHAR(64),
  code VARCHAR(64) NOT NULL
);

CREATE TABLE limit_singular_join_list_a (
  id VARCHAR(64) PRIMARY KEY,
  root_id VARCHAR(64)
);

CREATE TABLE limit_singular_join_list_b (
  id VARCHAR(64) PRIMARY KEY,
  root_id VARCHAR(64)
);

CREATE TABLE limit_singular_join_target (
  id VARCHAR(64) PRIMARY KEY,
  name VARCHAR(64)
);

CREATE TABLE limit_singular_join_assoc (
  root_id VARCHAR(64) NOT NULL,
  target_id VARCHAR(64) NOT NULL
);

GO
