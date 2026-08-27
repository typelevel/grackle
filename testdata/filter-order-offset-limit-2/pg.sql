CREATE TABLE root_2 (
  id VARCHAR PRIMARY KEY
);

CREATE TABLE containers_2 (
  id VARCHAR PRIMARY KEY,
  root_id VARCHAR
);

CREATE TABLE lista_2 (
  id VARCHAR PRIMARY KEY,
  container_id VARCHAR
);

CREATE TABLE listb_2 (
  id VARCHAR PRIMARY KEY,
  container_id VARCHAR
);
