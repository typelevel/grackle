CREATE TYPE string_array AS VARRAY(100) OF VARCHAR2(100);
/

CREATE TABLE array_join_root (
  id VARCHAR2(100) PRIMARY KEY
);

CREATE TABLE array_join_list_a (
  id VARCHAR2(100) PRIMARY KEY,
  root_id VARCHAR2(100),
  a_elem string_array
);

CREATE TABLE array_join_list_b (
  id VARCHAR2(100) PRIMARY KEY,
  root_id VARCHAR2(100),
  b_elem INTEGER
);
