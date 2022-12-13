CREATE TABLE episodes3 (
  id VARCHAR,
  name VARCHAR,
  PRIMARY KEY (id, name)
);

CREATE TABLE images3 (
  public_url VARCHAR PRIMARY KEY,
  id VARCHAR NOT NULL,
  name VARCHAR NOT NULL
);

INSERT INTO episodes3 (id, name) VALUES ('a', 'abc');

INSERT INTO images3 (public_url, id, name) VALUES ('test', 'a', 'abc');
