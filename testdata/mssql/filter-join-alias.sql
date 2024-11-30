CREATE TABLE episodes3 (
  id VARCHAR(100),
  name VARCHAR(100),
  PRIMARY KEY (id, name)
);

CREATE TABLE images3 (
  public_url VARCHAR(100) PRIMARY KEY,
  id VARCHAR(100) NOT NULL,
  name VARCHAR(100) NOT NULL
);

INSERT INTO episodes3 (id, name) VALUES ('a', 'abc');

INSERT INTO images3 (public_url, id, name) VALUES ('test', 'a', 'abc');

GO
