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
