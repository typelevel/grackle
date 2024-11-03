CREATE TABLE root_2 (
  id VARCHAR(100) PRIMARY KEY
);

CREATE TABLE containers_2 (
  id VARCHAR(100) PRIMARY KEY,
  root_id VARCHAR(100)
);

CREATE TABLE lista_2 (
  id VARCHAR(100) PRIMARY KEY,
  container_id VARCHAR(100)
);

CREATE TABLE listb_2 (
  id VARCHAR(100) PRIMARY KEY,
  container_id VARCHAR(100)
);

INSERT INTO root_2 (id) VALUES
('r0'),
('r1');

INSERT INTO containers_2 (id, root_id) VALUES
('c0', 'r0'),
('c1', 'r0'),
('c2', 'r1'),
('c3', 'r1');

INSERT INTO lista_2 (id, container_id) VALUES
('a00', 'c0'),
('a01', 'c0'),
('a10', 'c1'),
('a11', 'c1'),
('a20', 'c2'),
('a21', 'c2'),
('a30', 'c3'),
('a31', 'c3');

INSERT INTO listb_2 (id, container_id) VALUES
('b00', 'c0'),
('b01', 'c0'),
('b10', 'c1'),
('b11', 'c1'),
('b20', 'c2'),
('b21', 'c2'),
('b30', 'c3'),
('b31', 'c3');

GO
