CREATE TABLE level0 (
  id VARCHAR(100) PRIMARY KEY
);

CREATE TABLE level1 (
  id VARCHAR(100) PRIMARY KEY,
  level0_id VARCHAR(100)
);

CREATE TABLE level2 (
  id VARCHAR(100) PRIMARY KEY,
  level1_id VARCHAR(100),
  attr BIT
);

INSERT INTO level0 (id) VALUES
('00'),
('01');

INSERT INTO level1 (id, level0_id) VALUES
('10', '00'),
('11', '00'),
('12', '01'),
('13', '01');

INSERT INTO level2 (id, level1_id, attr) VALUES
('20', '10', 0),
('21', '10', 0),
('22', '11', 0),
('23', '11', 0),
('24', '12', 1),
('25', '12', 0),
('26', '13', 0),
('27', '13', 0);

GO
