CREATE TABLE level0 (
  id VARCHAR2(100) PRIMARY KEY
);

CREATE TABLE level1 (
  id VARCHAR2(100) PRIMARY KEY,
  level0_id VARCHAR2(100)
);

CREATE TABLE level2 (
  id VARCHAR2(100) PRIMARY KEY,
  level1_id VARCHAR2(100),
  attr BOOLEAN
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
('20', '10', 'FALSE'),
('21', '10', 'FALSE'),
('22', '11', 'FALSE'),
('23', '11', 'FALSE'),
('24', '12', 'TRUE'),
('25', '12', 'FALSE'),
('26', '13', 'FALSE'),
('27', '13', 'FALSE');
