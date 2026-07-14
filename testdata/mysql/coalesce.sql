CREATE TABLE r (
    id VARCHAR(100) PRIMARY KEY
);

CREATE TABLE ca (
    id VARCHAR(100) PRIMARY KEY,
    rid VARCHAR(100) NOT NULL,
    a INT NOT NULL
);

CREATE TABLE cb (
    id VARCHAR(100) PRIMARY KEY,
    rid VARCHAR(100) NOT NULL,
    b BOOLEAN NOT NULL
);

CREATE TABLE cc (
    id VARCHAR(100) PRIMARY KEY,
    rid VARCHAR(100) NOT NULL,
    c DATETIME(6) NOT NULL
);

INSERT INTO r (id) VALUES
('R1'),
('R2'),
('R3');

INSERT INTO ca (id, rid, a) VALUES
('CA1a', 'R1', '10'),
('CA1b', 'R1', '11'),
('CA2', 'R2', '20'),
('CA3', 'R3', '30');

INSERT INTO cb (id, rid, b) VALUES
('CB2a', 'R2', 1),
('CB2b', 'R2', 0),
('CB3', 'R3', 1);

INSERT INTO cc (id, rid, c) VALUES
('CC1', 'R1', '2020-05-27 19:00:00'),
('CC2', 'R2', '2004-10-19 08:23:54'),
('CC3', 'R3', '2014-10-19 08:23:54');
