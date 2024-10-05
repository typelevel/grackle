CREATE TABLE r (
    id VARCHAR(100) PRIMARY KEY
);

CREATE TABLE ca (
    id VARCHAR(100) PRIMARY KEY,
    rid VARCHAR(100) NOT NULL,
    a INTEGER NOT NULL
);

CREATE TABLE cb (
    id VARCHAR(100) PRIMARY KEY,
    rid VARCHAR(100) NOT NULL,
    b BOOLEAN NOT NULL
);

CREATE TABLE cc (
    id VARCHAR(100) PRIMARY KEY,
    rid VARCHAR(100) NOT NULL,
    c TIMESTAMP WITH TIME ZONE NOT NULL
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
('CB2a', 'R2', 'TRUE'),
('CB2b', 'R2', 'FALSE'),
('CB3', 'R3', 'TRUE');

INSERT INTO cc (id, rid, c) VALUES
('CC1', 'R1', TIMESTAMP '2020-05-27 21:00:00 +02:00'),
('CC2', 'R2', TIMESTAMP '2004-10-19 10:23:54 +02:00'),
('CC3', 'R3', TIMESTAMP '2014-10-19 10:23:54 +02:00');
