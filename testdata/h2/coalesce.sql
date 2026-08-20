CREATE TABLE r (
    id VARCHAR PRIMARY KEY
);

CREATE TABLE ca (
    id VARCHAR PRIMARY KEY,
    rid VARCHAR NOT NULL,
    a INTEGER NOT NULL
);

CREATE TABLE cb (
    id VARCHAR PRIMARY KEY,
    rid VARCHAR NOT NULL,
    b BOOLEAN NOT NULL
);

CREATE TABLE cc (
    id VARCHAR PRIMARY KEY,
    rid VARCHAR NOT NULL,
    c TIMESTAMP(9) WITH TIME ZONE NOT NULL
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
('CC1', 'R1', '2020-05-27 21:00:00+02:00'),
('CC2', 'R2', '2004-10-19 10:23:54+02:00'),
('CC3', 'R3', '2014-10-19 10:23:54+02:00');
