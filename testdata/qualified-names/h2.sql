CREATE SCHEMA qualified;

CREATE TABLE qualified.country (
    code CHAR(3) NOT NULL PRIMARY KEY,
    name VARCHAR NOT NULL
);

CREATE TABLE qualified.city (
    id INTEGER NOT NULL PRIMARY KEY,
    countrycode CHAR(3) NOT NULL,
    name VARCHAR NOT NULL
);

CREATE TABLE qualified.speaks (
    countrycode CHAR(3) NOT NULL,
    lang VARCHAR NOT NULL,
    PRIMARY KEY (countrycode, lang)
);

-- Deliberately named so that folding the qualifier of qualified.country with an underscore
-- yields this table's name: pins that synthesized aliases and real tables can coexist.
CREATE TABLE qualified_country (
    code CHAR(3) NOT NULL PRIMARY KEY,
    motto VARCHAR NOT NULL
);

INSERT INTO qualified.country (code, name) VALUES
('CAN', 'Canada'),
('DEU', 'Germany');

INSERT INTO qualified.city (id, countrycode, name) VALUES
(1, 'CAN', 'Toronto'),
(2, 'CAN', 'Ottawa'),
(3, 'DEU', 'Berlin');

INSERT INTO qualified.speaks (countrycode, lang) VALUES
('CAN', 'English'),
('CAN', 'French'),
('DEU', 'German');

INSERT INTO qualified_country (code, motto) VALUES
('CAN', 'A mari usque ad mare'),
('DEU', 'Einigkeit und Recht und Freiheit');
