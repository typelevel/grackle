CREATE SCHEMA qualified;

CREATE TABLE qualified.country (
    code character(3) NOT NULL PRIMARY KEY,
    name text NOT NULL
);

CREATE TABLE qualified.city (
    id integer NOT NULL PRIMARY KEY,
    countrycode character(3) NOT NULL,
    name text NOT NULL
);

INSERT INTO qualified.country (code, name) VALUES
('CAN', 'Canada'),
('DEU', 'Germany');

INSERT INTO qualified.city (id, countrycode, name) VALUES
(1, 'CAN', 'Toronto'),
(2, 'CAN', 'Ottawa'),
(3, 'DEU', 'Berlin');
