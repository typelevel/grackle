CREATE SCHEMA qualified;

-- In MySQL a schema is a database, and the image grants the test user rights on its own
-- database only, so the tables below would be unreadable without this. The user named here
-- is MYSQL_USER/MARIADB_USER from docker-compose.yml; rename it there and these tables come
-- back as a denied SELECT at test time.
GRANT ALL PRIVILEGES ON qualified.* TO 'test'@'%';

CREATE TABLE qualified.country (
    code CHAR(3) NOT NULL PRIMARY KEY,
    name VARCHAR(64) NOT NULL
);

CREATE TABLE qualified.city (
    id INTEGER NOT NULL PRIMARY KEY,
    countrycode CHAR(3) NOT NULL,
    name VARCHAR(64) NOT NULL
);

CREATE TABLE qualified.speaks (
    countrycode CHAR(3) NOT NULL,
    lang VARCHAR(64) NOT NULL,
    PRIMARY KEY (countrycode, lang)
);

-- Deliberately named so that folding the qualifier of qualified.country with an underscore
-- yields this table's name: pins that synthesized aliases and real tables can coexist.
CREATE TABLE qualified_country (
    code CHAR(3) NOT NULL PRIMARY KEY,
    motto VARCHAR(64) NOT NULL
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
