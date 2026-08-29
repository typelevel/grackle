CREATE TABLE city (
    id integer NOT NULL PRIMARY KEY,
    name VARCHAR NOT NULL,
    countrycode CHAR(3) NOT NULL,
    district VARCHAR NOT NULL,
    population integer NOT NULL
);

CREATE TABLE country (
    code CHAR(3) NOT NULL PRIMARY KEY,
    name VARCHAR NOT NULL,
    continent VARCHAR NOT NULL,
    region VARCHAR NOT NULL,
    surfacearea real NOT NULL,
    indepyear smallint,
    population integer NOT NULL,
    lifeexpectancy real,
    gnp numeric(10,2),
    gnpold numeric(10,2),
    localname VARCHAR NOT NULL,
    governmentform VARCHAR NOT NULL,
    headofstate VARCHAR,
    capital integer,
    code2 CHAR(2) NOT NULL,
    FOREIGN KEY (capital) REFERENCES city(id)
);

CREATE TABLE countrylanguage (
    countrycode CHAR(3) NOT NULL,
    language VARCHAR NOT NULL,
    isofficial BOOLEAN NOT NULL,
    percentage real NOT NULL,
    PRIMARY KEY (countrycode, language),
    FOREIGN KEY (countrycode) REFERENCES country(code)
);
