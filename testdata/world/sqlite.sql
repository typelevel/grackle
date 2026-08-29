CREATE TABLE city (
    id integer NOT NULL PRIMARY KEY,
    name nvarchar(100) NOT NULL,
    countrycode varchar(3) NOT NULL,
    district nvarchar(100) NOT NULL,
    population integer NOT NULL
);

CREATE TABLE country (
    code varchar(3) NOT NULL PRIMARY KEY,
    name nvarchar(100) NOT NULL,
    continent nvarchar(100) NOT NULL,
    region nvarchar(100) NOT NULL,
    surfacearea real NOT NULL,
    indepyear smallint,
    population integer NOT NULL,
    lifeexpectancy real,
    gnp numeric(10,2),
    gnpold numeric(10,2),
    localname nvarchar(100) NOT NULL,
    governmentform nvarchar(100) NOT NULL,
    headofstate nvarchar(100),
    capital integer,
    code2 varchar(2) NOT NULL,
    FOREIGN KEY (capital) REFERENCES city(id)
);

CREATE TABLE countrylanguage (
    countrycode varchar(3) NOT NULL,
    language nvarchar(100) NOT NULL,
    isofficial integer NOT NULL,
    percentage real NOT NULL,
    PRIMARY KEY (countrycode, language),
    FOREIGN KEY (countrycode) REFERENCES country(code)
);
