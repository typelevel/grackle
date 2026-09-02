CREATE TABLE city (
    id INT NOT NULL PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    countrycode VARCHAR(3) NOT NULL,
    district VARCHAR(100) NOT NULL,
    population INT NOT NULL
);

CREATE TABLE country (
    code VARCHAR(3) NOT NULL PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    continent VARCHAR(100) NOT NULL,
    region VARCHAR(100) NOT NULL,
    surfacearea DOUBLE NOT NULL,
    indepyear SMALLINT,
    population INT NOT NULL,
    lifeexpectancy DOUBLE,
    gnp DECIMAL(10,2),
    gnpold DECIMAL(10,2),
    localname VARCHAR(100) NOT NULL,
    governmentform VARCHAR(100) NOT NULL,
    headofstate VARCHAR(100),
    capital INT,
    code2 VARCHAR(2) NOT NULL,
    FOREIGN KEY (capital) REFERENCES city(id)
);

CREATE TABLE countrylanguage (
    countrycode VARCHAR(3) NOT NULL,
    language VARCHAR(100) NOT NULL,
    isofficial BOOLEAN NOT NULL,
    percentage DOUBLE NOT NULL,
    PRIMARY KEY (countrycode, language),
    FOREIGN KEY (countrycode) REFERENCES country(code)
);
