SET DEFINE OFF;

CREATE TABLE city (
    id integer NOT NULL,
    name nvarchar2(100) NOT NULL,
    countrycode char(3) NOT NULL,
    district nvarchar2(100) NOT NULL,
    population integer NOT NULL
);

CREATE TABLE country (
    code char(3) NOT NULL,
    name nvarchar2(100) NOT NULL,
    continent nvarchar2(100) NOT NULL,
    region nvarchar2(100) NOT NULL,
    surfacearea real NOT NULL,
    indepyear smallint,
    population integer NOT NULL,
    lifeexpectancy real,
    gnp numeric(10,2),
    gnpold numeric(10,2),
    localname nvarchar2(100) NOT NULL,
    governmentform nvarchar2(100) NOT NULL,
    headofstate nvarchar2(100),
    capital integer,
    code2 char(2) NOT NULL
);

CREATE TABLE countrylanguage (
    countrycode char(3) NOT NULL,
    language nvarchar2(100) NOT NULL,
    isofficial boolean NOT NULL,
    percentage real NOT NULL
);

--
-- Data for Name: country; Type: TABLE DATA; Schema: public; Owner: chriskl
--

--
-- Data for Name: countrylanguage; Type: TABLE DATA; Schema: public; Owner: chriskl
--

ALTER TABLE city
    ADD CONSTRAINT city_pkey PRIMARY KEY (id);

ALTER TABLE country
    ADD CONSTRAINT country_pkey PRIMARY KEY (code);

ALTER TABLE countrylanguage
    ADD CONSTRAINT countrylanguage_pkey PRIMARY KEY (countrycode, language);

ALTER TABLE country
    ADD CONSTRAINT country_capital_fkey FOREIGN KEY (capital) REFERENCES city(id);

ALTER TABLE countrylanguage
    ADD CONSTRAINT countrylanguage_countrycode_fkey FOREIGN KEY (countrycode) REFERENCES country(code);
