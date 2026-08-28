CREATE TABLE city (
    id integer NOT NULL,
    name nvarchar(100) NOT NULL,
    countrycode varchar(3) NOT NULL,
    district nvarchar(100) NOT NULL,
    population integer NOT NULL
);

CREATE TABLE country (
    code varchar(3) NOT NULL,
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
    code2 varchar(2) NOT NULL
);

CREATE TABLE countrylanguage (
    countrycode varchar(3) NOT NULL,
    language nvarchar(100) NOT NULL,
    isofficial bit NOT NULL,
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

GO
