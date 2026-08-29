CREATE TABLE films (
    title VARCHAR PRIMARY KEY,
    synopsis_short VARCHAR,
    synopsis_long VARCHAR
);

CREATE TABLE series (
    title VARCHAR PRIMARY KEY,
    synopsis_short VARCHAR,
    synopsis_long VARCHAR
);

CREATE TABLE episodes2 (
    title VARCHAR PRIMARY KEY,
    series_title VARCHAR NOT NULL,
    synopsis_short VARCHAR,
    synopsis_long VARCHAR
);
