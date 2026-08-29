CREATE TABLE films (
    title VARCHAR(100) PRIMARY KEY,
    synopsis_short VARCHAR(100),
    synopsis_long VARCHAR(100)
);

CREATE TABLE series (
    title VARCHAR(100) PRIMARY KEY,
    synopsis_short VARCHAR(100),
    synopsis_long VARCHAR(100)
);

CREATE TABLE episodes2 (
    title VARCHAR(100) PRIMARY KEY,
    series_title VARCHAR(100) NOT NULL,
    synopsis_short VARCHAR(100),
    synopsis_long VARCHAR(100)
);
