CREATE TABLE films (
    title TEXT PRIMARY KEY,
    synopsis_short TEXT,
    synopsis_long TEXT
);

CREATE TABLE series (
    title TEXT PRIMARY KEY,
    synopsis_short TEXT,
    synopsis_long TEXT
);

CREATE TABLE episodes2 (
    title TEXT PRIMARY KEY,
    series_title TEXT NOT NULL,
    synopsis_short TEXT,
    synopsis_long TEXT
);
