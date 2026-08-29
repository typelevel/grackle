DROP TABLE IF EXISTS movies;

CREATE TABLE movies (
    id UUID PRIMARY KEY,
    title VARCHAR NOT NULL,
    genre INTEGER NOT NULL,
    releasedate DATE NOT NULL,
    showtime TIME NOT NULL,
    nextshowing TIMESTAMP(9) WITH TIME ZONE NOT NULL,
    duration BIGINT NOT NULL,
    categories VARCHAR ARRAY NOT NULL,
    features VARCHAR ARRAY NOT NULL,
    tags INTEGER NOT NULL
);
