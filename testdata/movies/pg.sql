CREATE TABLE movies (
    id UUID PRIMARY KEY,
    title TEXT NOT NULL,
    genre INTEGER NOT NULL,
    releasedate DATE NOT NULL,
    showtime TIME NOT NULL,
    nextshowing TIMESTAMP WITH TIME ZONE NOT NULL,
    duration BIGINT NOT NULL,
    categories VARCHAR[] NOT NULL,
    features VARCHAR[] NOT NULL,
    tags INTEGER NOT NULL
);
