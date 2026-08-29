DROP TABLE IF EXISTS movies;

CREATE TABLE movies (
    id VARCHAR(36) PRIMARY KEY,
    title VARCHAR(100) NOT NULL,
    genre INTEGER NOT NULL,
    releasedate TEXT NOT NULL,
    showtime TEXT NOT NULL,
    nextshowing TEXT NOT NULL,
    duration INTEGER NOT NULL,
    categories VARCHAR(100) CHECK (json_valid(categories) = 1) NOT NULL,
    features VARCHAR(100) CHECK (json_valid(features) = 1) NOT NULL,
    tags INTEGER NOT NULL
);
