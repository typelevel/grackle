DROP TABLE IF EXISTS movies;

CREATE TABLE movies (
    id VARCHAR(36) PRIMARY KEY,
    title VARCHAR(100) NOT NULL,
    genre INTEGER NOT NULL,
    releasedate DATE NOT NULL,
    showtime TIME NOT NULL,
    nextshowing DATETIMEOFFSET(7) NOT NULL,
    duration INTEGER NOT NULL,
    categories VARCHAR(100) CHECK (ISJSON(categories) = 1) NOT NULL,
    features VARCHAR(100) CHECK (ISJSON(features) = 1) NOT NULL,
    tags INTEGER NOT NULL
);

GO
