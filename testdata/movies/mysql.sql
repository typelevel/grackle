DROP TABLE IF EXISTS movies;

CREATE TABLE movies (
    id VARCHAR(36) PRIMARY KEY,
    title VARCHAR(100) NOT NULL,
    genre INT NOT NULL,
    releasedate DATE NOT NULL,
    showtime TIME(6) NOT NULL,
    nextshowing DATETIME(6) NOT NULL,
    duration INT NOT NULL,
    categories JSON NOT NULL,
    features JSON NOT NULL,
    tags INT NOT NULL
);
