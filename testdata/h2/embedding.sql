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

INSERT INTO films (title, synopsis_short, synopsis_long) VALUES
('Film 1', 'Short film 1', 'Long film 1'),
('Film 2', 'Short film 2', 'Long film 2'),
('Film 3', 'Short film 3', 'Long film 3');

INSERT INTO series (title, synopsis_short, synopsis_long) VALUES
('Series 1', 'Short series 1', 'Long series 1'),
('Series 2', 'Short series 2', 'Long series 2'),
('Series 3', 'Short series 3', 'Long series 3');

INSERT INTO episodes2 (title, series_title, synopsis_short, synopsis_long) VALUES
('S01E01', 'Series 1', 'Short S01E01', 'Long S01E01'),
('S01E02', 'Series 1', 'Short S01E02', 'Long S01E02'),
('S01E03', 'Series 1', 'Short S01E03', 'Long S01E03'),
('S02E01', 'Series 2', 'Short S02E01', 'Long S02E01'),
('S02E02', 'Series 2', 'Short S02E02', 'Long S02E02'),
('S02E03', 'Series 2', 'Short S02E03', 'Long S02E03'),
('S03E01', 'Series 3', 'Short S03E01', 'Long S03E01'),
('S03E02', 'Series 3', 'Short S03E02', 'Long S03E02'),
('S03E03', 'Series 3', 'Short S03E03', 'Long S03E03');
