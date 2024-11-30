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

INSERT INTO movies (id, title, genre, releasedate, showtime, nextshowing, duration, categories, features, tags) VALUES
('6a7837fc-b463-4d32-b628-0f4b3065cb21', 'Celine et Julie Vont en Bateau', '1', '1974-10-07', '19:35:00', '2020-05-22 19:35:00 +00:00', 12300000, '["drama","comedy"]', '["hd","hls"]', '1');

INSERT INTO movies (id, title, genre, releasedate, showtime, nextshowing, duration, categories, features, tags) VALUES
('11daf8c0-11c3-4453-bfe1-cb6e6e2f9115', 'Duelle', '1', '1975-09-15', '19:20:00', '2020-05-27 19:20:00 +00:00', 7260000, '["drama"]', '["hd"]', '1');

INSERT INTO movies (id, title, genre, releasedate, showtime, nextshowing, duration, categories, features, tags) VALUES
('aea9756f-621b-42d5-b130-71f3916c4ba3', 'L''Amour fou', '1', '1969-01-15', '21:00:00', '2020-05-27 21:00:00 +00:00', 15120000, '["drama"]', '["hd"]', '2');

INSERT INTO movies (id, title, genre, releasedate, showtime, nextshowing, duration, categories, features, tags) VALUES
('2ddb041f-86c2-4bd3-848c-990a3862634e', 'Last Year at Marienbad', '1', '1961-06-25', '20:30:00', '2020-05-26 20:30:00 +00:00', 5640000, '["drama"]', '["hd"]', '5');

INSERT INTO movies (id, title, genre, releasedate, showtime, nextshowing, duration, categories, features, tags) VALUES
('8ae5b13b-044c-4ff0-8b71-ccdb7d77cd88', 'Zazie dans le Métro', '3', '1960-10-28', '20:15:00', '2020-05-25 20:15:00 +00:00', 5340000, '["drama", "comedy"]', '["hd"]', '3');

INSERT INTO movies (id, title, genre, releasedate, showtime, nextshowing, duration, categories, features, tags) VALUES
('9dce9deb-9188-4cc2-9685-9842b8abdd34', 'Alphaville', '2', '1965-05-05', '19:45:00', '2020-05-19 19:45:00 +00:00', 5940000, '["drama", "science fiction"]', '["hd"]', '4');

INSERT INTO movies (id, title, genre, releasedate, showtime, nextshowing, duration, categories, features, tags) VALUES
('1bf00ac6-91ab-4e51-b686-3fd5e2324077', 'Stalker', '1', '1979-05-13', '15:30:00', '2020-05-19 15:30:00 +00:00', 9660000, '["drama", "science fiction"]', '["hd"]', '7');

INSERT INTO movies (id, title, genre, releasedate, showtime, nextshowing, duration, categories, features, tags) VALUES
('6a878e06-6563-4a0c-acd9-d28dcfb2e91a', 'Weekend', '3', '1967-12-29', '22:30:00', '2020-05-19 22:30:00 +00:00', 6300000, '["drama", "comedy"]', '["hd"]', '2');

INSERT INTO movies (id, title, genre, releasedate, showtime, nextshowing, duration, categories, features, tags) VALUES
('2a40415c-ea6a-413f-bbef-a80ae280c4ff', 'Daisies', '3', '1966-12-30', '21:30:00', '2020-05-15 21:30:00 +00:00', 4560000, '["drama", "comedy"]', '["hd"]', '6');

INSERT INTO movies (id, title, genre, releasedate, showtime, nextshowing, duration, categories, features, tags) VALUES
('2f6dcb0a-4122-4a21-a1c6-534744dd6b85', 'Le Pont du Nord', '1', '1982-01-13', '20:45:00', '2020-05-11 20:45:00 +00:00', 7620000, '["drama"]', '["hd"]', '3');

GO
