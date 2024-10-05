CREATE TYPE string_array2 AS VARRAY(100) OF VARCHAR2(100);
/

CREATE TABLE movies (
    id VARCHAR(36) PRIMARY KEY,
    title VARCHAR(100) NOT NULL,
    genre INTEGER NOT NULL,
    releasedate DATE NOT NULL,
    showtime INTERVAL DAY (0) TO SECOND (0) NOT NULL,
    nextshowing TIMESTAMP WITH TIME ZONE NOT NULL,
    duration NUMBER(18) NOT NULL,
    categories string_array2 NOT NULL,
    features string_array2 NOT NULL,
    tags INTEGER NOT NULL
);

INSERT INTO movies (id, title, genre, releasedate, showtime, nextshowing, duration, categories, features, tags) VALUES
('6a7837fc-b463-4d32-b628-0f4b3065cb21', 'Celine et Julie Vont en Bateau', '1', DATE '1974-10-07', INTERVAL '0 19:35:00' DAY TO SECOND (0), TIMESTAMP '2020-05-22 19:35:00 +00:00', '12300000', string_array2('drama','comedy'), string_array2('hd','hls'), '1');

INSERT INTO movies (id, title, genre, releasedate, showtime, nextshowing, duration, categories, features, tags) VALUES
('11daf8c0-11c3-4453-bfe1-cb6e6e2f9115', 'Duelle', '1', DATE '1975-09-15', INTERVAL '0 19:20:00' DAY TO SECOND (0), TIMESTAMP '2020-05-27 19:20:00 +00:00', '7260000', string_array2('drama'), string_array2('hd'), '1');

INSERT INTO movies (id, title, genre, releasedate, showtime, nextshowing, duration, categories, features, tags) VALUES
('aea9756f-621b-42d5-b130-71f3916c4ba3', 'L''Amour fou', '1', DATE '1969-01-15', INTERVAL '0 21:00:00' DAY TO SECOND (0), TIMESTAMP '2020-05-27 21:00:00 +00:00', '15120000', string_array2('drama'), string_array2('hd'), '2');

INSERT INTO movies (id, title, genre, releasedate, showtime, nextshowing, duration, categories, features, tags) VALUES
('2ddb041f-86c2-4bd3-848c-990a3862634e', 'Last Year at Marienbad', '1', DATE '1961-06-25', INTERVAL '0 20:30:00' DAY TO SECOND (0), TIMESTAMP '2020-05-26 20:30:00 +00:00', '5640000', string_array2('drama'), string_array2('hd'), '5');

INSERT INTO movies (id, title, genre, releasedate, showtime, nextshowing, duration, categories, features, tags) VALUES
('8ae5b13b-044c-4ff0-8b71-ccdb7d77cd88', 'Zazie dans le Métro', '3', DATE '1960-10-28', INTERVAL '0 20:15:00' DAY TO SECOND (0), TIMESTAMP '2020-05-25 20:15:00 +00:00', '5340000', string_array2('drama', 'comedy'), string_array2('hd'), '3');

INSERT INTO movies (id, title, genre, releasedate, showtime, nextshowing, duration, categories, features, tags) VALUES
('9dce9deb-9188-4cc2-9685-9842b8abdd34', 'Alphaville', '2', DATE '1965-05-05', INTERVAL '0 19:45:00' DAY TO SECOND (0), TIMESTAMP '2020-05-19 19:45:00 +00:00', '5940000', string_array2('drama', 'science fiction'), string_array2('hd'), '4');

INSERT INTO movies (id, title, genre, releasedate, showtime, nextshowing, duration, categories, features, tags) VALUES
('1bf00ac6-91ab-4e51-b686-3fd5e2324077', 'Stalker', '1', DATE '1979-05-13', INTERVAL '0 15:30:00' DAY TO SECOND (0), TIMESTAMP '2020-05-19 15:30:00 +00:00', '9660000', string_array2('drama', 'science fiction'), string_array2('hd'), '7');

INSERT INTO movies (id, title, genre, releasedate, showtime, nextshowing, duration, categories, features, tags) VALUES
('6a878e06-6563-4a0c-acd9-d28dcfb2e91a', 'Weekend', '3', DATE '1967-12-29', INTERVAL '0 22:30:00' DAY TO SECOND (0), TIMESTAMP '2020-05-19 22:30:00 +00:00', '6300000', string_array2('drama', 'comedy'), string_array2('hd'), '2');

INSERT INTO movies (id, title, genre, releasedate, showtime, nextshowing, duration, categories, features, tags) VALUES
('2a40415c-ea6a-413f-bbef-a80ae280c4ff', 'Daisies', '3', DATE '1966-12-30', INTERVAL '0 21:30:00' DAY TO SECOND (0), TIMESTAMP '2020-05-15 21:30:00 +00:00', '4560000', string_array2('drama', 'comedy'), string_array2('hd'), '6');

INSERT INTO movies (id, title, genre, releasedate, showtime, nextshowing, duration, categories, features, tags) VALUES
('2f6dcb0a-4122-4a21-a1c6-534744dd6b85', 'Le Pont du Nord', '1', DATE '1982-01-13', INTERVAL '0 20:45:00' DAY TO SECOND (0), TIMESTAMP '2020-05-11 20:45:00 +00:00', '7620000', string_array2('drama'), string_array2('hd'), '3');
