CREATE TABLE movies (
    id UUID PRIMARY KEY,
    title TEXT NOT NULL,
    genre INTEGER NOT NULL,
    releasedate DATE NOT NULL,
    showtime TIME NOT NULL,
    nextshowing TIMESTAMP WITH TIME ZONE,
    duration BIGINT NOT NULL,
    categories VARCHAR[] NOT NULL,
    features VARCHAR[] NOT NULL
);

COPY movies (id, title, genre, releasedate, showtime, nextshowing, duration, categories, features) FROM STDIN WITH DELIMITER '|';
6a7837fc-b463-4d32-b628-0f4b3065cb21|Celine et Julie Vont en Bateau|1|1974-10-07|19:35:00|2020-05-22T19:35:00Z|12300000|{"drama","comedy"}|{"hd","hls"}
11daf8c0-11c3-4453-bfe1-cb6e6e2f9115|Duelle|1|1975-09-15|19:20:00|2020-05-27T19:20:00Z|7260000|{"drama"}|{"hd"}
aea9756f-621b-42d5-b130-71f3916c4ba3|L'Amour fou|1|1969-01-15|21:00:00|2020-05-27T21:00:00Z|15120000|{"drama"}|{"hd"}
2ddb041f-86c2-4bd3-848c-990a3862634e|Last Year at Marienbad|1|1961-06-25|20:30:00|2020-05-26T20:30:00Z|5640000|{"drama"}|{"hd"}
8ae5b13b-044c-4ff0-8b71-ccdb7d77cd88|Zazie dans le Métro|3|1960-10-28|20:15:00|2020-05-25T20:15:00Z|5340000|{"drama","comedy"}|{"hd"}
9dce9deb-9188-4cc2-9685-9842b8abdd34|Alphaville|2|1965-05-05|19:45:00|2020-05-19T19:45:00Z|5940000|{"drama","science fiction"}|{"hd"}
1bf00ac6-91ab-4e51-b686-3fd5e2324077|Stalker|1|1979-05-13|15:30:00|2020-05-19T15:30:00Z|9660000|{"drama","science fiction"}|{"hd"}
6a878e06-6563-4a0c-acd9-d28dcfb2e91a|Weekend|3|1967-12-29|22:30:00|2020-05-19T22:30:00Z|6300000|{"drama","comedy"}|{"hd"}
2a40415c-ea6a-413f-bbef-a80ae280c4ff|Daisies|3|1966-12-30|21:30:00|2020-05-15T21:30:00Z|4560000|{"drama","comedy"}|{"hd"}
2f6dcb0a-4122-4a21-a1c6-534744dd6b85|Le Pont du Nord|1|1982-01-13|20:45:00|2020-05-11T20:45:00Z|7620000|{"drama"}|{"hd"}
\.
