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

COPY films (title, synopsis_short, synopsis_long) FROM STDIN WITH DELIMITER '|';
Film 1|Short film 1|Long film 1
Film 2|Short film 2|Long film 2
Film 3|Short film 3|Long film 3
\.


COPY series (title, synopsis_short, synopsis_long) FROM STDIN WITH DELIMITER '|';
Series 1|Short series 1|Long series 1
Series 2|Short series 2|Long series 2
Series 3|Short series 3|Long series 3
\.
