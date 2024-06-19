CREATE TABLE entities (
    id TEXT PRIMARY KEY,
    entity_type INTEGER NOT NULL,
    title TEXT,
    synopsis_short TEXT,
    synopsis_long TEXT,
    film_rating TEXT,
    film_label INTEGER,
    series_number_of_episodes INTEGER,
    series_label TEXT,
    image_url TEXT,
    hidden_image_url TEXT
);

CREATE TABLE episodes (
    id TEXT PRIMARY KEY,
    series_id TEXT NOT NULL,
    title TEXT,
    synopsis_short TEXT,
    synopsis_long TEXT
);

COPY entities (id, entity_type, title, synopsis_short, synopsis_long, film_rating, film_label, series_number_of_episodes, series_label, image_url, hidden_image_url) FROM STDIN WITH DELIMITER '|' NULL AS '';
1|1|Film 1|Short film 1|Long film 1|PG|1|||http://www.example.com/film1.jpg|
2|1|Film 2|Short film 2|Long film 2|U|2|||http://www.example.com/film2.jpg|
3|1|Film 3|Short film 3|Long film 3|15|3|||http://www.example.com/film3.jpg|
4|2|Series 1|Short series 1|Long series 1|||5|One||hidden_series1.jpg
5|2|Series 2|Short series 2|Long series 2|||6|Two||hidden_series2.jpg
6|2|Series 3|Short series 3|Long series 3|||7|Three||hidden_series3.jpg
\.

COPY episodes (id, series_id, title, synopsis_short, synopsis_long) FROM STDIN WITH DELIMITER '|' NULL AS '';
1|4|S1E1|Short S1E1|Long S1E1
2|4|S1E2|Short S1E2|Long S1E2
3|5|S2E1|Short S2E1|Long S2E1
4|5|S2E2|Short S2E2|Long S2E2
5|6|S3E1|Short S3E1|Long S3E1
6|6|S3E2|Short S3E2|Long S3E2
\.
