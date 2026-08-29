CREATE TABLE entities (
    id VARCHAR PRIMARY KEY,
    entity_type INTEGER NOT NULL,
    title VARCHAR,
    synopsis_short VARCHAR,
    synopsis_long VARCHAR,
    film_rating VARCHAR,
    film_label INTEGER,
    series_number_of_episodes INTEGER,
    series_label VARCHAR,
    image_url VARCHAR,
    hidden_image_url VARCHAR
);

CREATE TABLE episodes (
    id VARCHAR PRIMARY KEY,
    series_id VARCHAR NOT NULL,
    title VARCHAR,
    synopsis_short VARCHAR,
    synopsis_long VARCHAR
);
