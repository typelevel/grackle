CREATE TABLE entities (
    id VARCHAR(100) PRIMARY KEY,
    entity_type INTEGER NOT NULL,
    title VARCHAR(100),
    synopsis_short VARCHAR(100),
    synopsis_long VARCHAR(100),
    film_rating VARCHAR(100),
    film_label INTEGER,
    series_number_of_episodes INTEGER,
    series_label VARCHAR(100),
    image_url VARCHAR(100),
    hidden_image_url VARCHAR(100)
);

CREATE TABLE episodes (
    id VARCHAR(100) PRIMARY KEY,
    series_id VARCHAR(100) NOT NULL,
    title VARCHAR(100),
    synopsis_short VARCHAR(100),
    synopsis_long VARCHAR(100)
);
