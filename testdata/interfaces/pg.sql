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
