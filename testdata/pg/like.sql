CREATE TABLE likes (
    id INTEGER PRIMARY KEY,
    notnullable TEXT NOT NULL,
    nullable TEXT
);

COPY likes (id, notnullable, nullable) FROM STDIN WITH DELIMITER '|';
1|foo|\N
2|bar|baz
\.
