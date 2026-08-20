CREATE TABLE likes (
    id INTEGER PRIMARY KEY,
    notnullable VARCHAR NOT NULL,
    nullable VARCHAR
);

INSERT INTO likes (id, notnullable, nullable) VALUES
(1, 'foo', NULL),
(2, 'bar', 'baz');
