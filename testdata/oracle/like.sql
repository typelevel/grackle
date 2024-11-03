CREATE TABLE likes (
    id INTEGER PRIMARY KEY,
    notnullable VARCHAR(100) NOT NULL,
    nullable VARCHAR(100)
);

INSERT INTO likes (id, notnullable, nullable) VALUES
(1, 'foo', NULL),
(2, 'bar', 'baz');
