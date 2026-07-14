CREATE TABLE null_ordering (
    id INT PRIMARY KEY,
    v INT
);

INSERT INTO null_ordering (id, v) VALUES
(1, 10),
(2, NULL),
(3, 5),
(4, NULL);
