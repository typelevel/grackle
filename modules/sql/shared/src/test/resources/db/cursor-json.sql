CREATE TABLE brands (
    id Int PRIMARY KEY,
    categories Int
);

COPY brands (id, categories) FROM STDIN WITH DELIMITER '|';
1|8
2|16
\.
