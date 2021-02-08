
CREATE TABLE collections (
    id TEXT PRIMARY KEY,
    item_type TEXT NOT NULL,
    itema TEXT,
    itemb TEXT
);

COPY collections (id, item_type, itema, itemb) FROM STDIN WITH DELIMITER '|' NULL AS '';
1|ItemA|A|null
2|ItemB|null|B
\.
