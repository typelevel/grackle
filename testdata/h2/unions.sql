CREATE TABLE collections (
    id VARCHAR PRIMARY KEY,
    item_type VARCHAR NOT NULL,
    itema VARCHAR,
    itemb VARCHAR
);

INSERT INTO collections (id, item_type, itema, itemb) VALUES
('1', 'ItemA', 'A', NULL),
('2', 'ItemB', NULL, 'B');
