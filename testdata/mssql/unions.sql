CREATE TABLE collections (
    id VARCHAR(100) PRIMARY KEY,
    item_type VARCHAR(100) NOT NULL,
    itema VARCHAR(100),
    itemb VARCHAR(100)
);

INSERT INTO collections (id, item_type, itema, itemb) VALUES
('1', 'ItemA', 'A', NULL),
('2', 'ItemB', NULL, 'B');

GO
