CREATE TABLE collections (
    id VARCHAR2(100) PRIMARY KEY,
    item_type VARCHAR2(100) NOT NULL,
    itema VARCHAR2(100),
    itemb VARCHAR2(100)
);

INSERT INTO collections (id, item_type, itema, itemb) VALUES
('1', 'ItemA', 'A', NULL),
('2', 'ItemB', NULL, 'B');
