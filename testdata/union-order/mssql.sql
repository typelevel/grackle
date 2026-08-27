CREATE SCHEMA qualified;
GO

CREATE TABLE qualified.union_order_entities (
    id VARCHAR(100) NOT NULL PRIMARY KEY,
    entity_type VARCHAR(100) NOT NULL,
    name VARCHAR(100) NOT NULL
);

INSERT INTO qualified.union_order_entities (id, entity_type, name) VALUES
('1', 'ItemA', 'Charlie'),
('2', 'ItemB', 'Alpha'),
('3', 'ItemA', 'Bravo'),
('4', 'ItemB', 'Delta');

GO
