-- schema `qualified` is created by qualified-names.sql, which loads first
CREATE TABLE qualified.union_order_entities (
    id text NOT NULL PRIMARY KEY,
    entity_type text NOT NULL,
    name text NOT NULL
);

INSERT INTO qualified.union_order_entities (id, entity_type, name) VALUES
('1', 'ItemA', 'Charlie'),
('2', 'ItemB', 'Alpha'),
('3', 'ItemA', 'Bravo'),
('4', 'ItemB', 'Delta');
