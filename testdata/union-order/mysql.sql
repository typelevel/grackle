-- schema `qualified` is created by qualified-names.sql, which loads first
CREATE TABLE qualified.union_order_entities (
    id VARCHAR(100) NOT NULL PRIMARY KEY,
    entity_type VARCHAR(100) NOT NULL,
    name VARCHAR(100) NOT NULL
);
