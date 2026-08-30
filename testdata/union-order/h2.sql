-- schema `qualified` is created by qualified-names.sql, which loads first
CREATE TABLE qualified.union_order_entities (
    id VARCHAR NOT NULL PRIMARY KEY,
    entity_type VARCHAR NOT NULL,
    name VARCHAR NOT NULL
);
