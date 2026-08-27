-- schema `qualified` is created by qualified-names.sql, which loads first
CREATE TABLE qualified.union_order_entities (
    id text NOT NULL PRIMARY KEY,
    entity_type text NOT NULL,
    name text NOT NULL
);
