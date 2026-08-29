CREATE TABLE recursive_interface_items (
    id VARCHAR PRIMARY KEY,
    item_type INTEGER NOT NULL
);

CREATE TABLE recursive_interface_next_items (
    id VARCHAR PRIMARY KEY,
    next_item VARCHAR
);
