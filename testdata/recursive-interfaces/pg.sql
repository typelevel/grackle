CREATE TABLE recursive_interface_items (
    id TEXT PRIMARY KEY,
    item_type INTEGER NOT NULL
);

CREATE TABLE recursive_interface_next_items (
    id TEXT PRIMARY KEY,
    next_item TEXT
);
