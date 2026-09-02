CREATE TABLE recursive_interface_items (
    id VARCHAR(100) PRIMARY KEY,
    item_type INT NOT NULL
);

CREATE TABLE recursive_interface_next_items (
    id VARCHAR(100) PRIMARY KEY,
    next_item VARCHAR(100)
);
