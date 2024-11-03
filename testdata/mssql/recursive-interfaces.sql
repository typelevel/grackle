CREATE TABLE recursive_interface_items (
    id VARCHAR(100) PRIMARY KEY,
    item_type INTEGER NOT NULL
);

CREATE TABLE recursive_interface_next_items (
    id VARCHAR(100) PRIMARY KEY,
    next_item VARCHAR(100)
);

INSERT INTO recursive_interface_items (id, item_type) VALUES
('1', '1'),
('2', '2');

INSERT INTO recursive_interface_next_items (id, next_item) VALUES
('1', '2'),
('2', '1');

GO
