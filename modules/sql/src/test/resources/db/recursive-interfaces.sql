CREATE TABLE recursive_interface_items (
    id TEXT PRIMARY KEY,
    item_type INTEGER NOT NULL
);

CREATE TABLE recursive_interface_next_items (
    id TEXT PRIMARY KEY,
    next_item TEXT
);

COPY recursive_interface_items (id, item_type) FROM STDIN WITH DELIMITER '|' NULL AS '';
1|1
2|2
\.

COPY recursive_interface_next_items (id, next_item) FROM STDIN WITH DELIMITER '|' NULL AS '';
1|2
2|1
\.
