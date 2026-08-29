CREATE TABLE records (
    id INTEGER PRIMARY KEY,
    record TEXT CHECK (json_valid(record) = 1)
);
