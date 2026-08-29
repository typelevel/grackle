CREATE TABLE seq_scan_a
(
    id VARCHAR PRIMARY KEY
);

CREATE TABLE seq_scan_b
(
    id   VARCHAR PRIMARY KEY,
    a_id VARCHAR NOT NULL
);

CREATE INDEX seq_scan_b_a_id_idx ON seq_scan_b(a_id);

CREATE TABLE seq_scan_c
(
    id     VARCHAR PRIMARY KEY,
    b_id   VARCHAR NOT NULL,
    name_c VARCHAR NOT NULL
);

CREATE INDEX seq_scan_c_b_id_idx ON seq_scan_c(b_id);

CREATE TABLE seq_scan_d
(
    id     VARCHAR PRIMARY KEY,
    b_id   VARCHAR NOT NULL,
    name_d VARCHAR NOT NULL
);

CREATE INDEX seq_scan_d_b_id_idx ON seq_scan_d(b_id);
