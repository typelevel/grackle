CREATE TABLE seq_scan_a
(
    id VARCHAR(100) PRIMARY KEY
);

CREATE TABLE seq_scan_b
(
    id   VARCHAR(100) PRIMARY KEY,
    a_id VARCHAR(100) NOT NULL
);

CREATE INDEX seq_scan_b_a_id_idx ON seq_scan_b(a_id);

CREATE TABLE seq_scan_c
(
    id     VARCHAR(100) PRIMARY KEY,
    b_id   VARCHAR(100) NOT NULL,
    name_c VARCHAR(100) NOT NULL
);

CREATE INDEX seq_scan_c_b_id_idx ON seq_scan_c(b_id);

CREATE TABLE seq_scan_d
(
    id     VARCHAR(100) PRIMARY KEY,
    b_id   VARCHAR(100) NOT NULL,
    name_d VARCHAR(100) NOT NULL
);

CREATE INDEX seq_scan_d_b_id_idx ON seq_scan_d(b_id);
