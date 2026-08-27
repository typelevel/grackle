CREATE TYPE string_array2 AS VARRAY(100) OF VARCHAR2(100);
/

CREATE TABLE movies (
    id VARCHAR(36) PRIMARY KEY,
    title VARCHAR(100) NOT NULL,
    genre INTEGER NOT NULL,
    releasedate DATE NOT NULL,
    showtime INTERVAL DAY (0) TO SECOND (0) NOT NULL,
    nextshowing TIMESTAMP WITH TIME ZONE NOT NULL,
    duration NUMBER(18) NOT NULL,
    categories string_array2 NOT NULL,
    features string_array2 NOT NULL,
    tags INTEGER NOT NULL
);
