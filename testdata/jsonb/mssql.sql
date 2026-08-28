CREATE TABLE records (
    id Integer PRIMARY KEY,
    record NVARCHAR(500) CHECK (ISJSON(record) = 1)
);

GO
