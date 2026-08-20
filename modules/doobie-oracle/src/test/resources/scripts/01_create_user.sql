ALTER SESSION SET CONTAINER=FREEPDB1;

CREATE USER TEST IDENTIFIED BY test QUOTA UNLIMITED ON USERS;

GRANT CONNECT, RESOURCE TO TEST;

-- A schema is a user in Oracle, so a schema-qualified fixture needs a second user. The fixtures
-- all run as TEST, so TEST needs ANY rights to populate it. CREATE ANY INDEX is required as well
-- as CREATE ANY TABLE, because a PRIMARY KEY creates an index in the other schema.
CREATE USER QUALIFIED IDENTIFIED BY test QUOTA UNLIMITED ON USERS;

GRANT CREATE ANY TABLE, CREATE ANY INDEX, INSERT ANY TABLE, SELECT ANY TABLE TO TEST;
