sqlplus -s test/test@//localhost/FREEPDB1 < <(cat /grackle-initdb.d/*.sql)
