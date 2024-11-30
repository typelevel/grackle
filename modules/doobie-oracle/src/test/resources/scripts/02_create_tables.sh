#!/bin/bash

if [ ! -f /tmp/healthy ]; then
  echo "Intializing the database ..."
  sqlplus -s test/test@//localhost/FREEPDB1 < <(cat /grackle-initdb.d/*.sql)

  /bin/touch /tmp/healthy

  echo "Database initialized"
fi
