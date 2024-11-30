#!/bin/bash

# Start SQL Server
/opt/mssql/bin/sqlservr &

# Wait for SQL Server to start (max 90 seconds)
for i in {1..90}; do
  /opt/mssql-tools18/bin/sqlcmd -S localhost -U sa -P Test_123_Test -No -Q 'SELECT 1' &> /dev/null
  if [ $? -eq 0 ]; then
      echo "SQL Server is up"
      break
  fi
  echo "Waiting for SQL Server to start..."
  sleep 1
done

# Initialize the database
if [ ! -f /tmp/healthy ]; then
  echo "Intializing the database ..."
  /opt/mssql-tools18/bin/sqlcmd -S localhost -U sa -P Test_123_Test -No \
    -i <(cat /container-entrypoint-initdb.d/init.sql /grackle-initdb.d/*.sql)

  /bin/touch /tmp/healthy

  echo "Database initialized"
fi

# Keep the container running
tail -f /dev/null
