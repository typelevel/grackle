#!/bin/bash
set -e

# The mysql CLI - used both by docker-entrypoint-initdb.d to load the fixtures under
# testdata/mysql/, and by any ad-hoc `docker compose exec ... mysql` - picks its client-side
# charset from its own compiled-in default (historically latin1) unless a config file says
# otherwise; the server's --character-set-server setting has no effect on this. MySQL 8.3+
# also removed --skip-character-set-client-handshake, the old server-side override.
#
# A bind-mounted conf.d file won't do it: mysql refuses to read "world-writable" option
# files, and every file bind-mounted from the host in this dev environment shows up 0777
# (WSL/DrvFS forces this regardless of the file's real permissions), so mysql would silently
# ignore it. Writing the file fresh inside the container's own filesystem, with a mode mysql
# will trust, sidesteps that entirely.
install -d -m 0755 /etc/mysql/conf.d
cat > /etc/mysql/conf.d/charset.cnf <<'EOF'
[client]
default-character-set=utf8mb4
EOF
chmod 0644 /etc/mysql/conf.d/charset.cnf

exec /usr/local/bin/docker-entrypoint.sh "$@"
