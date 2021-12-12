#!/bin/sh

if [ -f tools.db ]; then
    echo "tools.db exists"
    exit 1
else
    sqlite3 tools.db < build_db.sql
fi
