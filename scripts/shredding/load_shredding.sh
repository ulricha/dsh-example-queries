#!/bin/bash

# Usage: load_shredding.sh <PGDB> <DIR> <DPTS>

PG_DB=$1
DIR=$2
DPTS=$3

function loadtable {
    LOADCMD="COPY ${1} FROM '${DIR}/${1}${DPTS}.csv' CSV DELIMITER ','"
    echo ${LOADCMD}
    echo ${LOADCMD} | psql ${PG_DB}
}

loadtable "departments"
loadtable "employees"
loadtable "tasks"
loadtable "contacts"

echo "create index on employees(salary)" | psql ${PG_DB}
echo "create index on employees(dpt)" | psql ${PG_DB}
echo "create index on tasks(emp)" | psql ${PG_DB}
echo "create index on contacts(dpt)" | psql ${PG_DB}
