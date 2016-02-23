#!/bin/bash

# Load TPC-H data into PostgreSQL
# Usage: load.sh <PGDB> <BASEDIR>

PG_DB=$1
TPCHDIR=$2

function loadtable {
    LOADCMD="COPY ${1} FROM '${TPCHDIR}/dbgen/${1}.tbl' CSV DELIMITER '|'"
    echo ${LOADCMD}
    echo ${LOADCMD} | psql -h /tmp -p 7483 ${PG_DB}
}

echo ${PG_DB}
echo ${TPCHDIR}

psql -h /tmp -p 7483 ${PG_DB} < ./schema.sql
loadtable "customer"
loadtable "lineitem"
loadtable "nation"
loadtable "orders"
loadtable "partsupp"
loadtable "part"
loadtable "region"
loadtable "supplier"
