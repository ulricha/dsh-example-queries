#!/bin/bash

# Load TPC-H data into PostgreSQL
# Usage: load.sh <PGDB> <BASEDIR>

PG_DB=$1
TPCHDIR=$2

function loadtable {
    LOADCMD="COPY ${1} FROM '${TPCHDIR}/dbgen/${1}.tbl' CSV DELIMITER '|'"
    echo ${LOADCMD}
    echo ${LOADCMD} | psql ${PG_DB}
}

echo ${PG_DB}
echo ${TPCHDIR}

dropdb ${PG_DB}
createdb ${PG_DB}
psql ${PG_DB} < ${TPCHDIR}/dbgen/dss.ddl
loadtable "customer"
loadtable "lineitem"
loadtable "nation"
loadtable "orders"
loadtable "partsupp"
loadtable "part"
loadtable "region"
loadtable "supplier"
