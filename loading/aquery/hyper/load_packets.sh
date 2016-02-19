PG_DB=$1
FILE=$2

LOADCMD="COPY packets FROM '${FILE}' CSV DELIMITER ','"
echo ${LOADCMD}
echo ${LOADCMD} | psql -h /tmp -p 7483 ${PG_DB}
