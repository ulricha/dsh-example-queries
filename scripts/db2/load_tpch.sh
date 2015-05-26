# load tpch data into db2
# load_tpch.sh <database> <datadir>

db2 connect to $1
for tab in customer  lineitem  nation  orders  partsupp  part  region  supplier; do
    echo $tab;
    db2 "load from $2/$tab.tbl of del modified by coldel| insert into $tab";
done

