# runstats <database>

db2 connect to $1
for tab in customer  lineitem  nation  orders  partsupp  part  region  supplier; do
    echo $tab;
    db2 "runstats on table $tab";
    db2 "reorg table $tab";
done

