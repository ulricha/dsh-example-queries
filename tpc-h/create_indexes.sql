alter table part add primary key (p_partkey);
alter table supplier add primary key (s_suppkey);
alter table partsupp add primary key (ps_partkey, ps_suppkey);
alter table customer add primary key (c_custkey);
alter table orders add primary key (o_orderkey);
alter table lineitem add primary key (l_orderkey, l_linenumber);
alter table nation add primary key (n_nationkey);
alter table region add primary key (r_regionkey);
