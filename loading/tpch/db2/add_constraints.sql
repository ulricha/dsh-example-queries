alter table part add primary key (p_partkey);
alter table supplier add primary key (s_suppkey);
alter table partsupp add primary key (ps_partkey, ps_suppkey);
alter table customer add primary key (c_custkey);
alter table orders add primary key (o_orderkey);
alter table lineitem add primary key (l_orderkey, l_linenumber);
alter table nation add primary key (n_nationkey);
alter table region add primary key (r_regionkey);

alter table partsupp add foreign key (ps_partkey) references part(p_partkey);
alter table partsupp add foreign key (ps_suppkey) references supplier(s_suppkey);
alter table customer add foreign key (c_nationkey) references nation(n_nationkey);
alter table orders add foreign key (o_custkey) references customer(c_custkey);
alter table lineitem add foreign key (l_orderkey) references orders(o_orderkey);
alter table lineitem add foreign key (l_partkey) references part(p_partkey);
alter table lineitem add foreign key (l_suppkey) references supplier(s_suppkey);
alter table lineitem add foreign key (l_partkey, l_suppkey) references partsupp(ps_partkey, ps_suppkey);
alter table nation add foreign key (n_regionkey) references region(r_regionkey);
