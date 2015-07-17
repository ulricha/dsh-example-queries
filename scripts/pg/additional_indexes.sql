create index on region(r_name);
create index on nation(n_name);

-- various property indexes
create index on customer(c_mktsegment);
create index on part(p_size);
create index on part(p_type);
create index on orders(o_orderstatus);

-- date indexes
create index on orders(o_orderdate);
create index on lineitem(l_commitdate);
create index on lineitem(l_shipdate);
create index on lineitem(l_receiptdate);

-- foreign key indexes
create index on customer(c_nationkey);
create index on orders(o_custkey);
create index on lineitem(l_orderkey);
create index on supplier(s_nationkey);
create index on partsupp(ps_partkey);
create index on partsupp(ps_suppkey);
create index on nation(n_regionkey);
