create index on part(p_size);
create index on part(p_type);

create index on region(r_name);

create index on nation(n_name);

create index on customer(c_mktsegment);

create index on orders(o_orderdate);

create index on lineitem(l_commitdate);
create index on lineitem(l_shipdate);
create index on lineitem(l_receiptdate);
