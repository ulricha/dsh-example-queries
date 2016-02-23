create index region_name_idx on region(r_name);
create index nation_name_idx on nation(n_name);

-- various property indexes
create index cust_seg_idx on customer(c_mktsegment);
create index part_size_idx on part(p_size);
create index part_type_idx on part(p_type);
create index part_name_idx on part(p_name);
create index order_status_idx on orders(o_orderstatus);
create index li_quant_idx on lineitem(l_quantity);
create index li_discount_idx on lineitem(l_discount);

-- date indexes
create index order_date_idx on orders(o_orderdate);
create index li_comdate_idx on lineitem(l_commitdate);
create index li_shipdate_idx on lineitem(l_shipdate);
create index li_receiptdate_idx on lineitem(l_receiptdate);

-- foreign key indexes
create index cust_nationkey_idx on customer(c_nationkey);
create index order_custkey_idx on orders(o_custkey);
create index li_suppkey_idx on lineitem(l_suppkey);
create index li_partsuppkey_idx on lineitem(l_partkey, l_suppkey);
create index supp_nationkey_idx on supplier(s_nationkey);
create index ps_partkey_idx on partsupp(ps_partkey);
create index ps_suppkey_idx on partsupp(ps_suppkey);
create index nation_regionkey_idx on nation(n_regionkey);

--cluster lineitem using lineitem_l_shipdate_idx;
--cluster orders using orders_o_orderdate_idx;
