-- SQL version of query 'custFromOrders'
select c.c_name, json_agg(json_build_array(i.o_orderpriority, i.o_totalprice))
from customer c,
     lateral (select o.o_orderpriority, o.o_totalprice
              from orders o
              where c.c_custkey = o.o_custkey) i
where c.c_nationkey in (select n.n_nationkey
                        from nation n
                        where n.n_name = 'GERMANY')
  and c.c_custkey in (select o.o_custkey from orders o)
group by c.c_custkey, c.c_name
order by c.c_custkey;
