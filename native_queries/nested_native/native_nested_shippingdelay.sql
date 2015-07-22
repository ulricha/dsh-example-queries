-- SQL implementation of the 'shippingDelay' query (paper running example)
-- Native SQL implementation based on arrays
select o.o_orderkey,
       array_sort(array_agg(ls.l_quantity)) as qs,
       avg(ls.l_shipdate - o.o_orderdate)
from orders o,
     lateral (select l.l_shipdate, l.l_quantity
              from lineitem l
              where l.l_orderkey = o.o_orderkey) ls
group by o.o_orderkey
order by o.o_orderkey;
