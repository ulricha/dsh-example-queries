-- SQL implementation of the 'shippingDelayInterval' query (paper running example)
-- Native SQL implementation based on arrays and LATERAL
select o.o_orderkey,
       array_to_json(array_sort(array_agg(ls.l_quantity))) as qs,
       avg(ls.l_shipdate - o.o_orderdate)
from orders o,
     lateral (select l.l_shipdate, l.l_quantity
              from lineitem l
              where l.l_orderkey = o.o_orderkey) ls
where ((o.o_orderdate < DATE '1993-10-01')
  and (o.o_orderdate >= DATE '1993-07-01'))
group by o.o_orderkey
order by o.o_orderkey;
