-- SQL version using a flat LATERAL join
select c.c_name, array_agg(i.r)
from customer c,
     lateral (select sum(l.l_extendedprice * (1 - l.l_discount)) as r
              from orders o,
                   lineitem l
              where o.o_custkey = c.c_custkey
                and o.o_orderkey = l.l_orderkey
              group by o.o_orderkey) i
where c.c_nationkey in (select n.n_nationkey
                        from nation n
                        where n.n_name = 'GERMANY')
  and c.c_custkey in (select o.o_custkey from orders o)
group by c.c_custkey, c.c_name
order by c.c_custkey;
