-- SQL version using deep LATERAL joins
select c.c_name, array_agg(i.r)
from customer c,
     lateral (select sum(ls.l_extendedprice * (1 - ls.l_discount)) as r
              from orders o,
                   lateral (select l.l_extendedprice, l.l_discount
                            from lineitem l
                            where o.o_orderkey = l.l_orderkey) ls
              where o.o_custkey = c.c_custkey
              group by o.o_orderkey) i
where c.c_nationkey in (select n.n_nationkey
                        from nation n
                        where n.n_name = 'GERMANY')
  and c.c_custkey in (select o.o_custkey from orders o)
group by c.c_custkey, c.c_name
order by c.c_custkey;
