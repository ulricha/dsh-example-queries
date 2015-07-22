-- Native SQL + array_agg
-- Exploit the fact that there are no orders without lineitems.
select c.c_name, json_agg(json_build_array(os.o_orderdate, os.r))
from customer c,
     (select o.o_custkey, o.o_orderdate,
             sum(l.l_extendedprice * (1 - l.l_discount)) as r
      from orders o, lineitem l
      where o.o_orderkey = l.l_orderkey
      and   o.o_orderstatus = 'P'
      group by o.o_orderkey, o.o_custkey, o.o_orderdate) os
where c.c_nationkey in (select n.n_nationkey
                        from nation n
                        where n_name = 'GERMANY')
and c.c_custkey in (select o.o_custkey
                    from orders o
                    where o.o_orderstatus = 'P')
and c.c_custkey = os.o_custkey
group by c.c_custkey, c.c_name
order by c.c_custkey;
