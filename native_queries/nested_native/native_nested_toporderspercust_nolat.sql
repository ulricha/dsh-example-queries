-- SQL query without LATERAL
select c.c_name, json_agg(ii.o_orderdate)
from customer c,
     (select i.o_custkey, i.o_orderdate
      from (select ics.o_custkey, ics.o_orderdate, row_number() over (partition by ics.o_custkey order by ics.ic) as x
            from (select o.o_custkey, o.o_orderdate, count(*) as ic
                  from orders o,
                       lineitem l
                  where o.o_orderkey = l.l_orderkey
                  group by o.o_orderkey, o.o_custkey, o.o_orderdate) ics) i
      where i.x <= 10) ii
where c.c_nationkey in (select n.n_nationkey
                        from nation n
                        where n.n_name = 'GERMANY')
  and c.c_custkey = ii.o_custkey
group by c.c_custkey, c.c_name
order by c.c_custkey;
