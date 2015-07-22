-- SQL versions of query "topOrdersPerCust'"
-- needs indexes on l_orderkey and o_custkey to terminate in reasonable time.
select c.c_name, json_agg(ii.o_orderdate)
from customer c,
     lateral (select i.o_orderdate
              from (select ics.o_orderdate, row_number() over (partition by ics.c_custkey order by ics.ic) as x
                    from (select o.o_orderdate, c.c_custkey, count(*) as ic
                          from orders o,
                               lineitem l
                          where o.o_orderkey = l.l_orderkey
                            and o.o_custkey = c.c_custkey
                          group by o.o_orderkey, o.o_orderdate) ics) i
              where i.x <= 10) ii
where c.c_nationkey in (select n.n_nationkey
                        from nation n
                        where n.n_name = 'GERMANY')
group by c.c_custkey, c.c_name
order by c.c_custkey;
