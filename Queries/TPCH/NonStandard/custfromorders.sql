-- SQL version of query 'custFromOrders'

explain analyze
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

--------------------------------------------------------------------------------

-- DSH: outer query
explain analyze
SELECT a3.c_custkey AS k1, a3.c_custkey AS o1, a3.c_name AS i1
FROM ( SELECT a0.c_custkey, a0.c_name, a0.c_nationkey
       FROM customer AS a0
       WHERE (a0.c_nationkey IN ( SELECT a2.n_nationkey
                                  FROM nation AS a2
                                  WHERE (a2.n_name = 'GERMANY')
             ))
             AND (a0.c_custkey IN ( SELECT a1.o_custkey
                                    FROM orders AS a1
             ))
     ) AS a3(c_custkey, c_name, c_nationkey)
ORDER BY o1 ASC;

-- DSH: inner query
explain analyze
SELECT a4.c_custkey AS r1, a4.c_custkey AS o1,
       a5.o_orderkey AS o2, a5.o_orderpriority AS i1,
       a5.o_totalprice AS i2
FROM ( SELECT a0.c_custkey, a0.c_name, a0.c_nationkey
       FROM customer AS a0
       WHERE (a0.c_nationkey IN ( SELECT a2.n_nationkey
                                  FROM nation AS a2
                                  WHERE (a2.n_name = 'GERMANY')
             ))
             AND (a0.c_custkey IN ( SELECT a1.o_custkey
                                    FROM orders AS a1
             ))
     ) AS a4(c_custkey, c_name, c_nationkey),
     orders AS a5
WHERE (a4.c_custkey = a5.o_custkey)
ORDER BY r1 ASC, o1 ASC, o2 ASC;
