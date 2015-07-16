-- Native SQL + array_agg
-- Exploit the fact that there are no orders without lineitems.

-- explain analyze
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

--------------------------------------------------------------------------------
-- Native SQL + array_agg + LATERAL

select c.c_name, json_agg(json_build_array(os.o_orderdate, os.r))
from customer c,
     lateral (select o.o_orderdate,
                     sum(l.l_extendedprice * (1 - l.l_discount)) as r
                     from orders o, lineitem l
              where o.o_orderkey = l.l_orderkey
              and   o.o_orderstatus = 'P'
              and   o.o_custkey = c.c_custkey
              group by o.o_orderkey, o.o_custkey, o.o_orderdate) os
where c.c_nationkey in (select n.n_nationkey
                        from nation n
                        where n_name = 'GERMANY')
and c.c_custkey in (select o.o_custkey
                    from orders o
                    where o.o_orderstatus = 'P')
group by c.c_custkey, c.c_name
order by c.c_custkey;

--------------------------------------------------------------------------------
-- DSH: outer query

explain analyze
SELECT a4.c_custkey AS k1, a4.c_custkey AS o1, a4.c_name AS i1
FROM ( SELECT a0.c_custkey, a0.c_name, a0.c_nationkey
       FROM customer AS a0
       WHERE (a0.c_nationkey IN ( SELECT a3.n_nationkey
                                  FROM nation AS a3
                                  WHERE (a3.n_name = 'GERMANY')
             ))
             AND (a0.c_custkey IN ( SELECT a2.o_custkey
                                    FROM ( SELECT a1.o_custkey, a1.o_orderdate,
                                                  a1.o_orderkey,
                                                  a1.o_orderstatus
                                           FROM orders AS a1
                                           WHERE (a1.o_orderstatus = 'P')
                                         ) AS a2(o_custkey, o_orderdate, o_orderkey, o_orderstatus)
             ))
     ) AS a4(c_custkey, c_name, c_nationkey)
ORDER BY o1 ASC;

--------------------------------------------------------------------------------
-- DSH: inner query

explain analyze
WITH t0 AS (
        SELECT a1.o_custkey, a1.o_orderdate, a1.o_orderkey,
               a1.o_orderstatus
        FROM orders AS a1
        WHERE (a1.o_orderstatus = 'P')
    )
SELECT a10.c_custkey AS r1, a10.c_custkey AS o1, a10.o2,
       a10.i12 AS i1,
       COALESCE(SUM((a10.l_extendedprice * (1 - a10.l_discount))), 0) AS i2
FROM (( SELECT a5.c_custkey, a5.c_name, a5.c_nationkey,
               a6.o_orderkey AS o2, a6.o_orderkey AS k2, a6.o_custkey AS i11,
               a6.o_orderdate AS i12, a6.o_orderkey AS i13
        FROM ( SELECT a0.c_custkey, a0.c_name, a0.c_nationkey
               FROM customer AS a0
               WHERE (a0.c_nationkey IN ( SELECT a3.n_nationkey
                                          FROM nation AS a3
                                          WHERE (a3.n_name = 'GERMANY')
                     ))
                     AND (a0.c_custkey IN ( SELECT a2.o_custkey
                                            FROM t0 AS a2
                     ))
             ) AS a5(c_custkey, c_name, c_nationkey),
             t0 AS a6
        WHERE (a5.c_custkey = a6.o_custkey)
     ) AS a8
     LEFT OUTER JOIN
     ( SELECT a7.l_discount, a7.l_extendedprice, a7.l_orderkey
       FROM lineitem AS a7
     ) AS a9
     ON (a8.i13 = a9.l_orderkey)) AS a10(c_custkey, c_name, c_nationkey, o2, k2, i11, i12, i13, l_discount, l_extendedprice, l_orderkey)
GROUP BY a10.o2, a10.i12, a10.i13, a10.k2, a10.c_custkey
ORDER BY r1 ASC, o1 ASC, o2 ASC;
