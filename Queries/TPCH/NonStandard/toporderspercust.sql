-- SQL versions of query "topOrdersPerCust'"

-- SQL query without LATERAL
explain analyze
select c.c_name, array_agg(ii.o_orderdate)
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

--------------------------------------------------------------------------------

-- SQL query with LATERAL
explain analyze
select c.c_name, array_agg(ii.o_orderdate)
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
SELECT a10.c_custkey AS r1, a10.i18 AS o1, a10.c_custkey AS o2,
       a10.o2 AS o3, a10.i12 AS i1
FROM ( SELECT ROW_NUMBER() OVER (PARTITION BY a9.c_custkey
              ORDER BY COUNT(a9.l_orderkey) ASC, a9.c_custkey ASC, a9.o2 ASC
              ) AS i10, a9.o2, a9.i12, a9.c_custkey, a9.i13, a9.k2,
              COUNT(a9.l_orderkey) AS i18
       FROM (( SELECT a4.c_custkey, a4.c_name, a4.c_nationkey,
                      a5.o_orderkey AS o2, a5.o_orderkey AS k2,
                      a5.o_custkey AS i11, a5.o_orderdate AS i12,
                      a5.o_orderkey AS i13
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
            ) AS a7
            LEFT OUTER JOIN
            ( SELECT a6.l_orderkey
              FROM lineitem AS a6
            ) AS a8
            ON (a7.i13 = a8.l_orderkey)) AS a9(c_custkey, c_name, c_nationkey, o2, k2, i11, i12, i13, l_orderkey)
       GROUP BY a9.o2, a9.i12, a9.c_custkey, a9.i13, a9.k2
     ) AS a10(i10, o2, i12, c_custkey, i13, k2, i18)
WHERE (a10.i10 <= 10)
ORDER BY r1 ASC, o1 ASC, o2 ASC, o3 ASC;
