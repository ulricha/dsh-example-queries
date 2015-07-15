-- SQL versions of query 'custRevenues'

-- SQL version using deep LATERAL joins
explain analyze
select c.c_name, json_agg(i.r)
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

--------------------------------------------------------------------------------

-- SQL version using a flat LATERAL join
explain analyze
select c.c_name, json_agg(i.r)
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
SELECT a9.c_custkey AS r1, a9.c_custkey AS o1,
       a9.o_orderkey AS o2,
       COALESCE(SUM((a9.l_extendedprice * (1 - a9.l_discount))), 0) AS i1
FROM (( SELECT a4.c_custkey, a4.c_name, a4.c_nationkey,
               a5.o_custkey, a5.o_orderkey
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
     ( SELECT a6.l_discount, a6.l_extendedprice, a6.l_orderkey
       FROM lineitem AS a6
     ) AS a8
     ON (a7.o_orderkey = a8.l_orderkey)) AS a9(c_custkey, c_name, c_nationkey, o_custkey, o_orderkey, l_discount, l_extendedprice, l_orderkey)
GROUP BY a9.c_custkey, a9.o_orderkey
ORDER BY r1 ASC, o1 ASC, o2 ASC;
