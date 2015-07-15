-- SQL versions of query 'regionsTopCustomers'

-- Basic SQL version (don't consider customers without orders)
explain analyze
select n.n_name, json_agg(ii.c_name)
from region r,
     nation n,
     lateral (select i.c_name
              from (select ocs.c_name, row_number() over (partition by ocs.n
                                                          order by ocs.oc) as rn
                    from (select c.c_name, n.n_nationkey as n, count(*) as oc
                          from customer c,
                               orders o
                          where c.c_nationkey = n.n_nationkey
                            and c.c_custkey = o.o_custkey
                          group by c.c_custkey, c.c_name) ocs) i
              where i.rn <= 10) ii
where r.r_name = 'EUROPE'
  and r.r_regionkey = n.n_regionkey
group by n.n_nationkey
order by n.n_nationkey;

--------------------------------------------------------------------------------

-- SQL version: include customers without orders, using UNION ALL
explain analyze
select n.n_name, json_agg(ii.c_name)
from region r,
     nation n,
     lateral (select i.c_name
              from (select ocs.c_name, row_number() over (partition by ocs.n
                                                          order by ocs.oc) as rn
                    from ((select c.c_name, n.n_nationkey as n, count(*) as oc
                          from customer c,
                               orders o
                          where c.c_nationkey = n.n_nationkey
                            and c.c_custkey = o.o_custkey
                          group by c.c_custkey, c.c_name)
                          union all
                          (select c.c_name, n.n_nationkey as n, 0 as oc
                           from customer c
                           where c.c_nationkey = n.n_nationkey
                             and c.c_custkey not in (select o.o_custkey from orders o))) ocs) i
              where i.rn <= 10) ii
where r.r_name = 'EUROPE'
  and r.r_regionkey = n.n_regionkey
group by n.n_nationkey
order by n.n_nationkey;

--------------------------------------------------------------------------------

-- SQL version: include customers without orders, using LEFT OUTER JOIN
explain analyze
select n.n_name, json_agg(ii.c_name)
from region r,
     nation n,
     lateral (select i.c_name
              from (select ocs.c_name, row_number() over (partition by ocs.n
                                                          order by ocs.oc) as rn
                    from ((select c.c_name, n.n_nationkey as n, count(o.o_orderkey) as oc
                          from customer c
                               left outer join
                               orders o
                               on c.c_custkey = o.o_custkey
                          where c.c_nationkey = n.n_nationkey
                          group by c.c_custkey, c.c_name)) ocs) i
              where i.rn <= 10) ii
where r.r_name = 'EUROPE'
  and r.r_regionkey = n.n_regionkey
group by n.n_nationkey
order by n.n_nationkey;

--------------------------------------------------------------------------------

-- DSH: outer query
explain analyze
SELECT a0.r_regionkey AS k1, a1.n_nationkey AS k2,
       a0.r_regionkey AS o1, a1.n_nationkey AS o2, a1.n_name AS i1
FROM region AS a0,
     nation AS a1
WHERE (a0.r_regionkey = a1.n_regionkey)
  AND (a0.r_name = 'EUROPE')
ORDER BY o1 ASC, o2 ASC;

-- DSH: inner query
explain analyze
SELECT a9.r_regionkey AS r1, a9.n_nationkey AS r2, a9.i16 AS o1,
       a9.r_regionkey AS o2, a9.n_nationkey AS o3, a9.c_custkey AS o4,
       a9.c_name AS i1
FROM ( SELECT ROW_NUMBER() OVER (PARTITION BY a8.r_regionkey,
              a8.n_nationkey
              ORDER BY COUNT(a8.o_orderkey) ASC, a8.r_regionkey ASC, a8.n_nationkey ASC, a8.c_custkey ASC
              ) AS i9, a8.r_regionkey, a8.n_nationkey, a8.c_custkey,
              a8.c_name, COUNT(a8.o_orderkey) AS i16
       FROM (( SELECT a2.r_name, a2.r_regionkey, a3.n_name,
                      a3.n_nationkey, a3.n_regionkey, a4.c_custkey, a4.c_name,
                      a4.c_nationkey
               FROM region AS a2,
                    nation AS a3,
                    customer AS a4
               WHERE (a3.n_nationkey = a4.c_nationkey)
                     AND (a2.r_regionkey = a3.n_regionkey)
                     AND (a2.r_name = 'EUROPE')
            ) AS a6
            LEFT OUTER JOIN
            ( SELECT a5.o_custkey, a5.o_orderkey
              FROM orders AS a5
            ) AS a7
            ON (a6.c_custkey = a7.o_custkey)) AS a8(r_name, r_regionkey, n_name, n_nationkey, n_regionkey, c_custkey, c_name, c_nationkey, o_custkey, o_orderkey)
       GROUP BY a8.r_regionkey, a8.n_nationkey, a8.c_custkey, a8.c_name
     ) AS a9(i9, r_regionkey, n_nationkey, c_custkey, c_name, i16)
WHERE (a9.i9 <= 10)
ORDER BY r1 ASC, r2 ASC, o1 ASC, o2 ASC, o3 ASC, o4 ASC;
