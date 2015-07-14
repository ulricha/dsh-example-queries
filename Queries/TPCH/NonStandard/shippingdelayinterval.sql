-- SQL implementation of the 'shippingDelayInterval' query (paper running example)

-- Sorting arbitrary arrays
-- http://stackoverflow.com/questions/2913368/sorting-array-elements
-- http://postgres.cz/wiki/PostgreSQL_SQL_Tricks_I#General_array_sort
CREATE OR REPLACE FUNCTION array_sort (ANYARRAY)
RETURNS ANYARRAY LANGUAGE SQL
AS $$
   SELECT ARRAY(SELECT unnest($1) ORDER BY 1)
$$;

-- Native SQL implementation based on arrays
explain analyze
select o.o_orderkey,
       array_sort(array_agg(ls.l_quantity)) as qs,
       avg(ls.l_shipdate - o.o_orderdate)
from orders o,
     lateral (select l.l_shipdate, l.l_quantity
              from lineitem l
              where l.l_orderkey = o.o_orderkey) ls
where ((o.o_orderdate < DATE '1993-08-01')
  and (o.o_orderdate >= DATE '1993-07-01'))
group by o.o_orderkey
order by o.o_orderkey;

--------------------------------------------------------------------------------
-- DSH: outer query

explain analyze
SELECT a0.o_orderkey AS k1, a0.o_orderkey AS o1,
       a0.o_orderkey AS i1,
       AVG((CAST((a0.o_orderdate - a1.l_shipdate) AS DOUBLE PRECISION))) AS i2
FROM orders AS a0,
     lineitem AS a1
WHERE (a0.o_orderkey = a1.l_orderkey)
AND   ((a0.o_orderdate < DATE '1993-08-01') AND (a0.o_orderdate >= DATE '1993-07-01'))
GROUP BY a0.o_orderkey
ORDER BY o1 ASC;

--------------------------------------------------------------------------------
-- DSH: inner query

explain analyze
SELECT a2.o_orderkey AS r1, a3.l_shipdate AS o1,
       a2.o_orderkey AS o2, a3.l_orderkey AS o3, a3.l_linenumber AS o4,
       a3.l_quantity AS i1
FROM orders AS a2,
     lineitem AS a3
WHERE (a2.o_orderkey = a3.l_orderkey)
AND   ((a2.o_orderdate < DATE '1993-08-01') AND (a2.o_orderdate >= DATE '1993-07-01'))
ORDER BY r1 ASC, o1 ASC, o2 ASC, o3 ASC, o4 ASC;
