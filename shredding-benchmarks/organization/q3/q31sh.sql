EXPLAIN ANALYZE
select (0) as "1_1",(t1913."2") as "1_2",(2) as "2_b_1",(row_number() over (order by t1913."2",t1910."emp")) as "2_b_2",(t1910."emp") as "2_emp" from (select (1) as "2") as t1913,employees as t1910;
