EXPLAIN ANALYZE
select (0) as "1_1",(t1974."2") as "1_2",(t1970."tsk") as "2_a",(2) as "2_b_1",(row_number() over (order by t1974."2",t1970."id")) as "2_b_2" from (select (1) as "2") as t1974,tasks as t1970;
