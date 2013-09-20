EXPLAIN ANALYZE
select (0) as "1_1",(t1943."2") as "1_2",(t1940."dpt") as "2_dpt",(2) as "2_emps_1",(row_number() over (order by t1943."2",t1940."dpt")) as "2_emps_2" from (select (1) as "2") as t1943,departments as t1940;
