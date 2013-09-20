EXPLAIN ANALYZE
select (4) as "1_1",(t1829."2") as "1_2",(t1821."emp") as "2_emp",(3) as "2_tasks_1",(row_number() over (order by t1829."2",t1821."emp")) as "2_tasks_2" from (select (t1820."dpt") as "1_1_dpt",(row_number() over (order by t1820."dpt")) as "2" from departments as t1820) as t1829,employees as t1821 where (t1829."1_1_dpt") = (t1821."dpt");
