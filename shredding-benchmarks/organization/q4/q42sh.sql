EXPLAIN ANALYZE
select (2) as "1_1",(t1945."2") as "1_2",(t1941."emp") as "2" from (select (t1940."dpt") as "1_1_dpt",(row_number() over (order by t1940."dpt")) as "2" from departments as t1940) as t1945,employees as t1941 where (t1945."1_1_dpt") = (t1941."dpt");
