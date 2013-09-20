 EXPLAIN ANALYZE
 select (2) as "1_1",(t1915."2") as "1_2",(t1911."tsk") as "2" from (select (t1910."dpt") as "1_1_dpt",(t1910."emp") as "1_1_emp",(t1910."salary") as "1_1_salary",(row_number() over (order by t1910."emp")) as "2" from employees as t1910) as t1915,tasks as t1911 where (t1915."1_1_emp") = (t1911."emp");
