 EXPLAIN ANALYZE
 select (2) as "1_1",(t1976."2") as "1_2",(t1971."emp") as "2_b",(t1972."dpt") as "2_c" from (select (t1970."emp") as "1_1_emp",(t1970."id") as "1_1_id",(t1970."tsk") as "1_1_tsk",(row_number() over (order by t1970."id")) as "2" from tasks as t1970) as t1976,employees as t1971,departments as t1972 where ((t1971."emp") = (t1976."1_1_emp")) and ((t1971."dpt") = (t1972."dpt"));
