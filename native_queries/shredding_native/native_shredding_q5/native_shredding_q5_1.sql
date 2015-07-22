select (0) as "1_1",(t1845."2") as "1_2",(t1841."tsk") as "2_a",(2) as "2_b_1",(row_number() over (order by t1845."2",t1841."id")) as "2_b_2" from (select (1) as "2") as t1845,tasks as t1841;
