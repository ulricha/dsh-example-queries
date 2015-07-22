select (0) as "1_1",(t1832."2") as "1_2",(2) as "2_b_1",(row_number() over (order by t1832."2",t1829."id")) as "2_b_2",(t1829."name") as "2_emp" from (select (1) as "2") as t1832,employees as t1829;
