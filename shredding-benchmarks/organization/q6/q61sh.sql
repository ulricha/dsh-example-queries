 EXPLAIN ANALYZE
 select (0) as "1_1",(t2010."2") as "1_2",(t2005."dpt") as "2_department",(5) as "2_people_1",(row_number() over (order by t2010."2",t2005."dpt")) as "2_people_2" from (select (1) as "2") as t2010,departments as t2005;
