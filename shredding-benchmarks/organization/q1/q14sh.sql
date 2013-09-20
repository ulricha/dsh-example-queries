EXPLAIN ANALYZE
select (4) as "1_1",(t1827."2") as "1_2",(t1823."client") as "2_client",(t1823."name") as "2_name" from (select (t1820."dpt") as "1_1_dpt",(row_number() over (order by t1820."dpt")) as "2" from departments as t1820) as t1827,contacts as t1823 where (t1827."1_1_dpt") = (t1823."dpt");
