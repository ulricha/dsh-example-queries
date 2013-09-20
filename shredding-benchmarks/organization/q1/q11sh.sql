EXPLAIN ANALYZE
select (0) as "1_1", (t1825."2") as "1_2",(4) as "2_contacts_1",
       (row_number() over (order by t1825."2",t1820."dpt")) as "2_contacts_2",
       (t1820."dpt") as "2_dpt",(4) as "2_employees_1",
       (row_number() over (order by t1825."2",t1820."dpt")) as "2_employees_2" 
from (select (1) as "2") as t1825,departments as t1820;
