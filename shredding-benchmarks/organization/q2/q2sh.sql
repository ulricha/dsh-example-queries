EXPLAIN ANALYZE
WITH q1848 AS
  (SELECT (1) AS "2")
SELECT (0) AS "1_1",
       (t1849."2") AS "1_2",
       (t1844."dpt") AS "2_dpt"
FROM q1848 AS t1849,
     departments AS t1844
WHERE NOT (EXISTS
             (SELECT 0 AS dummy
              FROM employees AS t1845
              WHERE ((t1844."dpt") = (t1845."dpt"))
                AND (NOT (EXISTS
                            (SELECT 0 AS dummy
                             FROM tasks AS t1846
                             WHERE ((t1845."emp") = (t1846."emp"))
                               AND ((t1846."tsk") = ('abstract')))))));
