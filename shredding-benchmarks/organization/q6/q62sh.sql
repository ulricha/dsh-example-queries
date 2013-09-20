EXPLAIN ANALYZE
(WITH q1920 AS
  (SELECT (t1914."dpt") AS "1_1_dpt",
          (row_number() over (
                              ORDER BY t1914."dpt")) AS "2"
   FROM departments AS t1914)
SELECT (5) AS "1_1",
       (t1921."2") AS "1_2",
       (t1915."emp") AS "2_name",
       (2) AS "2_tasks_1",
       (row_number() over (
                           ORDER BY t1921."2",t1915."dpt",t1915."emp",t1915."salary")) AS "2_tasks_2"
FROM q1920 AS t1921,
     employees AS t1915
WHERE ((t1921."1_1_dpt") = (t1915."dpt"))
  AND (((t1915."salary") > (1000000))
       OR ((t1915."salary") < (1000))))
UNION ALL (WITH q1922 AS
             (SELECT (t1914."dpt") AS "1_1_dpt",
                     (row_number() over (
                                         ORDER BY t1914."dpt")) AS "2"
              FROM departments AS t1914)
           SELECT (5) AS "1_1",
                  (t1923."2") AS "1_2",
                  (t1917."name") AS "2_name",
                  (4) AS "2_tasks_1",
                  (row_number() over (
                                      ORDER BY t1923."2",t1917."client",t1917."dpt",t1917."id",t1917."name")) AS "2_tasks_2"
           FROM q1922 AS t1923,
                contacts AS t1917
           WHERE ((t1923."1_1_dpt") = (t1917."dpt"))
             AND (t1917."client")) ;
