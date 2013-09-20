EXPLAIN ANALYZE
(WITH q1924 AS
   (SELECT (t1914."dpt") AS "1_1_dpt",
           (t1915."dpt") AS "1_2_dpt",
           (t1915."emp") AS "1_2_emp",
           (t1915."salary") AS "1_2_salary",
           (row_number() over (
                               ORDER BY t1914."dpt",t1915."dpt",t1915."emp",t1915."salary")) AS "2"
    FROM departments AS t1914,
         employees AS t1915
    WHERE ((t1914."dpt") = (t1915."dpt"))
      AND (((t1915."salary") > (1000000))
           OR ((t1915."salary") < (1000))))
 SELECT (2) AS "1_1",
        (t1925."2") AS "1_2",
        (t1916."tsk") AS "2"
 FROM q1924 AS t1925,
      tasks AS t1916
 WHERE (t1925."1_2_emp") = (t1916."emp"))
UNION ALL (WITH q1926 AS
             (SELECT (t1914."dpt") AS "1_1_dpt",
                     (t1917."client") AS "1_2_client",
                     (t1917."dpt") AS "1_2_dpt",
                     (t1917."id") AS "1_2_id",
                     (t1917."name") AS "1_2_name",
                     (row_number() over (
                                         ORDER BY t1914."dpt",t1917."client",t1917."dpt",t1917."id",t1917."name")) AS "2"
              FROM departments AS t1914,
                   contacts AS t1917
              WHERE ((t1914."dpt") = (t1917."dpt"))
                AND (t1917."client"))
           SELECT (4) AS "1_1",
                  (t1927."2") AS "1_2",
                  ('buy') AS "2"
           FROM q1926 AS t1927)
