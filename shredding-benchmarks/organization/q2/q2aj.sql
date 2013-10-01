EXPLAIN ANALYZE
WITH
-- binding due to set operation
t0000 (pos12_str) AS
  ((SELECT a0003.emp AS pos12_str
      FROM employees AS a0003)
   EXCEPT ALL
   (SELECT a0004.emp AS pos12_str
      FROM tasks AS a0004,
           employees AS a0005
     WHERE a0004.emp = a0005.emp
       AND NOT ('abstract' = a0004.tsk))),

-- binding due to set operation
t0001 (pos18_str) AS
  ((SELECT a0000.dpt AS pos18_str
      FROM departments AS a0000)
   EXCEPT ALL
   (SELECT a0001.dpt AS pos18_str
      FROM employees AS a0001,
           departments AS a0002,
           t0000 AS a0006
     WHERE a0001.dpt = a0002.dpt
       AND a0001.emp = a0006.pos12_str))

SELECT 1 AS iter19_nat, a0007.pos18_str
   FROM t0001 AS a0007
  ORDER BY a0007.pos18_str ASC;
