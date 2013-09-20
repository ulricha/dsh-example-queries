EXPLAIN ANALYZE
WITH
-- binding due to rownum operator
t0000 (item4_str, item1_str, item2_str, item3_int, iter24_nat) AS
  (SELECT a0001.dpt AS item4_str, a0002.dpt AS item1_str,
          a0002.emp AS item2_str, a0002.salary AS item3_int,
          ROW_NUMBER () OVER (ORDER BY a0001.dpt ASC, a0002.emp ASC) AS
          iter24_nat
     FROM departments AS a0001,
          employees AS a0002),

-- binding due to set operation
t0001 (iter18_nat) AS
  ((SELECT a0003.iter24_nat AS iter18_nat
      FROM t0000 AS a0003
     WHERE a0003.item4_str = a0003.item1_str)
   EXCEPT ALL
   (SELECT a0005.iter24_nat AS iter18_nat
      FROM tasks AS a0004,
           t0000 AS a0005
     WHERE a0004.tsk = 'abstract'
       AND a0005.item4_str = a0005.item1_str
       AND a0004.emp = a0005.item2_str)),

-- binding due to set operation
t0002 (iter21_str) AS
  ((SELECT a0000.dpt AS iter21_str
      FROM departments AS a0000)
   EXCEPT ALL
   (SELECT a0007.item4_str AS iter21_str
      FROM t0001 AS a0006,
           t0000 AS a0007
     WHERE a0006.iter18_nat = a0007.iter24_nat))

SELECT 1 AS iter22_nat, a0008.iter21_str
   FROM t0002 AS a0008
  ORDER BY a0008.iter21_str ASC;
