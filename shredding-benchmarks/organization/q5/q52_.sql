EXPLAIN ANALYZE
WITH
-- binding due to rank operator
t0000 (item8_str, item9_int, item10_str, iter11_nat) AS
  (SELECT a0000.emp AS item8_str, a0000.id AS item9_int,
          a0000.tsk AS item10_str,
          DENSE_RANK () OVER (ORDER BY a0000.emp ASC, a0000.tsk ASC) AS
          iter11_nat
     FROM tasks AS a0000)

SELECT a0003.dpt AS item15_str,
        a0002.emp AS item14_str, a0001.iter11_nat AS iter13_nat
   FROM t0000 AS a0001,
        employees AS a0002,
        departments AS a0003
  WHERE a0002.dpt = a0003.dpt
    AND a0001.item8_str = a0002.emp
  ORDER BY a0001.iter11_nat ASC, a0001.iter11_nat ASC;
