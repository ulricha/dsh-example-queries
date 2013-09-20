EXPLAIN ANALYZE
WITH
-- binding due to rank operator
t0000 (item7_str, item4_str, item5_str, item6_int, pos17_nat) AS
  (SELECT a0000.dpt AS item7_str, a0001.dpt AS item4_str,
          a0001.emp AS item5_str, a0001.salary AS item6_int,
          DENSE_RANK () OVER (ORDER BY a0000.dpt ASC, a0001.emp ASC) AS
          pos17_nat
     FROM departments AS a0000,
          employees AS a0001
    WHERE a0000.dpt = a0001.dpt)

SELECT a0003.tsk AS item15_str, a0002.pos17_nat AS pos13_nat
   FROM t0000 AS a0002,
        tasks AS a0003
  WHERE a0002.item5_str = a0003.emp
  ORDER BY a0002.pos17_nat ASC, a0002.pos17_nat ASC, a0002.item5_str ASC,
        a0003.emp ASC, a0003.tsk ASC;
