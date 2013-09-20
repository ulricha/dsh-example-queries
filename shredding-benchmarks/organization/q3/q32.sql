EXPLAIN ANALYZE
WITH
-- binding due to rownum operator
t0000 (item4_str, item5_str, item6_int, pos7_nat) AS
  (SELECT a0000.dpt AS item4_str, a0000.emp AS item5_str,
          a0000.salary AS item6_int,
          ROW_NUMBER () OVER (ORDER BY a0000.emp ASC) AS pos7_nat
     FROM employees AS a0000)

SELECT a0002.tsk AS item11_str, a0001.pos7_nat AS iter9_nat
   FROM t0000 AS a0001,
        tasks AS a0002
  WHERE a0001.item5_str = a0002.emp
  ORDER BY a0001.pos7_nat ASC, a0001.pos7_nat ASC, a0002.emp ASC, a0002.tsk ASC;
