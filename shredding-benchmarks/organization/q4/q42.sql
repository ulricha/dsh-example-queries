EXPLAIN ANALYZE
WITH
-- binding due to rownum operator
t0000 (item4_str, pos5_nat) AS
  (SELECT a0000.dpt AS item4_str,
          ROW_NUMBER () OVER (ORDER BY a0000.dpt ASC) AS pos5_nat
     FROM departments AS a0000)

SELECT a0002.emp AS item9_str, a0001.pos5_nat AS iter7_nat
   FROM t0000 AS a0001,
        employees AS a0002
  WHERE a0001.item4_str = a0002.dpt
  ORDER BY a0001.pos5_nat ASC, a0001.pos5_nat ASC, a0002.emp ASC;
