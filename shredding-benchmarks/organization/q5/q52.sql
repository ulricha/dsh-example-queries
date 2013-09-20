EXPLAIN ANALYZE
WITH
-- binding due to rank operator
t0000 (item11_str, item12_int, item13_str, iter14_nat) AS
  (SELECT a0000.emp AS item11_str, a0000.id AS item12_int,
          a0000.tsk AS item13_str,
          DENSE_RANK () OVER (ORDER BY a0000.emp ASC, a0000.tsk ASC) AS
          iter14_nat
     FROM tasks AS a0000)

SELECT a0005.emp AS item26_str,
        a0004.dpt AS item25_str, a0001.iter14_nat AS iter24_nat
   FROM t0000 AS a0001,
        t0000 AS a0002,
        employees AS a0003,
        departments AS a0004,
        employees AS a0005,
        departments AS a0006
  WHERE a0001.iter14_nat = a0002.iter14_nat
    AND a0003.dpt = a0004.dpt
    AND a0002.item11_str = a0003.emp
    AND a0005.dpt = a0006.dpt
    AND a0001.item11_str = a0005.emp
  ORDER BY a0001.iter14_nat ASC, a0001.iter14_nat ASC;
