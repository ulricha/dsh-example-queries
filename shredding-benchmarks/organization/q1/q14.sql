EXPLAIN ANALYZE
WITH
-- binding due to rownum operator
t0000 (item5_str, pos6_nat) AS
  (SELECT a0000.dpt AS item5_str,
          ROW_NUMBER () OVER (ORDER BY a0000.dpt ASC) AS pos6_nat
     FROM departments AS a0000)

SELECT a0002.name AS item11_str,
        CASE WHEN a0002.client THEN 1 ELSE 0 END AS item9_bool,
        a0001.pos6_nat AS iter8_nat
   FROM t0000 AS a0001,
        contacts AS a0002
  WHERE a0001.item5_str = a0002.dpt
  ORDER BY a0001.pos6_nat ASC, a0001.pos6_nat ASC, a0002.name ASC;
