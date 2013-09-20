EXPLAIN ANALYZE
WITH
-- binding due to rank operator
t0000 (item1_str, iter2_nat) AS
  (SELECT a0001.dpt AS item1_str,
          DENSE_RANK () OVER (ORDER BY a0001.dpt ASC) AS iter2_nat
     FROM departments AS a0001),

-- binding due to rownum operator
t0001 (item14_str, item15_str, item16_int, item1_str, iter2_nat,
  iter45_nat) AS
  (SELECT a0000.dpt AS item14_str, a0000.emp AS item15_str,
          a0000.salary AS item16_int, a0002.item1_str, a0002.iter2_nat,
          ROW_NUMBER () OVER
          (ORDER BY a0002.iter2_nat ASC, a0002.item1_str ASC, a0000.emp ASC) AS
          iter45_nat
     FROM employees AS a0000,
          t0000 AS a0002
    WHERE a0000.dpt = a0002.item1_str),

-- binding due to set operation
t0002 (iter29_nat, item30_bool) AS
  ((SELECT a0003.iter45_nat AS iter29_nat, 1 AS item30_bool
      FROM t0001 AS a0003
     WHERE 1000000 < a0003.item16_int)
   UNION ALL
   (SELECT a0004.iter45_nat AS iter29_nat,
           CASE WHEN a0004.item16_int < 1000 THEN 1 ELSE 0 END AS item30_bool
      FROM t0001 AS a0004
     WHERE NOT (1000000 < a0004.item16_int))),

-- binding due to rownum operator
t0003 (iter29_nat, item30_bool, item14_str, item15_str, item16_int,
  item1_str, iter2_nat, iter45_nat, iter21_nat, iter44_nat, iter52_nat) AS
  (SELECT a0005.iter29_nat, a0005.item30_bool, a0006.item14_str,
          a0006.item15_str, a0006.item16_int, a0006.item1_str, a0006.iter2_nat,
          a0006.iter45_nat, a0007.iter2_nat AS iter21_nat,
          a0007.iter45_nat AS iter44_nat,
          ROW_NUMBER () OVER
          (ORDER BY a0007.iter2_nat ASC, a0005.iter29_nat ASC) AS iter52_nat
     FROM t0002 AS a0005,
          t0001 AS a0006,
          t0001 AS a0007
    WHERE a0005.iter29_nat = a0006.iter45_nat
      AND a0005.iter29_nat = a0007.iter45_nat
      AND a0005.item30_bool = 1),

-- binding due to rownum operator
t0004 (item3_bool, item4_str, item5_int, item6_str, item1_str,
  iter2_nat, iter54_nat) AS
  (SELECT CASE WHEN a0009.client THEN 1 ELSE 0 END AS item3_bool,
          a0009.dpt AS item4_str, a0009.id AS item5_int,
          a0009.name AS item6_str, a0010.item1_str, a0010.iter2_nat,
          ROW_NUMBER () OVER
          (ORDER BY a0010.iter2_nat ASC, a0010.item1_str ASC, a0009.id ASC) AS
          iter54_nat
     FROM contacts AS a0009,
          t0000 AS a0010
    WHERE a0009.client
      AND a0009.dpt = a0010.item1_str),

-- binding due to set operation
t0005 (iter37_nat, item38_str, pos39_nat, pos40_nat) AS
  ((SELECT a0008.iter21_nat AS iter37_nat, a0008.item15_str AS item38_str,
           1 AS pos39_nat, a0008.iter52_nat AS pos40_nat
      FROM t0003 AS a0008)
   UNION ALL
   (SELECT a0011.iter2_nat AS iter37_nat, a0011.item6_str AS item38_str,
           2 AS pos39_nat, a0011.iter54_nat AS pos40_nat
      FROM t0004 AS a0011))

SELECT ROW_NUMBER () OVER
        (ORDER BY a0012.iter37_nat ASC, a0012.pos40_nat ASC, a0012.pos39_nat
         ASC) AS pos41_nat, a0012.item38_str, a0012.iter37_nat
   FROM t0005 AS a0012
  ORDER BY a0012.iter37_nat ASC, a0012.pos39_nat ASC, a0012.pos40_nat ASC;
