EXPLAIN ANALYZE
WITH
-- binding due to rownum operator
t0000 (item1_str, iter2_nat) AS
  (SELECT a0001.dpt AS item1_str,
          ROW_NUMBER () OVER (ORDER BY a0001.dpt ASC) AS iter2_nat
     FROM departments AS a0001),

-- binding due to rownum operator
t0001 (item3_bool, item4_str, item5_int, item6_str, item1_str,
  iter2_nat, iter80_nat) AS
  (SELECT CASE WHEN a0000.client THEN 1 ELSE 0 END AS item3_bool,
          a0000.dpt AS item4_str, a0000.id AS item5_int,
          a0000.name AS item6_str, a0002.item1_str, a0002.iter2_nat,
          ROW_NUMBER () OVER
          (ORDER BY a0002.iter2_nat ASC, a0002.item1_str ASC, a0000.id ASC) AS
          iter80_nat
     FROM contacts AS a0000,
          t0000 AS a0002
    WHERE a0000.client
      AND a0000.dpt = a0002.item1_str),

-- binding due to rownum operator
t0002 (item1_str, item13_str, item14_str, item15_int, iter84_nat) AS
  (SELECT a0004.dpt AS item1_str, a0005.dpt AS item13_str,
          a0005.emp AS item14_str, a0005.salary AS item15_int,
          ROW_NUMBER () OVER (ORDER BY a0004.dpt ASC, a0005.emp ASC) AS
          iter84_nat
     FROM departments AS a0004,
          employees AS a0005),

-- binding due to rownum operator
t0003 (item1_str, item13_str, item14_str, item15_int, iter84_nat,
  item85_bool, item44_str, item45_int, item46_str, iter86_nat) AS
  (SELECT a0006.item1_str, a0006.item13_str, a0006.item14_str, a0006.item15_int,
          a0006.iter84_nat,
          CASE WHEN a0006.item1_str = a0006.item13_str THEN 1 ELSE 0 END AS
          item85_bool, a0007.emp AS item44_str, a0007.id AS item45_int,
          a0007.tsk AS item46_str,
          ROW_NUMBER () OVER (ORDER BY a0006.iter84_nat ASC, a0007.id ASC) AS
          iter86_nat
     FROM t0002 AS a0006,
          tasks AS a0007
    WHERE a0006.item1_str = a0006.item13_str),

-- binding due to set operation
t0004 (pos69_nat, iter70_nat, item71_str, pos72_nat) AS
  ((SELECT 1 AS pos69_nat, a0003.iter80_nat AS iter70_nat, 'buy' AS item71_str,
           2 AS pos72_nat
      FROM t0001 AS a0003)
   UNION ALL
   (SELECT a0008.iter86_nat AS pos69_nat, a0008.iter84_nat AS iter70_nat,
           a0008.item46_str AS item71_str, 1 AS pos72_nat
      FROM t0003 AS a0008
     WHERE a0008.item1_str = a0008.item13_str
       AND a0008.item14_str = a0008.item44_str)),

-- binding due to rownum operator
t0005 (item1_str, item13_str, item14_str, item15_int, iter84_nat,
  item76_str, iter2_nat, item90_bool, iter91_nat) AS
  (SELECT a0010.item1_str, a0010.item13_str, a0010.item14_str, a0010.item15_int,
          a0010.iter84_nat, a0011.item1_str AS item76_str, a0011.iter2_nat,
          CASE WHEN a0010.item1_str = a0010.item13_str THEN 1 ELSE 0 END AS
          item90_bool,
          ROW_NUMBER () OVER
          (ORDER BY a0011.iter2_nat ASC, a0010.iter84_nat ASC) AS iter91_nat
     FROM t0002 AS a0010,
          t0000 AS a0011
    WHERE a0010.item1_str = a0011.item1_str
      AND a0010.item1_str = a0010.item13_str),

-- binding due to set operation
t0006 (iter31_nat, item32_bool) AS
  ((SELECT a0012.iter91_nat AS iter31_nat, 1 AS item32_bool
      FROM t0005 AS a0012
     WHERE 1000000 < a0012.item15_int)
   UNION ALL
   (SELECT a0013.iter91_nat AS iter31_nat,
           CASE WHEN a0013.item15_int < 1000 THEN 1 ELSE 0 END AS item32_bool
      FROM t0005 AS a0013
     WHERE NOT (1000000 < a0013.item15_int))),

-- binding due to rownum operator
t0007 (iter24_nat, item32_bool, iter20_nat, item1_str, item13_str,
  item14_str, item15_int, iter84_nat, item76_str, iter2_nat,
  item90_bool, iter91_nat, iter98_nat) AS
  (SELECT a0014.iter31_nat AS iter24_nat, a0014.item32_bool,
          a0015.iter84_nat AS iter20_nat, a0016.item1_str, a0016.item13_str,
          a0016.item14_str, a0016.item15_int, a0016.iter84_nat,
          a0016.item76_str, a0016.iter2_nat, a0016.item90_bool,
          a0016.iter91_nat,
          ROW_NUMBER () OVER
          (ORDER BY a0016.iter2_nat ASC, a0014.iter31_nat ASC) AS iter98_nat
     FROM t0006 AS a0014,
          t0005 AS a0015,
          t0005 AS a0016
    WHERE a0014.iter31_nat = a0015.iter91_nat
      AND a0014.iter31_nat = a0016.iter91_nat
      AND a0014.item32_bool = 1),

-- binding due to set operation
t0008 (item39_nat, pos40_nat, iter41_nat, pos42_nat) AS
  ((SELECT a0017.iter20_nat AS item39_nat, 1 AS pos40_nat,
           a0017.iter2_nat AS iter41_nat, a0017.iter98_nat AS pos42_nat
      FROM t0007 AS a0017)
   UNION ALL
   (SELECT a0018.iter80_nat AS item39_nat, 2 AS pos40_nat,
           a0018.iter2_nat AS iter41_nat, a0018.iter80_nat AS pos42_nat
      FROM t0001 AS a0018)),

-- binding due to rownum operator
t0009 (item39_nat, pos40_nat, iter41_nat, pos42_nat, pos43_nat) AS
  (SELECT a0019.item39_nat, a0019.pos40_nat, a0019.iter41_nat, a0019.pos42_nat,
          ROW_NUMBER () OVER
          (ORDER BY a0019.iter41_nat ASC, a0019.pos42_nat ASC, a0019.pos40_nat
           ASC) AS pos43_nat
     FROM t0008 AS a0019)

SELECT a0020.pos43_nat, a0009.item71_str
   FROM t0004 AS a0009,
        t0009 AS a0020
  WHERE a0009.iter70_nat = a0020.item39_nat
    AND a0020.pos40_nat = a0009.pos72_nat
  ORDER BY a0020.pos43_nat ASC, a0009.pos69_nat ASC;
