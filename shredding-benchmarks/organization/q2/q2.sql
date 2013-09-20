EXPLAIN ANALYZE
WITH
-- binding due to rownum operator
t0000 (item1_str, pos2_nat) AS
  (SELECT a0000.dpt AS item1_str,
          ROW_NUMBER () OVER (ORDER BY a0000.dpt ASC) AS pos2_nat
     FROM departments AS a0000),

-- binding due to rownum operator
t0001 (item1_str, pos2_nat, item8_str, item9_str, item10_int,
  pos36_nat) AS
  (SELECT a0002.item1_str, a0002.pos2_nat, a0003.dpt AS item8_str,
          a0003.emp AS item9_str, a0003.salary AS item10_int,
          ROW_NUMBER () OVER (ORDER BY a0002.pos2_nat ASC, a0003.emp ASC) AS
          pos36_nat
     FROM t0000 AS a0002,
          employees AS a0003
    WHERE a0002.item1_str = a0003.dpt),

-- binding due to set operation
t0002 (iter21_nat, item22_bool) AS
  ((SELECT a0005.pos36_nat AS iter21_nat, 0 AS item22_bool
      FROM t0001 AS a0005)
   UNION ALL
   (SELECT a0006.pos36_nat AS iter21_nat,
           CASE WHEN 'abstract' = a0007.tsk THEN 1 ELSE 0 END AS item22_bool
      FROM t0001 AS a0006,
           tasks AS a0007
     WHERE a0006.item9_str = a0007.emp)),

-- binding due to aggregate
t0003 (iter21_nat, item23_bool) AS
  (SELECT a0008.iter21_nat, MAX (a0008.item22_bool) AS item23_bool
     FROM t0002 AS a0008
    GROUP BY a0008.iter21_nat),

-- binding due to set operation
t0004 (iter27_nat, item28_bool) AS
  ((SELECT a0001.pos2_nat AS iter27_nat, 1 AS item28_bool
      FROM t0000 AS a0001)
   UNION ALL
   (SELECT a0004.pos2_nat AS iter27_nat, a0009.item23_bool AS item28_bool
      FROM t0001 AS a0004,
           t0003 AS a0009
     WHERE a0004.pos36_nat = a0009.iter21_nat)),

-- binding due to aggregate
t0005 (iter27_nat, item29_bool) AS
  (SELECT a0010.iter27_nat, MIN (a0010.item28_bool) AS item29_bool
     FROM t0004 AS a0010
    GROUP BY a0010.iter27_nat),

-- binding due to rank operator
t0006 (iter27_nat, item29_bool, pos30_nat) AS
  (SELECT a0011.iter27_nat, a0011.item29_bool,
          DENSE_RANK () OVER (ORDER BY a0011.iter27_nat ASC) AS pos30_nat
     FROM t0005 AS a0011)

SELECT 1 AS iter33_nat, a0013.item1_str
   FROM t0006 AS a0012,
        t0000 AS a0013
  WHERE a0012.pos30_nat = a0013.pos2_nat
    AND a0012.item29_bool = 1
  ORDER BY a0012.pos30_nat ASC;
