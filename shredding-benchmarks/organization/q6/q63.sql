EXPLAIN ANALYZE
WITH
-- binding due to rownum operator
t0000 (item8_str, pos9_nat) AS
  (SELECT a0000.dpt AS item8_str,
          ROW_NUMBER () OVER (ORDER BY a0000.dpt ASC) AS pos9_nat
     FROM departments AS a0000),

-- binding due to rownum operator
t0001 (item8_str, pos9_nat, item18_str, item19_str, item20_int,
  item68_int, item69_int, item70_bool, item71_bool, item72_bool,
  pos73_nat) AS
  (SELECT a0001.item8_str, a0001.pos9_nat, a0002.dpt AS item18_str,
          a0002.emp AS item19_str, a0002.salary AS item20_int,
          1000000 AS item68_int, 1000 AS item69_int,
          CASE WHEN 1000000 < a0002.salary THEN 1 ELSE 0 END AS item70_bool,
          CASE WHEN a0002.salary < 1000 THEN 1 ELSE 0 END AS item71_bool,
          CASE WHEN ((1000000 < a0002.salary) OR (a0002.salary < 1000)) THEN 1
          ELSE 0 END AS item72_bool,
          ROW_NUMBER () OVER (ORDER BY a0001.pos9_nat ASC, a0002.emp ASC) AS
          pos73_nat
     FROM t0000 AS a0001,
          employees AS a0002
    WHERE ((1000000 < a0002.salary) OR (a0002.salary < 1000))
      AND a0001.item8_str = a0002.dpt),

-- binding due to rownum operator
t0002 (item8_str, pos9_nat, item4_bool, item5_str, item6_int,
  item7_str, pos75_nat) AS
  (SELECT a0004.item8_str, a0004.pos9_nat,
          CASE WHEN a0005.client THEN 1 ELSE 0 END AS item4_bool,
          a0005.dpt AS item5_str, a0005.id AS item6_int,
          a0005.name AS item7_str,
          ROW_NUMBER () OVER (ORDER BY a0004.pos9_nat ASC, a0005.name ASC) AS
          pos75_nat
     FROM t0000 AS a0004,
          contacts AS a0005
    WHERE a0005.client
      AND a0004.item8_str = a0005.dpt),

-- binding due to set operation
t0003 (pos35_nat, item36_nat, iter37_nat) AS
  ((SELECT a0003.pos73_nat AS pos35_nat, 1 AS item36_nat,
           a0003.pos9_nat AS iter37_nat
      FROM t0001 AS a0003)
   UNION ALL
   (SELECT a0006.pos75_nat AS pos35_nat, 2 AS item36_nat,
           a0006.pos9_nat AS iter37_nat
      FROM t0002 AS a0006)),

-- binding due to rownum operator
t0004 (pos35_nat, item36_nat, iter37_nat, item38_nat) AS
  (SELECT a0007.pos35_nat, a0007.item36_nat, a0007.iter37_nat,
          ROW_NUMBER () OVER
          (ORDER BY a0007.iter37_nat ASC, a0007.item36_nat ASC, a0007.pos35_nat
           ASC) AS item38_nat
     FROM t0003 AS a0007),

-- binding due to rownum operator
t0005 (item8_str, pos9_nat, item18_str, item19_str, item20_int,
  item68_int, item69_int, item70_bool, item71_bool, item72_bool,
  pos73_nat, item48_str, item49_int, item50_str, item77_nat) AS
  (SELECT a0009.item8_str, a0009.pos9_nat, a0009.item18_str, a0009.item19_str,
          a0009.item20_int, a0009.item68_int, a0009.item69_int,
          a0009.item70_bool, a0009.item71_bool, a0009.item72_bool,
          a0009.pos73_nat, a0010.emp AS item48_str, a0010.id AS item49_int,
          a0010.tsk AS item50_str,
          ROW_NUMBER () OVER
          (ORDER BY a0009.pos73_nat ASC, a0009.item19_str ASC, a0010.emp ASC,
           a0010.tsk ASC) AS item77_nat
     FROM t0001 AS a0009,
          tasks AS a0010
    WHERE a0009.item19_str = a0010.emp),

-- binding due to rank operator
t0006 (item8_str, pos9_nat, item4_bool, item5_str, item6_int,
  item7_str, pos75_nat, pos35_nat, item36_nat, iter37_nat, item38_nat,
  item45_str, item46_nat, item47_nat, item82_bool, item81_nat) AS
  (SELECT a0012.item8_str, a0012.pos9_nat, a0012.item4_bool, a0012.item5_str,
          a0012.item6_int, a0012.item7_str, a0012.pos75_nat, a0013.pos35_nat,
          a0013.item36_nat, a0013.iter37_nat, a0013.item38_nat,
          'buy'::text AS item45_str, 2 AS item46_nat, 2 AS item47_nat,
          CASE WHEN a0013.item36_nat = 2 THEN 1 ELSE 0 END AS item82_bool,
          DENSE_RANK () OVER (ORDER BY a0012.pos75_nat ASC) AS item81_nat
     FROM t0002 AS a0012,
          t0004 AS a0013
    WHERE a0012.pos75_nat = a0013.pos35_nat),

-- binding due to set operation
t0007 (iter61_nat, pos62_nat, item63_str, item64_nat) AS
  ((SELECT a0008.item38_nat AS iter61_nat, a0011.item77_nat AS pos62_nat,
           a0011.item50_str AS item63_str, 1 AS item64_nat
      FROM t0004 AS a0008,
           t0005 AS a0011
     WHERE a0008.pos35_nat = a0011.pos73_nat
       AND a0008.item36_nat = 1)
   UNION ALL
   (SELECT a0014.item38_nat AS iter61_nat, a0014.item81_nat AS pos62_nat,
           a0014.item45_str AS item63_str, a0014.item47_nat AS item64_nat
      FROM t0006 AS a0014
     WHERE a0014.item82_bool = 1))

SELECT a0015.item63_str, a0015.iter61_nat
   FROM t0007 AS a0015
  ORDER BY a0015.iter61_nat ASC, a0015.iter61_nat ASC, a0015.item64_nat ASC,
        a0015.pos62_nat ASC;
