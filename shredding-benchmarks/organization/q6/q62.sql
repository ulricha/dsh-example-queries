EXPLAIN ANALYZE
WITH
-- binding due to rownum operator
t0000 (item5_str, pos6_nat) AS
  (SELECT a0000.dpt AS item5_str,
          ROW_NUMBER () OVER (ORDER BY a0000.dpt ASC) AS pos6_nat
     FROM departments AS a0000),

-- binding due to rownum operator
t0001 (item5_str, pos6_nat, item15_str, item16_str, item17_int,
  item37_int, item38_int, item39_bool, item40_bool, item41_bool,
  pos42_nat) AS
  (SELECT a0001.item5_str, a0001.pos6_nat, a0002.dpt AS item15_str,
          a0002.emp AS item16_str, a0002.salary AS item17_int,
          1000000 AS item37_int, 1000 AS item38_int,
          CASE WHEN 1000000 < a0002.salary THEN 1 ELSE 0 END AS item39_bool,
          CASE WHEN a0002.salary < 1000 THEN 1 ELSE 0 END AS item40_bool,
          CASE WHEN ((1000000 < a0002.salary) OR (a0002.salary < 1000)) THEN 1
          ELSE 0 END AS item41_bool,
          ROW_NUMBER () OVER (ORDER BY a0001.pos6_nat ASC, a0002.emp ASC) AS
          pos42_nat
     FROM t0000 AS a0001,
          employees AS a0002
    WHERE ((1000000 < a0002.salary) OR (a0002.salary < 1000))
      AND a0001.item5_str = a0002.dpt),

-- binding due to rownum operator
t0002 (item5_str, pos6_nat, item1_bool, item2_str, item3_int,
  item4_str, pos44_nat) AS
  (SELECT a0004.item5_str, a0004.pos6_nat,
          CASE WHEN a0005.client THEN 1 ELSE 0 END AS item1_bool,
          a0005.dpt AS item2_str, a0005.id AS item3_int,
          a0005.name AS item4_str,
          ROW_NUMBER () OVER (ORDER BY a0004.pos6_nat ASC, a0005.name ASC) AS
          pos44_nat
     FROM t0000 AS a0004,
          contacts AS a0005
    WHERE a0005.client
      AND a0004.item5_str = a0005.dpt),

-- binding due to set operation
t0003 (iter32_nat, pos33_nat, item34_str, item35_nat) AS
  ((SELECT a0003.pos6_nat AS iter32_nat, a0003.pos42_nat AS pos33_nat,
           a0003.item16_str AS item34_str, 1 AS item35_nat
      FROM t0001 AS a0003)
   UNION ALL
   (SELECT a0006.pos6_nat AS iter32_nat, a0006.pos44_nat AS pos33_nat,
           a0006.item4_str AS item34_str, 2 AS item35_nat
      FROM t0002 AS a0006))

SELECT a0007.item34_str, a0007.iter32_nat
   FROM t0003 AS a0007
  ORDER BY a0007.iter32_nat ASC, a0007.iter32_nat ASC, a0007.item35_nat ASC,
        a0007.pos33_nat ASC;
