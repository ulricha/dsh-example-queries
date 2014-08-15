explain
SELECT MAX((a2.item1 - a2.item5)) AS item1
FROM ( SELECT MIN(a1.item1) OVER (ORDER BY a1.pos ASC
              ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS item5,
              a1.pos AS pos, a1.item1 AS item1
       FROM ( SELECT ROW_NUMBER() OVER (ORDER BY a0.t_timestamp ASC, a0.t_tid ASC, a0.t_timestamp ASC
                     ) AS pos, a0.t_price AS item1
              FROM trades AS a0
              WHERE (a0.t_tradedate = 15)
                    AND (a0.t_tid = 40)
            ) AS a1(pos, item1)
     ) AS a2(item5, pos, item1);
