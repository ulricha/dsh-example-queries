explain analyze
SELECT MAX((a1.item1 - a1.item5)) AS item1
FROM ( SELECT a0.t_price AS item1,
              MIN(a0.t_price) OVER (ORDER BY a0.t_timestamp ASC, a0.t_tid ASC, a0.t_timestamp ASC
              ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS item5
       FROM trades AS a0
       WHERE (a0.t_tradedate = 15)
             AND (a0.t_tid = 40)
     ) AS a1(item1, item5);
