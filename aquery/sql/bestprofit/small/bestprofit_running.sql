explain analyze
WITH t0 AS (
        SELECT ROW_NUMBER() OVER (ORDER BY a0.t_timestamp ASC, a0.t_tid ASC, a0.t_timestamp ASC
               ) AS pos, a0.t_price AS item1
        FROM tradessmall AS a0
        WHERE (a0.t_tradedate = 2)
              AND (a0.t_tid = 4)
    )
SELECT MAX((a1.item1 - a3.item5)) AS item1
FROM t0 AS a1,
     ( SELECT a2.pos AS pos1, MIN(a2.item1) OVER (ORDER BY a2.pos ASC
              ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS item5
       FROM t0 AS a2
     ) AS a3(pos1, item5)
WHERE (a1.pos = a3.pos1);
