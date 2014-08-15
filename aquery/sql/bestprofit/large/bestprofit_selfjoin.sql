explain analyze
WITH t0 AS (
        SELECT ROW_NUMBER() OVER (ORDER BY a0.t_tid ASC, a0.t_timestamp ASC
               ) AS posnew, a0.t_price AS item1, a0.t_tid AS item2,
               a0.t_timestamp AS item3, a0.t_tradedate AS item4
        FROM trades AS a0
        WHERE (a0.t_tradedate = 15)
              AND (a0.t_tid = 40)
    )
SELECT MAX((a1.item1 - a5.item1)) AS item1
FROM t0 AS a1,
     t0 AS a2,
     ( SELECT a3.posnew AS descr, MIN(a4.item1) AS item1
       FROM t0 AS a3,
            t0 AS a4
       WHERE (CAST(a3.posnew AS INTEGER) >= CAST(a4.posnew AS INTEGER))
       GROUP BY a3.posnew
     ) AS a5(descr, item1)
WHERE (a1.posnew = a2.posnew)
      AND (a2.posnew = a5.descr);
