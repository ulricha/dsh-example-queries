explain analyze
WITH
  Prec (src, dest, length, ts, ptime) AS
  (SELECT p_src, p_dest, p_len, p_ts,
  	  MIN(p_ts) OVER (PARTITION BY p_src, p_dest
		        ORDER BY p_ts
		     	ROWS BETWEEN 1 PRECEDING
		     	AND 1 PRECEDING)
   FROM packets),

  FLOW (src, dest, length, ts, ag) AS
  (SELECT src, dest, length, ts,
  	  CASE WHEN ts-ptime > 120 THEN 1 ELSE 0 END
   FROM Prec),
  
  FlowID (src, dest, length, ts, fID) AS
  (SELECT src, dest, length, ts,
          SUM(ag) OVER (ORDER BY src, dest, ts
	  	        ROWS UNBOUNDED PRECEDING)
   FROM Flow)

  SELECT src, dest, AVG(length), COUNT(ts)
  FROM FlowID
  GROUP BY src, dest, fID;
