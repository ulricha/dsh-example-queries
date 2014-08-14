select max(running_diff)
from (select t_tid, t_tradedate,
             t_price - min(t_price) over
               (partition by t_tid, t_tradedate
	        order by t_timestamp
                rows unbounded preceding)
               as running_diff
      from tradessmall) as t1
where t_tid = 4 and t_tradedate = 2;
