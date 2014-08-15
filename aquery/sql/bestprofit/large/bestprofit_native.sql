explain analyze
select max(running_diff)
from (select t_tid, t_tradedate,
             t_price - min(t_price) over
               (partition by t_tid, t_tradedate
	        order by t_timestamp
                rows unbounded preceding)
               as running_diff
      from trades) as t1
where t_tid = 40 and t_tradedate = 15;
