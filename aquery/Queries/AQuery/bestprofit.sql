explain analyze
select max(running_diff)
from (select t_tid, t_tradedate,
             t_price - min(t_price) over
               (partition by t_tid, t_tradedate
	        order by t_timestamp
                rows unbounded preceding)
               as running_diff
      from trades) as t1
where t_tid = 94 and t_tradedate = 15;

explain analyze
select max(running_diff)
from (select t_price - min(t_price) over
               (order by t_timestamp
                rows unbounded preceding)
               as running_diff
      from trades
      where t_tid = 94 and t_tradedate = 15) as t1;
