explain analyze
select max(running_diff)
from (select t_price - min(t_price) over
               (order by t_timestamp
                rows unbounded preceding)
               as running_diff
      from trades
      where t_tid = 40 and t_tradedate = 15) as t1;
