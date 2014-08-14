drop table trades;
create table trades(t_price real not null, t_tid int not null, t_timestamp int not null, t_tradeDate int not null);
copy trades from '/home/au/work/dev/Alex.NestedQueries/aquery/trades.csv' csv;
alter table trades add primary key (t_tid, t_timestamp);
analyze;
