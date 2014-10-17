drop table tradessmall;
create table tradessmall(t_price real not null, t_tid int not null, t_timestamp int not null, t_tradeDate int not null);
copy tradessmall from '/home/au/work/dev/Alex.NestedQueries/aquery/tradessmall.csv' csv;
alter table tradessmall add primary key (t_tid, t_timestamp);
analyze;
