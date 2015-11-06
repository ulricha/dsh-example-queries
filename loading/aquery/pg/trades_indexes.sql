create index on trades(tid);
create index on trades(tradedate, ts);
create unique index uniq1 on trades(tid, tradedate, ts);
create unique index uniq2 on trades(tid, ts);
