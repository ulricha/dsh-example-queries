create index trades_tid_idx on trades(tid);
create index trades_tradedate_ts_idx on trades(tradedate, ts);
