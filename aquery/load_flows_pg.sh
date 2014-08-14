drop table packetslarge;
create table packetslarge(p_pid int not null, p_src int not null, p_dest int not null, p_len int not null, p_ts int not null);
copy packetslarge from '/home/au/work/dev/Alex.NestedQueries/aquery/packetslarge.csv' csv;
alter table packetslarge add primary key (p_pid);
analyze;
