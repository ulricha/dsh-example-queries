create table packets(pid int not null primary key,
                     src int not null,
                     dst int not null,
                     len int not null,
                     ts int not null);
