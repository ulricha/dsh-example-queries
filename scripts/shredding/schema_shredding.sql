create table departments(id int primary key,
                         dpt text not null unique);
create table employees(id int primary key,
                       dpt text not null references departments(dpt),
                       name text not null unique,
                       salary int not null);
create table tasks(id int primary key,
                   emp text not null references employees(name),
                   tsk text not null);
create table contacts(id int primary key,
                      name text not null,
                      dpt text not null references departments(dpt),
                      client bool not null);
