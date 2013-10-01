create table departments(dpt text primary key);
create table contacts(id serial primary key, name text, dpt text, client bool);
create table tasks(id serial primary key, emp text, tsk text);
create table employees(emp text primary key, dpt text, salary int);

create index emp_sal_idx on employees(salary);
create index tasks_tsk_idx on tasks using hash(tsk);

create index tasks_emp_idx on tasks(emp);
create index contacts_dpt_idx on contacts(dpt);
