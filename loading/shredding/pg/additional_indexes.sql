-- Create additional indexes on the organisation schema for the shredding benchmark queries.

create index on employees(salary);
create index on employees(dpt);
create index on tasks(emp);
create index on contacts(dpt);
