-- Create additional indexes on the organisation schema for the shredding benchmark queries.

create index employees_salary_idx on employees(salary);
create index employees_dpt_idx on employees(dpt);
create index tasks_emp_idx on tasks(emp);
create index contacts_dpt_idx on contacts(dpt);
