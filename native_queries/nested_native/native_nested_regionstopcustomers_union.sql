-- SQL version: include customers without orders, using UNION ALL
select n.n_name, json_agg(ii.c_name)
from region r,
     nation n,
     lateral (select i.c_name
              from (select ocs.c_name, row_number() over (partition by ocs.n
                                                          order by ocs.oc) as rn
                    from ((select c.c_name, n.n_nationkey as n, count(*) as oc
                          from customer c,
                               orders o
                          where c.c_nationkey = n.n_nationkey
                            and c.c_custkey = o.o_custkey
                          group by c.c_custkey, c.c_name)
                          union all
                          (select c.c_name, n.n_nationkey as n, 0 as oc
                           from customer c
                           where c.c_nationkey = n.n_nationkey
                             and c.c_custkey not in (select o.o_custkey from orders o))) ocs) i
              where i.rn <= 10) ii
where r.r_name = 'EUROPE'
  and r.r_regionkey = n.n_regionkey
group by n.n_nationkey
order by n.n_nationkey;
