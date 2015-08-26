-- For each supplier with a below_average account balance from a certain region,
-- compute for each supplied part the suppliers that are cheaper.

select s.s_name, json_agg(json_build_object('part', i.p_name, 'owncost', i.ps_supplycost, 'competitors', i.obj))
from supplier s,
     lateral (select p.p_name, ps.ps_supplycost, json_agg(json_build_object('name', ii.s_name, 'cost', ii.ps_supplycost)) as obj
              from part p,
                   partsupp ps,
                   lateral (select s1.s_name, ps1.ps_supplycost
                            from supplier s1,
                                 partsupp ps1
                            where ps1.ps_partkey = p.p_partkey
                            and   ps.ps_suppkey <> ps1.ps_suppkey
                            and s1.s_suppkey = ps1.ps_suppkey
                            and ps1.ps_supplycost < ps.ps_supplycost
                            and s.s_nationkey in (select n.n_nationkey
                                                  from nation n, region r
                                                  where n.n_regionkey = r.r_regionkey
                                                  and r.r_name = 'EUROPE')) ii
              where ps.ps_partkey = p.p_partkey
              and s.s_suppkey = ps.ps_suppkey
              group by p.p_partkey, p.p_name, ps.ps_supplycost) i
where s.s_nationkey in (select n.n_nationkey
                        from nation n, region r
                        where n.n_regionkey = r.r_regionkey
                        and r.r_name = 'EUROPE')
and s.s_acctbal < (select avg(s1.s_acctbal)
                   from supplier s1
                   where s1.s_nationkey in (select n.n_nationkey
                                            from nation n, region r
                                            where n.n_regionkey = r.r_regionkey
                                            and r.r_name = 'EUROPE'))
group by s.s_suppkey, s.s_name;
