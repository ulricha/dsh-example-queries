(select c.c_name, json_agg(json_build_object('o', i.o_orderkey, 'ls', i.obj))
from customer c,
     lateral (select o.o_orderkey,
                     json_agg(json_build_object('ln', ii.l_linenumber, 'n', ii.p_name, 'q', ii.l_quantity)) as obj
              from orders o,
                   lateral (select l.l_linenumber, p.p_name, l.l_quantity
                            from lineitem l,
                                 part p
                            where l.l_orderkey = o.o_orderkey
                            and   p.p_partkey = l.l_partkey) ii
              where o.o_custkey = c.c_custkey
              and   o.o_orderstatus = 'P'
              and   o.o_orderdate > date '1996-01-01'
              group by o.o_orderkey) i
where c.c_nationkey in (select n.n_nationkey
                        from nation n
                        where n.n_name = 'GERMANY')
group by c.c_custkey, c.c_name)
union
(select c.c_name, '[]'::jsonb
from customer c
where c.c_nationkey in (select n.n_nationkey
                        from nation n
                        where n.n_name = 'GERMANY')
and c.c_custkey not in (select o.o_orderkey from orders o));
