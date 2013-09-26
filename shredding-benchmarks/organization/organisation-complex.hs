{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

    
import           Data.Text hiding (all, singleton, length, null)

import qualified Prelude as P
import           Database.DSH
import           Database.DSH.Compiler

import           Database.HDBC
import           Database.HDBC.PostgreSQL
       
import           Records

-- Challenges
-- 1. not null -> EXISTS
-- 2. better criterion for equijoin predicates when pushing them down
--    => don't push predicates with free variables not bound by a local generator.
-- 3. Split the existential predicate into to two predicates which only correlate
--    with one of the generators, respectively.

-- Pairs of employees in same department with common task.
q1c :: Q [(Text, Text)]
q1c = [ pair (e_empQ e1) (e_empQ e2)
     | e1 <- employees
     , e2 <- employees
     , e_dptQ e1 == e_dptQ e2
     , not $ null [ toQ ()
                  | t1 <- tasks, t2 <- tasks
                  , t_empQ t1 == e_empQ e1
                  , t_empQ t2 == e_empQ e2
                  , t_tskQ t1 == t_tskQ t2
                  ]
     ]
     
-- Challenges
-- 1. not null -> EXISTS (semijoin)
-- 2. null -> NOT EXISTS (antijoin)
-- 3. two levels of existential nesting, with correlation over both levels.
-- 4. unequality predicate -> thetajoin

-- Pairs of Employees in the same department where x earns less than y and x can
-- do task y cannot do.
q2c :: Q [(Employee, Employee)]
q2c = 
  [ pair e1 e2
  | e1 <- employees
  , e2 <- employees
  , e_dptQ e1 == e_dptQ e2
  , e_empQ e1 /= e_empQ e2
  , e_salaryQ e1 < e_salaryQ e2
  , not $ null [ toQ ()
               | t <- tasks
               , t_empQ t == e_empQ e1
               , null [ toQ () | t2 <- tasks
                      , t_empQ t2 == e_empQ e2
                      , t_tskQ t == t_tskQ t2 ]
               ]
  ]

thd :: (QA a, QA b, QA c) => Q (a, b, c) -> Q c
thd (view -> (_, _, c)) = c
  
-- Employees where x can do task y cannot do, and x earns less than y, along
-- with lists of tasks.
q3c :: Q [(Employee, Employee, [Text])]
q3c = 
  let tbl = [ tuple3 e1 e2
                     [ t_tskQ t
                     | t <- tasks, t_empQ t == e_empQ e1
                     , null [ toQ () | t2 <- tasks, e_empQ e2 == t_empQ t2, t_tskQ t == t_tskQ t2 ]
                     ]
            | e1 <- employees, e2 <- employees
            , e_dptQ e1 == e_dptQ e2
            , e_empQ e1 /= e_empQ e2
            , e_salaryQ e1 < e_salaryQ e2
            ]
            
  in [ r | r <- tbl, not $ null $ thd r ]
  
-- Employees e1, e2 where e1 and e2 do different tasks, with tagged union of
-- tasks.
q4c :: Q [(Employee, Employee, [(Text, Text)])]
q4c =
  [ let as = [ pair (toQ "a") (t_tskQ t)
             | t <- tasks
             , e_empQ e1 == t_empQ t ]
        bs = [ pair (toQ "b") (t_tskQ t)
             | t <- tasks
             , e_empQ e2 == t_empQ t ]
    in tuple3 e1 e2 (as ++ bs)
  | e1 <- employees
  , e2 <- employees
  , e_dptQ e1 == e_dptQ e2
  , e_empQ e1 /= e_empQ e2
  ]
  

-- Employees e1, e2 in same department where e1 and e2 do different tasks, with
-- tagged symmetric difference of tasks.
q5c :: Q [(Employee, Employee, [(Text, Text)])]
q5c =
  let tbl = [ tuple3
               e1
               e2
               ([ pair (toQ "a") (t_tskQ t1)
                | t1 <- tasks
                , e_empQ e1 == t_empQ t1
                , null [ toQ ()
                       | t2 <- tasks
                       , t_tskQ t1 == t_tskQ t2
                       , e_empQ e2 == t_tskQ t2
                       ]
                ]
                ++
                [ pair (toQ "b") (t_tskQ t1)
                | t1 <- tasks
                , e_empQ e1 == t_empQ t1
                , null [ toQ ()
                       | t2 <- tasks
                       , t_tskQ t1 == t_tskQ t2
                       , e_empQ e1 == t_tskQ t2
                       ]
                ])
            | e1 <- employees
            , e2 <- employees
            , e_dptQ e1 == e_dptQ e2
            , e_empQ e1 /= e_empQ e2
            ]
  in [ t | t@(view -> (_, _, c)) <- tbl, not $ null c ]

-- getConn :: IO X100Info
-- getConn = P.return $ x100Info "localhost" "48130" Nothing

getPGConn :: IO Connection
getPGConn = connectPostgreSQL "user = 'au' password = 'foobar' host = 'localhost' dbname = 'organisation16'"

main :: IO ()
main = getPGConn
       P.>>= (\conn -> debugVL "q1c" conn q1c)
