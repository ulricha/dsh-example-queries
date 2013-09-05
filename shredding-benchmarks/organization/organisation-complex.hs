{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

    
import           Data.Text hiding (all, singleton, length, null)

import qualified Prelude as P
import           Database.DSH
       
import           Records

-- Challenges
-- 1. not null -> EXISTS
-- 2. better criterion for equijoin predicates when pushing them down
--    => don't push predicates with free variables not bound by a local generator.
q1 :: Q [(Text, Text)]
q1 = [ pair (e_empQ e1) (e_empQ e2)
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
q2 :: Q [(Employee, Employee)]
q2 = 
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
  
q3 :: Q [(Employee, Employee, [Text])]
q3 = 
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
                      
