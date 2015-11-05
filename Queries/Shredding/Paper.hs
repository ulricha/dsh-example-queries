{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ViewPatterns        #-}

-- | Benchmark queries from the SIGMOD 2014 paper on query shredding by Cheney et al.
module Queries.Shredding.Paper
    ( q1
    , q2
    , q3
    , q4
    , q5
    , q6
    ) where

import Prelude()
import Database.DSH
import Schema.Shredding
import Queries.Shredding.BuildingBlocks

--------------------------------------------------------------------------------

q1 :: Q [(Text, [(Text, (Integer, [Text]))], [(Text, Bool)])]
q1 = [ tup3 (d_dptQ d) (employeesOfDept d) (contactsOfDept d)
     | d <- departments
     ]

--------------------------------------------------------------------------------

q2 :: Q [Text]
q2 = [ d
     | (view -> (d, emps, _)) <- q1
     , all (\(view -> (_, st)) -> "abstract" `elem` (snd st)) emps
     ]

--------------------------------------------------------------------------------

q3 :: Q [(Text, [Text])]
q3 = [ tup2 (e_empQ e) (tasksOfEmp e)
     | e <- employees
     ]

--------------------------------------------------------------------------------

q4 :: Q [(Text, [Text])]
q4 = [ tup2 (d_dptQ d) [ e_empQ e | e <- employees, e_dptQ e == d_dptQ d ]
     | d <- departments
     ]

--------------------------------------------------------------------------------

q5 :: Q [(Text, [(Text, Text)])]
q5 = [ pair (t_tskQ t) (employeesByTask t) | t <- tasks ]

--------------------------------------------------------------------------------

isPoor :: Q (Text, (Integer, [Text])) -> Q Bool
isPoor (view -> (_, st)) = (fst st) < 1000

isRich :: Q (Text, (Integer, [Text])) -> Q Bool
isRich (view -> (_, st)) = (fst st) > 1000000

outliers :: Q [(Text, (Integer, [Text]))] -> Q [(Text, (Integer, [Text]))]
outliers = filter (\x -> isRich x || isPoor x)

clients :: Q [(Text, Bool)] -> Q [(Text, Bool)]
clients = filter snd

getTasks :: QA a => Q [(Text, a)] -> (Q a -> Q [Text]) -> Q [(Text, [Text])]
getTasks xs f = [ tup2 (fst x) (f $ snd x) | x <- xs ]

q6 :: Q [(Text, [(Text, [Text])])]
q6 = [ tup2 d (append (getTasks (outliers es) snd)
                      (getTasks (clients cs) (const $ toQ ["buy"])))
     | (view -> (d, es, cs)) <- q1
     ]
