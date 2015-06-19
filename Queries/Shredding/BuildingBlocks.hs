{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ViewPatterns        #-}

module Queries.Shredding.BuildingBlocks
    ( tasksOfEmp
    , employeesOfDept
    , contactsOfDept
    , employeesByTask
    ) where

import Prelude()
import Database.DSH
import Schema.Shredding

--------------------------------------------------------------------------------
-- Query building blocks

tasksOfEmp :: Q Employee -> Q [Text]
tasksOfEmp e = [ t_tskQ t | t <- tasks, e_empQ e == t_empQ t ]

employeesOfDept :: Q Department -> Q [(Text, (Integer, [Text]))]
employeesOfDept d = [ tup2 (e_empQ e) (tup2 (e_salaryQ e) (tasksOfEmp e))
                    | e <- employees
                    , d_dptQ d == e_dptQ e ]

contactsOfDept :: Q Department -> Q [(Text, Bool)]
contactsOfDept d = [ tup2 (d_dptQ d) (c_clientQ c)
                   | c <- contacts
                   , d_dptQ d == c_dptQ c
                   ]

employeesByTask :: Q Task -> Q [(Text, Text)]
employeesByTask t = [ pair (e_empQ e) (d_dptQ d)
                    | e <- employees
                    , d <- departments
                    , e_empQ e == t_empQ t
                    , e_dptQ e == d_dptQ d
                    ]
