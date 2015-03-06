{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Schema.Shredding where

import Data.Text
import Database.DSH

type Department = Text

departments :: Q [Department]
departments = table "departments" $ TableHints [ Key ["dpt"] ] NonEmpty

data Employee = Employee
    { e_dpt    :: Text
    , e_emp    :: Text
    , e_salary :: Integer
    }

deriveDSH ''Employee
deriveTA ''Employee
generateTableSelectors ''Employee

employees :: Q [Employee]
employees = table "employees" $ TableHints [ Key ["emp"] ] NonEmpty

data Task = Task
    { t_emp :: Text
    , t_id  :: Integer
    , t_tsk :: Text
    }

deriveDSH ''Task
deriveTA ''Task
generateTableSelectors ''Task

tasks :: Q [Task]
tasks = table "tasks" $ TableHints [ Key ["emp", "tsk"] ] NonEmpty

data Contact = Contact
    { c_client :: Bool
    , c_dpt    :: Text
    , c_id     :: Integer
    , c_name   :: Text
    }

deriveDSH ''Contact
deriveTA ''Contact
generateTableSelectors ''Contact

contacts :: Q [Contact]
contacts = table "contacts" $ TableHints [ Key ["name"] ] NonEmpty
