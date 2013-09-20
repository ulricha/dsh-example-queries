{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Records where 

import Data.Text
import Database.DSH

type Department = Text

departments :: Q [Department]
departments = tableWithKeys "departments" [["dpt"]]
-- departments = toQ []

data Employee = Employee 
    { e_dpt    :: Text
    , e_emp    :: Text
    , e_salary :: Integer
    }

deriveDSH ''Employee
deriveTA ''Employee
generateTableSelectors ''Employee

employees :: Q [Employee]
employees = tableWithKeys "employees" [["emp"]]
-- employees = toQ []

data Task = Task 
    { t_emp :: Text
    , t_id  :: Integer
    , t_tsk :: Text
    }

deriveDSH ''Task
deriveTA ''Task
generateTableSelectors ''Task

tasks :: Q [Task]
tasks = tableWithKeys "tasks" [["emp", "tsk"]]
-- tasks = toQ []

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
contacts = tableWithKeys "contacts" [["name"]]
-- contacts = toQ []
