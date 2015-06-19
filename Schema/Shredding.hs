{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

-- | The Employee-Department schema used in the "Query Shredding" paper.
module Schema.Shredding where

import           Data.List.NonEmpty(NonEmpty((:|)))

import           Data.Text
import           Database.DSH

data Department = Department
    { d_id  :: Integer
    , d_dpt :: Text
    }

deriveDSH ''Department
deriveTA ''Department
generateTableSelectors ''Department

departments :: Q [Department]
departments = table "departments"
                     ("id" :| ["dpt"])
                     (defaultHints $ pure $ Key (pure "id") )

data Employee = Employee
    { e_id     :: Integer
    , e_dpt    :: Text
    , e_emp    :: Text
    , e_salary :: Integer
    }

deriveDSH ''Employee
deriveTA ''Employee
generateTableSelectors ''Employee

employees :: Q [Employee]
employees = table "employees"
                  ("id" :| ["dpt", "emp", "salary"])
                  (defaultHints $ pure $ Key (pure "id"))

data Task = Task
    { t_emp :: Text
    , t_id  :: Integer
    , t_tsk :: Text
    }

deriveDSH ''Task
deriveTA ''Task
generateTableSelectors ''Task

tasks :: Q [Task]
tasks = table "tasks"
              ("emp" :| ["id", "tsk"])
              (defaultHints $ pure $ Key $ "emp" :| ["tsk"])

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
contacts = table "contacts"
                 ("client" :| ["dpt", "id", "name"])
                 (defaultHints $ pure $ Key $ pure "id")
