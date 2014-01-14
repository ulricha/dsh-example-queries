{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

    
import           Data.Text hiding (all, singleton)

import           Database.DSH
import           Database.DSH.Compiler
import           Database.X100Client
import qualified Prelude as P
       
import           Database.HDBC
import           Database.HDBC.PostgreSQL
       
import           Records

tasksOfEmployee :: Q Employee -> Q [Text]
tasksOfEmployee e = [ t_tskQ t | t <- tasks, e_empQ e == t_empQ t ]

employeesOfDepartment :: Q Department -> Q [(Text, [Text])]
employeesOfDepartment d = [ pair (e_empQ e) (tasksOfEmployee e)
                          | e <- employees
                          , d == e_dptQ e ]
                          
contactsOfDepartment :: Q Department -> Q [(Text, Bool)]
contactsOfDepartment d = [ pair (c_nameQ c) (c_clientQ c)
                         | c <- contacts
                         , d == c_dptQ c ]
                         
-- q3: nestedOrg
-- nestjoin over multiple levels
q1 :: Q [(Text, [(Text, [Text])], [(Text, Bool)])]
q1 = [ tuple3 d (employeesOfDepartment d) (contactsOfDepartment d)
     | d <- departments ]
            
type NestedOrg = (Text, [(Text, Integer, [Text])], [(Text, Bool)])

nestedOrg2 :: Q [NestedOrg]
nestedOrg2 =
  [ let es = [ tuple3 (e_empQ e)
                      (e_salaryQ e)
                      [ t_tskQ t | t <- tasks, e_empQ e == t_empQ t ]
             | e <- employees
             , d == e_dptQ e ]

        cs = [ pair (c_nameQ c) (c_clientQ c)
             | c <- contacts
             , d == c_dptQ c ]

    in tuple3 d es cs
  | d <- departments
  ]
  
expertise :: Q [NestedOrg] -> Q Text -> Q [Department]
expertise nestedOrg u =
  [ d
  | (view -> (d, es, _)) <- nestedOrg
  , all (\(view -> (_, _, tsks)) -> u `elem` tsks) es
  ]
  
-- Q2
q2 :: Q [Department]
q2 = expertise nestedOrg2 (toQ "abstract")

-- Q3
q3 :: Q [(Text, [Text])]
q3 = [ pair (e_empQ e) (tasksOfEmployee e)
     | e <- employees
     ]
                  
-- Q4
q4 :: Q [(Department, [Text])]
q4 = [ pair d [ e_empQ e | e <- employees, e_dptQ e == d ]
     | d <- departments ]
     
-- Q5
employeesByTask :: Q Task -> Q [(Text, Text)]
employeesByTask t = [ pair (e_empQ e) d
                    | e <- employees
                    , d <- departments
                    , e_empQ e == t_empQ t && e_dptQ e == d
                    ]
 
q5 :: Q [(Text, [(Text, Text)])]
q5 = [ pair (t_tskQ t) (employeesByTask t)
     | t <- tasks
     ]
   
-- Q6
outliersQ :: Q [(Text, [(Text, [Text])])]
outliersQ =
  [ let a = [ pair (e_empQ e)
                   [ t_tskQ t | t <- tasks, e_empQ e == t_empQ t ] 
            | e <- employees
            , d == e_dptQ e && (e_salaryQ e < 1000 || e_salaryQ e > 1000000)
            ]
        b = [ pair (c_dptQ c) (singleton (toQ "buy")) | c <- contacts, d == c_dptQ c, c_clientQ c ]
    in pair d (a ++ b)
  | d <- departments
  ]

isPoor :: Q (Text, Integer, [Text]) -> Q Bool
isPoor (view -> (_, sal, _)) = sal < 1000

isRich :: Q (Text, Integer, [Text]) -> Q Bool
isRich (view -> (_, sal, _)) = sal > 1000000
  
outliers :: Q [(Text, Integer, [Text])] -> Q [(Text, Integer, [Text])]
outliers xs = [ x | x <- xs, isRich x || isPoor x ]

clients :: Q [(Text, Bool)] -> Q [(Text, Bool)]
clients cs = [ c | c <- cs, snd c ]

get :: Q [(Text, Integer, [Text])] -> (Q (Text, Integer, [Text]) -> Q [Text]) -> Q [(Text, [Text])]
get emps f = [ pair d (f e) | e@(view -> (d, _, _)) <- emps ]

thd :: (QA a, QA b, QA c) => Q (a, b, c) -> Q c
thd (view -> (_, _, c)) = c

outliersFactored :: Q [NestedOrg] -> Q [(Text, [(Text, [Text])])]
outliersFactored orgs =
  [ let a = [ pair d cs | (view -> (d, _, cs)) <- outliers es ]
        b = [ pair (fst c) (singleton $ toQ "buy") | c <- clients cs ]
    in pair d (a ++ b)
  | (view -> (d, es, cs)) <- orgs ]
  
q6 :: Q [(Text, [(Text, [Text])])]
q6 = outliersFactored nestedOrg2

getConn :: IO X100Info
getConn = P.return $ x100Info "localhost" "48130" Nothing

getPGConn :: IO Connection
getPGConn = connectPostgreSQL "user = 'au' password = 'foobar' host = 'localhost' dbname = 'organisation16'"

allQueries :: IO ()
allQueries = getPGConn 
       P.>>= (\conn -> debugTAOpt "q1" conn q1
                       P.>> debugTAOpt "q2" conn q2
                       P.>> debugTAOpt "q3" conn q3
                       P.>> debugTAOpt "q4" conn q4
                       P.>> debugTAOpt "q5" conn q5
                       P.>> debugTAOpt "q6" conn q6)

someQuery :: IO ()
someQuery = getPGConn 
         -- P.>>= (\conn -> debugVLOpt "q62" conn q6)
         P.>>= (\conn -> debugTAOpt "q3" conn q3)

main :: IO ()
main = allQueries
