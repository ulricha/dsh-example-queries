{-# LANGUAGE OverloadedStrings #-}
module Queries.TPCH
    ( module Queries.TPCH.Q1
    , module Queries.TPCH.Q2
    , module Queries.TPCH.Q3
    , module Queries.TPCH.Q4
    , module Queries.TPCH.Q5
    , module Queries.TPCH.Q6
    , module Queries.TPCH.Q7
    , module Queries.TPCH.Q8
    , module Queries.TPCH.Q9
    , module Queries.TPCH.Q10
    , module Queries.TPCH.Q11
    , module Queries.TPCH.Q12
    , module Queries.TPCH.Q14
    , module Queries.TPCH.Q15
    , module Queries.TPCH.Q16
    , module Queries.TPCH.Q17
    , module Queries.TPCH.Q18
    , module Queries.TPCH.Q19
    , module Queries.TPCH.Q20
    , module Queries.TPCH.Q21
    , module Queries.TPCH.Q22
    , getConn
    ) where

import Data.Text
import Database.HDBC.PostgreSQL
import Database.DSH.Compiler
import Database.DSH.Backend.Sql

import Queries.TPCH.Q1
import Queries.TPCH.Q2
import Queries.TPCH.Q3
import Queries.TPCH.Q4
import Queries.TPCH.Q5
import Queries.TPCH.Q6
import Queries.TPCH.Q7
import Queries.TPCH.Q8
import Queries.TPCH.Q9
import Queries.TPCH.Q10
import Queries.TPCH.Q11
import Queries.TPCH.Q12
import Queries.TPCH.Q13
import Queries.TPCH.Q14
import Queries.TPCH.Q15
import Queries.TPCH.Q16
import Queries.TPCH.Q17
import Queries.TPCH.Q18
import Queries.TPCH.Q19
import Queries.TPCH.Q20
import Queries.TPCH.Q21
import Queries.TPCH.Q22

import Queries.TPCH.Common

getConn :: IO SqlBackend
getConn = do
    c <- connectPostgreSQL connString
    return $ sqlBackend c
  where
    connString = "user = 'au' password = 'foobar' host = 'localhost' port = '5432' dbname = 'tpch'"

debugAll :: IO ()
debugAll = do
    c <- getConn
    putStrLn "Q1"
    debugQ "q1" c q1

    putStrLn "Q2"
    debugQ "q2" c q2

    putStrLn "Q3"
    debugQ "q3" c q3

    putStrLn "Q4"
    debugQ "q4" c (q4'' $ Interval 42 47)

    putStrLn "Q5"
    debugQ "q5" c q5

    putStrLn "Q6"
    debugQ "q6" c q6

    putStrLn "Q7"
    debugQ "q7" c q7

    putStrLn "Q8"
    debugQ "q8" c $ q8 "BRAZIL" "AMERICA" "ECONOMY ANODIZED STELL" (Interval 42 57)

    putStrLn "Q9"
    debugQ "q9" c q9

    putStrLn "Q10"
    debugQ "q10" c q10

    putStrLn "Q11"
    debugQ "q11" c $ q11 "GERMANY" 0.0001

    putStrLn "Q12"
    debugQ "q12" c $ q12 "MAIL" "SHIP" 0xdeadbeef

    putStrLn "Q13"
    debugQ "q13" c q13

    putStrLn "Q14"
    debugQ "q14" c $ q14 0xdeadbeef

{-
    putStrLn "Q15"
    debugQ "q15" c $ q15 0xdeadbeef
-}

    putStrLn "Q16"
    debugQ "q16" c q16

    putStrLn "Q17"
    debugQ "q17" c q17

    putStrLn "Q18"
    debugQ "q18" c $ q18 300

    putStrLn "Q19"
    debugQ "q19" c $ q19 1 10 20 "Brand#12" "Brand#23" "Brand#34"

    putStrLn "Q20"
    debugQ "q20" c $ q20 "forest" (Interval 42 57) "CANADA"

    putStrLn "Q21"
    debugQ "q21" c $ q21 "SAUDI ARABIA"

    putStrLn "Q22"
    debugQ "q22" c $ q22 ["13", "31", "23", "29", "30", "18", "17"]
