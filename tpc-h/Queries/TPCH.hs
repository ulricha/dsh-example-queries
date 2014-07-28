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
import Queries.TPCH.Q14
import Queries.TPCH.Q15
import Queries.TPCH.Q16
import Queries.TPCH.Q17
import Queries.TPCH.Q18
import Queries.TPCH.Q19
import Queries.TPCH.Q20
import Queries.TPCH.Q21
import Queries.TPCH.Q22

getConn :: IO Connection
getConn = connectPostgreSQL "user = 'au' password = 'foobar' host = 'localhost' port = '5432' dbname = 'tpch'"

debugAll :: IO ()
debugAll = do
    c <- getConn
    debugQ "q1" c q1
    debugQ "q2" c q2
    debugQ "q3" c q3
    debugQ "q4" c q4
    debugQ "q5" c q5
    debugQ "q6" c q6
    debugQ "q7" c q7
    debugQ "q8" c q8
    debugQ "q9" c q9
    debugQ "q10" c q10
    debugQ "q11" c $ q11 "GERMANY" 0.0001
    debugQ "q12" c $ q12 "MAIL" "SHIP" 0xdeadbeef
    -- debugQ "q13" c q13
    debugQ "q14" c $ q14 0xdeadbeef
    debugQ "q15" c $ q15 0xdeadbeef
    debugQ "q16" c q16
    debugQ "q17" c q17
    debugQ "q18" c $ q18 300
    debugQ "q19" c $ q19 1 10 20 "Brand#12" "Brand#23" "Brand#34"
    debugQ "q20" c $ q20 "forest" 0xdeadbeef "CANADA"
    debugQ "q21" c $ q21 "SAUDI ARABIA"
    debugQ "q22" c $ q22 ["13", "31", "23", "29", "30", "18", "17"]
