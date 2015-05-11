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
    , module Queries.TPCH.Q13
    , module Queries.TPCH.Q14
    , module Queries.TPCH.Q15
    , module Queries.TPCH.Q16
    , module Queries.TPCH.Q17
    , module Queries.TPCH.Q18
    , module Queries.TPCH.Q19
    , module Queries.TPCH.Q20
    , module Queries.TPCH.Q21
    , module Queries.TPCH.Q22
    , debugAll
    ) where

import Database.DSH.Compiler
import Database.DSH.Backend

import qualified Data.Time.Calendar as C
import qualified Data.Decimal as D

import Queries.TPCH.BuildingBlocks
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

debugAll :: Backend c => c -> IO ()
debugAll c = do
    putStrLn "Q1"
    debugQ "q1" c q1Default

    putStrLn "Q2"
    debugQ "q2" c q2Default

    putStrLn "Q2a"
    debugQ "q2a" c q2aDefault

    putStrLn "Q3"
    debugQ "q3" c q3Default

    putStrLn "Q3a"
    debugQ "q3a" c q3aDefault

    putStrLn "Q4"
    debugQ "q4" c q4Default

    putStrLn "Q4a"
    debugQ "q4a" c q4aDefault

    putStrLn "Q4b"
    debugQ "q4b" c q4bDefault

    putStrLn "Q5"
    debugQ "q5" c q5Default

    putStrLn "Q6"
    let discount = D.realFracToDecimal 2 0.06
    debugQ "q6" c $ q6 (C.fromGregorian 1994 1 1) discount 24

    putStrLn "Q7"
    debugQ "q7" c $ q7 "FRANCE" "GERMANY"

    putStrLn "Q8"
    debugQ "q8" c $ q8 "BRAZIL" "AMERICA" "ECONOMY ANODIZED STEEL"

    putStrLn "Q9"
    debugQ "q9" c q9

    putStrLn "Q10"
    debugQ "q10" c $ q10 (C.fromGregorian 1993 10 1)

    putStrLn "Q11"
    debugQ "q11" c $ q11 "GERMANY" 0.0001

    putStrLn "Q12"
    debugQ "q12" c $ q12 "MAIL" "SHIP" (C.fromGregorian 1994 1 1)

    putStrLn "Q13"
    debugQ "q13" c $ q13 "special" "requests"

    putStrLn "Q14"
    debugQ "q14" c $ q14 (C.fromGregorian 1995 9 1)

    putStrLn "Q15"
    debugQ "q15" c $ q15 (C.fromGregorian 1996 1 1)

    putStrLn "Q16"
    debugQ "q16" c q16

    putStrLn "Q17"
    debugQ "q17" c q17

    putStrLn "Q18"
    debugQ "q18" c $ q18 300

    putStrLn "Q19"
    debugQ "q19" c $ q19 1 10 20 "Brand#12" "Brand#23" "Brand#34"

    putStrLn "Q20"
    debugQ "q20" c $ q20 "forest" (C.fromGregorian 1994 1 1) "CANADA"

    putStrLn "Q21"
    debugQ "q21" c $ q21 "SAUDI ARABIA"

    putStrLn "Q22"
    debugQ "q22" c $ q22 ["13", "31", "23", "29", "30", "18", "17"]
