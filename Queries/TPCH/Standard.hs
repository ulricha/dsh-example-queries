{-# LANGUAGE OverloadedStrings #-}

-- | Benchmark queries as defined in the TPC-H benchmark specification.
module Queries.TPCH.Standard
    ( module Queries.TPCH.Standard.Q1
    , module Queries.TPCH.Standard.Q2
    , module Queries.TPCH.Standard.Q3
    , module Queries.TPCH.Standard.Q4
    , module Queries.TPCH.Standard.Q5
    , module Queries.TPCH.Standard.Q6
    , module Queries.TPCH.Standard.Q7
    , module Queries.TPCH.Standard.Q8
    , module Queries.TPCH.Standard.Q9
    , module Queries.TPCH.Standard.Q10
    , module Queries.TPCH.Standard.Q11
    , module Queries.TPCH.Standard.Q12
    , module Queries.TPCH.Standard.Q13
    , module Queries.TPCH.Standard.Q14
    , module Queries.TPCH.Standard.Q15
    , module Queries.TPCH.Standard.Q16
    , module Queries.TPCH.Standard.Q17
    , module Queries.TPCH.Standard.Q18
    , module Queries.TPCH.Standard.Q19
    , module Queries.TPCH.Standard.Q20
    , module Queries.TPCH.Standard.Q21
    , module Queries.TPCH.Standard.Q22
    , debugAll
    ) where

import Database.DSH.Compiler
import Database.DSH.Backend

import Queries.TPCH.Standard.Q1
import Queries.TPCH.Standard.Q2
import Queries.TPCH.Standard.Q3
import Queries.TPCH.Standard.Q4
import Queries.TPCH.Standard.Q5
import Queries.TPCH.Standard.Q6
import Queries.TPCH.Standard.Q7
import Queries.TPCH.Standard.Q8
import Queries.TPCH.Standard.Q9
import Queries.TPCH.Standard.Q10
import Queries.TPCH.Standard.Q11
import Queries.TPCH.Standard.Q12
import Queries.TPCH.Standard.Q13
import Queries.TPCH.Standard.Q14
import Queries.TPCH.Standard.Q15
import Queries.TPCH.Standard.Q16
import Queries.TPCH.Standard.Q17
import Queries.TPCH.Standard.Q18
import Queries.TPCH.Standard.Q19
import Queries.TPCH.Standard.Q20
import Queries.TPCH.Standard.Q21
import Queries.TPCH.Standard.Q22

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
    debugQ "q6" c q6Default

    putStrLn "Q7"
    debugQ "q7" c q7Default

    putStrLn "Q7a"
    debugQ "q7a" c q7aDefault

    putStrLn "Q8"
    debugQ "q8" c q8Default

    putStrLn "Q9"
    debugQ "q9" c q9Default

    putStrLn "Q10"
    debugQ "q10" c q10Default

    putStrLn "Q11"
    debugQ "q11" c q11Default

    putStrLn "Q12"
    debugQ "q12" c q12Default

    putStrLn "Q12a"
    debugQ "q12a" c q12aDefault

    putStrLn "Q13"
    debugQ "q13" c q13Default

    putStrLn "Q14"
    debugQ "q14" c q14Default

    putStrLn "Q14a"
    debugQ "q14a" c q14aDefault

    putStrLn "Q15"
    debugQ "q15" c q15Default

    putStrLn "Q16"
    debugQ "q16" c q16

    putStrLn "Q17"
    debugQ "q17" c q17

    putStrLn "Q18"
    debugQ "q18" c q18Default

    putStrLn "Q19"
    debugQ "q19" c q19Default

    putStrLn "Q19a"
    debugQ "q19a" c q19aDefault

    putStrLn "Q20"
    debugQ "q20" c q20Default

    putStrLn "Q21"
    debugQ "q21" c q21Default

    putStrLn "Q21a"
    debugQ "q21a" c q21aDefault

    putStrLn "Q21b"
    debugQ "q21b" c q21bDefault

    putStrLn "Q21c"
    debugQ "q21c" c q21cDefault

    putStrLn "Q22"
    debugQ "q22" c q22Default
