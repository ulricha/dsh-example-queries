module Queries.Shredding
    ( module Queries.Shredding.Nested
    , module Queries.Shredding.Complex
    , module Queries.Shredding.Grouping
    , debugAll
    ) where

import Database.DSH.Compiler
import Database.DSH.Backend

import Queries.Shredding.Nested
import Queries.Shredding.Complex
import Queries.Shredding.Grouping

debugAll :: Backend c => c -> IO ()
debugAll c = do
    debugQ "q1" c q1
    debugQ "q2" c q2
    debugQ "q3" c q3
    debugQ "q4" c q4
    debugQ "q5" c q5
    debugQ "q6" c q6

    debugQ "q1c" c q1c
    debugQ "q2c" c q2c
    debugQ "q3c" c q3c
    debugQ "q4c" c q4c
    debugQ "q5c" c q5c

    debugQ "q1g" c q1g
    debugQ "q2g" c q2g
