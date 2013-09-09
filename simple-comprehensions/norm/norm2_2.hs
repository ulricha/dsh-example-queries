-- 2c544f140614066b97b9e61030ce7179108e6702
-- optimization string: ESRSRS

{-# LANGUAGE OverloadedStrings, MonadComprehensions, RebindableSyntax, ViewPatterns #-}

module Main where

import qualified Prelude as P
import Database.DSH
import Database.DSH.Compiler
import Database.X100Client

import Debug.Trace

-- Helper Functions and Queries

q :: Q [Integer]
q = [ x + 2 + y
    | x <- (toQ [1,2,3,4,5,6])
    , x > 3
    , y <- (toQ [10])
    ]

  
getConn :: IO X100Info
getConn = P.return $ x100Info "localhost" "48130" Nothing

main :: IO ()
main = getConn 
       -- P.>>= (\conn -> fromQX100 conn q P.>>= (\i -> putStrLn $ show i))
       P.>>= (\conn -> debugX100VL "filter" conn q)
