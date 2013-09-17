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

q :: Q [(Integer, Integer)]
q = [ tuple2 x y
    | x <- (toQ [1,2,3,4,5,6])
    , x > 3
    , x < 6
    , y <- (toQ [2,3,4,5,6,7])
    , x == y
    ]

  
getConn :: IO X100Info
getConn = P.return $ x100Info "localhost" "48130" Nothing

main :: IO ()
main = getConn 
       -- P.>>= (\conn -> fromQX100 conn q P.>>= (\i -> putStrLn $ show i))
       P.>>= (\conn -> debugCLX100 conn q P.>>= putStrLn)
