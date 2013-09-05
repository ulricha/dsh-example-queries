-- 2c544f140614066b97b9e61030ce7179108e6702
-- optimization string: ESRSRS

{-# LANGUAGE OverloadedStrings, MonadComprehensions, RebindableSyntax, ViewPatterns #-}

module Main where

import qualified Prelude as P
import Database.DSH
import Database.DSH.Compiler
import Database.HDBC.PostgreSQL

import Debug.Trace

-- Helper Functions and Queries

q :: Q [(Integer, Integer, Integer)]
q = [ tuple3 x y z
    | x <- (toQ [1,2,3,4,5,6])
    , y <- (toQ [2,3,4,5,6,7])
    , y == x
    , z <- (toQ [10, 20, 30])
    , x > 3
    , y < 3
    ]

getConn :: IO Connection
getConn = connectPostgreSQL "user = 'au' password = 'foobar' host = 'localhost' dbname = 'foo'"

main :: IO ()
main = getConn P.>>= (\conn -> debugTA "join" conn q)
