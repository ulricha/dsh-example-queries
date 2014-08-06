{-# LANGUAGE OverloadedStrings, MonadComprehensions, RebindableSyntax, ViewPatterns #-}

module Main where

import qualified Prelude as P
import Database.DSH
import Database.DSH.Compiler
import Database.HDBC.PostgreSQL

import Database.X100Client

-- Helper Functions and Queries

q1 :: Q [Integer]
q1 = [ x
     | x <- (toQ [1,2,3,4,5,6])
     , x > 3
     ]

q2 :: Q [(Integer, Integer)]
q2 = [ pair x y
     | (view -> (x, y)) <- toQ [(1,2), (2,3), (3,4), (5,6), (42,43)]
     , x == 3 || y > 40
     ]

getConn :: IO Connection
getConn = connectPostgreSQL "user = 'au' password = 'foobar' host = 'localhost' dbname = 'au'"

getConnX100 :: IO X100Info
getConnX100 = P.return $ x100Info "localhost" "48130" Nothing


main :: IO ()
main = getConnX100
       -- P.>>= (\conn -> runPrint conn q2 P.>> debugTAOpt "q2" conn q2)
       -- P.>>= (\conn -> debugX100 "q2" conn q2)
       P.>>= (\conn -> runQX100 conn q2 P.>>= \r -> putStrLn $ show r)
