{-# LANGUAGE OverloadedStrings #-}
module Queries.TPCHOther
    ( module Queries.TPCHOther.Jan.Q7
    , module Queries.TPCHOther.PendingProfit
    , getConn
    , debugAll
    ) where

import Data.Text
import Database.HDBC.PostgreSQL
import Database.DSH.Compiler

import Queries.TPCHOther.Jan.Q7
import Queries.TPCHOther.PendingProfit

getConn :: IO Connection
getConn = connectPostgreSQL "user = 'au' password = 'foobar' host = 'localhost' port = '5432' dbname = 'tpch'"

debugAll :: IO ()
debugAll = do
    c <- getConn
    putStrLn "Jan.Q7.a"
    debugQ "qjq7a" c jan_q7a

    putStrLn "Jan.Q7.b"
    debugQ "qjq7b" c jan_q7b

    putStrLn "PP"
    debugQ "qpp" c $ pendingProfit (pack "GERMANY")
