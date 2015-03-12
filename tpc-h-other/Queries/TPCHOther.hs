{-# LANGUAGE OverloadedStrings #-}
module Queries.TPCHOther
    ( module Queries.TPCHOther.Jan.Q7
    , module Queries.TPCHOther.PendingProfit
    , module Queries.TPCHOther.TopK
    , getConn
    , debugAll
    ) where

import Data.Text(pack)
import Control.Applicative

import Database.HDBC.PostgreSQL
import Database.DSH.Compiler
import Database.DSH.Backend.Sql

import Queries.TPCHOther.Jan.Q7
import Queries.TPCHOther.PendingProfit
import Queries.TPCHOther.TopK

getConn :: IO Connection
getConn = connectPostgreSQL "user = 'au' password = 'foobar' host = 'localhost' port = '5432' dbname = 'tpchsmall'"

debugAll :: IO ()
debugAll = do
    c <- sqlBackend <$> getConn
    putStrLn "Jan.Q7.a"
    debugQ "qjq7a" c jan_q7a

    putStrLn "Jan.Q7.b"
    debugQ "qjq7b" c jan_q7b

    putStrLn "PP"
    debugQ "qpp" c $ expectedRevenueFor (pack "GERMANY")
