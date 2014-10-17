{-# LANGUAGE OverloadedStrings #-}
module Queries.AQuery
    ( module Queries.AQuery.Trades
    , module Queries.AQuery.Packets
    , debugAll
    , getConn
    ) where

import Data.Text
import Database.HDBC.PostgreSQL
import Database.DSH.Compiler
import Queries.AQuery.Trades
import Queries.AQuery.Packets

getConn :: IO Connection
getConn = connectPostgreSQL "user = 'au' password = 'foobar' host = 'localhost' port = '5432' dbname = 'au'"

debugAll :: IO ()
debugAll = do
    c <- getConn

    putStrLn "bestProfit"
    debugQ "bestprofit" c $ bestProfit "foo" 42

    putStrLn "last10"
    debugQ "last10" c $ last10 42

    putStrLn "flowStats drop"
    debugQ "flowstats_drop" c $ flowStatsZip

    putStrLn "flowStats self"
    debugQ "flowstats_self" c $ flowStatsSelfJoin

    putStrLn "flowStats win"
    debugQ "flowstats_win" c $ flowStatsWin
