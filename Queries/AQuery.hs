{-# LANGUAGE OverloadedStrings #-}

module Queries.AQuery
    ( module Queries.AQuery.Trades
    , module Queries.AQuery.Packets
    , debugAll
    ) where

import qualified Data.Time.Calendar       as C

import           Database.DSH.Backend
import           Database.DSH.Compiler

import           Queries.AQuery.Packets
import           Queries.AQuery.Trades

debugAll :: Backend c => c -> IO ()
debugAll conn = do
    putStrLn "bestProfit"
    debugQ "bestprofit" conn $ bestProfit 614 (C.fromGregorian 2015 12 3)

    putStrLn "last10"
    debugQ "last10" conn $ last10 42

    putStrLn "flowStats drop"
    debugQ "flowstats_drop" conn flowStatsZip

    putStrLn "flowStats self"
    debugQ "flowstats_self" conn flowStatsSelfJoin

    putStrLn "flowStats win"
    debugQ "flowstats_win" conn flowStatsWin
