{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Database.DSH.Compiler
import Database.DSH.Backend.Sql
import Database.DSH.Backend
import Database.HDBC.ODBC
import Queries.TPCH.Standard
import Queries.TPCH.BuildingBlocks
import Schema.TPCH

conn :: IO SqlBackend
conn = sqlBackend <$> connectODBC "DSN=tpch1"

runQ8 :: SqlBackend -> IO ()
runQ8 c = debugQ "q2" c q8Default

main :: IO ()
main = do
    c <- conn
    debugAll c
