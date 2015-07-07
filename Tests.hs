{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.DeepSeq

import qualified Database.DSH as Q
import Database.DSH.Compiler
import Database.DSH.Backend.Sql
import Database.DSH.Backend
import Database.HDBC.ODBC
import Queries.Shredding.Paper
import Schema.Shredding

conn :: IO SqlBackend
conn = sqlBackend <$> connectODBC "DSN=organisation4096"

main :: IO ()
main = do
    c <- conn
    -- r <- runQ c q3
    -- r <- runQ c $ Q.map c_nameQ contacts
    r <- runQ c q1
    deepseq r $ putStrLn $ show $ length r
