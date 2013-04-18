-- This example was taken from the paper called "Comprehensive Comprehensions"
-- by Phil Wadler and Simon Peyton Jones

{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ViewPatterns        #-}

module Main where

import qualified Prelude as P
import Database.DSH
import Database.DSH.Compiler

import Database.HDBC.PostgreSQL

employees :: Q [(Text, Text, Integer)]
employees = toQ [
    ("Simon",  "MS",   80)
  , ("Erik",   "MS",   90)
  , ("Phil",   "Ed",   40)
  , ("Gordon", "Ed",   45)
  , ("Paul",   "Yale", 60)
  ]

salary :: Q (Text, Text, Integer) -> Q Integer
salary (view -> (_, _, s)) = s

department :: Q (Text, Text, Integer) -> Q Text
department (view -> (_, d, _)) = d

sumSalary :: Q [(Text, Text, Integer)] -> Q Integer
sumSalary es = sum $ map salary es

query :: Q [(Text, Integer)]
query = take 5 
	$ sortWith snd
	$ map (\(view -> (d, es)) -> tuple2 d (sumSalary es))
	$ groupWithKey department employees

getConn :: IO Connection
getConn = connectPostgreSQL "user = 'postgres' password = 'haskell98' host = 'localhost' dbname = 'ferry'"

main :: IO ()
main = getConn P.>>= (\conn -> (fromQ conn query) P.>>= (\result -> putStrLn $ show result))
