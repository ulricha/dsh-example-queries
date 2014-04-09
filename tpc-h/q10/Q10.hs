{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MonadComprehensions   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
    
-- TPC-H Q9

module Main where

import qualified Prelude as P
import Database.DSH
import Database.DSH.Compiler

import Database.HDBC.PostgreSQL

import Records

q10_join :: Q [((Integer, Text, Double, Text, Text, Text, Text), Double)]
q10_join = 
  [ pair (tuple7 (c_custkeyQ c)
  		 (c_nameQ c)
		 (c_acctbalQ c)
		 (c_phoneQ c)
		 (n_nameQ n)
		 (c_addressQ c)
		 (c_commentQ c))
         (l_extendedpriceQ l * (1 - l_discountQ l))
  | c <- customers
  , o <- orders
  , l <- lineitems
  , n <- nations
  , c_custkeyQ c == o_custkeyQ o
  , l_orderkeyQ l == o_orderkeyQ o
  , o_orderdateQ o >= 23
  , o_orderdateQ o < 42 + 3
  , l_returnflagQ l == "R"
  , c_nationkeyQ c == n_nationkeyQ n
  ]

q10 :: Q [((Integer, Text, Double, Text, Text, Text, Text), Double)]
q10 = 
  sortWith snd $
  [ pair k (sum $ map snd g)
  | (view -> (k, g)) <- groupWithKey fst q10_join
  ]

getConn :: IO Connection
getConn = connectPostgreSQL "user = 'au' password = 'foobar' host = 'localhost' port = '5432' dbname = 'tpch'"

debugQ :: (Show a, QA a) => String -> Q a -> IO ()
debugQ s q = getConn P.>>= \conn -> debugTAOpt s conn q

main :: IO ()
main = debugQ "q10" q10