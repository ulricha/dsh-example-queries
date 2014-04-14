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
    
-- TPC-H Q21

module Main where

import qualified Data.Text as T

import qualified Prelude as P
import Database.DSH
import Database.DSH.Compiler

import Database.HDBC.PostgreSQL

import Records

q21 :: Text -> Q [(Text, Integer)]
q21 nation =
  sortWith (\(view -> (name, nw)) -> pair (-1 * nw) name) $
  map (\kg -> pair (fst kg) (length $ snd kg)) $
  groupWithKey id $
  [ s_nameQ s
  | s <- suppliers
  , l1 <- lineitems
  , o <- orders
  , n <- nations
  , s_suppkeyQ s == l_suppkeyQ l1
  , o_orderkeyQ o == l_orderkeyQ l1
  , o_orderstatusQ o == toQ "F"
  , l_receiptdateQ l1 > l_commitdateQ l1
  , not $ null [ 1 :: Q Integer
               | l2 <- lineitems
	       , l_orderkeyQ l2 == l_orderkeyQ l1
	       , l_suppkeyQ l2 /= l_suppkeyQ l1
	       ]
  , null [ 1 :: Q Integer
         | l3 <- lineitems
	 , l_orderkeyQ l3 == l_orderkeyQ l1
	 , l_suppkeyQ l3 == l_suppkeyQ l1
	 , l_receiptdateQ l3 > l_commitdateQ l3
	 ]
  ]
 
getConn :: IO Connection
getConn = connectPostgreSQL "user = 'au' password = 'foobar' host = 'localhost' port = '5432' dbname = 'tpch'"

debugQ :: (Show a, QA a) => String -> Q a -> IO ()
debugQ s q = getConn P.>>= \conn -> debugVLOpt s conn q

main :: IO ()
main = debugQ "q21" (q21 "SAUDI ARABIA")
