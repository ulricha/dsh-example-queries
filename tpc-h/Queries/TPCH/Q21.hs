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

module Queries.TPCH.Q21
    ( q21
    , q21'
    ) where

import qualified Data.Text as T

import qualified Prelude as P
import Database.DSH
import Database.DSH.Compiler

import Database.HDBC.PostgreSQL

import Schema.TPCH

q21 :: Text -> Q [(Text, Integer)]
q21 nation =
  sortWith (\(view -> (name, nw)) -> pair (-1 * nw) name) $
  map (\kg -> pair (fst kg) (length $ snd kg)) $
  groupWithKey id $
  [ s_nameQ s
  | s  <- suppliers
  , l1 <- lineitems
  , o  <- orders
  , n  <- nations
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
	 , l_suppkeyQ l3 /= l_suppkeyQ l1
	 , l_receiptdateQ l3 > l_commitdateQ l3
	 ]
  , s_nationkeyQ s == n_nationkeyQ n
  , n_nameQ n == toQ nation
  ]

-- Variant of TPC-H Q21 with explicit universal and existential quantifiers.
q21' :: Text -> Q [(Text, Integer)]
q21' nation =
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
  , any (\l2 -> l_orderkeyQ l2 == l_orderkeyQ l1 && l_suppkeyQ l2 /= l_suppkeyQ l1) 
        lineitems
  , all (\l3 -> not $ l_orderkeyQ l3 == l_orderkeyQ l1
                      && l_suppkeyQ l3 /= l_suppkeyQ l1
                      && l_receiptdateQ l3 > l_commitdateQ l3)
        lineitems
  , s_nationkeyQ s == n_nationkeyQ n
  , n_nameQ n == toQ nation
  ]
