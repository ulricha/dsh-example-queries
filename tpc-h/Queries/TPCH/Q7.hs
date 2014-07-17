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
    
-- TPC-H Q7

module Queries.TPCH.Q7
    ( q7
    ) where

import qualified Prelude as P
import Database.DSH
import Database.DSH.Compiler

import Database.HDBC.PostgreSQL

import Schema.TPCH

between :: Q Integer -> Q Integer -> Q Integer -> Q Bool
between x l r = x >= l && x <= r

q7 = 
  sortWith fst $
  map (\(view -> (k, g)) -> pair k (sum $ [ v | (view -> (_, _, _, v)) <- g])) $
  groupWithKey (\(view -> (n1, n2, y, v)) -> tuple3 n1 n2 y) $
  [ xs
  | xs <- [ tuple4 (n_nameQ n1) 
                   (n_nameQ n2) 
                   (l_shipdateQ l)
                   (l_extendedpriceQ l * (1 - l_discountQ l))
          | s <- suppliers
          , l <- lineitems
          , o <- orders
          , c <- customers
          , n1 <- nations
          , n2 <- nations
          , s_suppkeyQ s == l_suppkeyQ l
          , o_orderkeyQ o == l_orderkeyQ l
          , c_custkeyQ c == o_custkeyQ o
          , s_nationkeyQ s == n_nationkeyQ n1
          , c_nationkeyQ c == n_nationkeyQ n2
          , (n_nameQ n1 == "FRANCE" && n_nameQ n2 == "GERMANY")
            || (n_nameQ n1 == "GERMANY" && n_nameQ n2 == "FRANCE")
          , between (l_shipdateQ l) 23 42
          ]
  ]
