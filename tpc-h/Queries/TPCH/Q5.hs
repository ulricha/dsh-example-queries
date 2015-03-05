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

-- TPC-H Q5

module Queries.TPCH.Q5
    ( q5
    ) where

import Database.DSH
import Schema.TPCH

q5 :: Q [(Text, Decimal)]
q5 =
  sortWith (\(view -> (_, r)) -> r * (-1)) $
  map (\(view -> (k, g)) -> pair k (sum [ e * (1 - d) | (view -> (_, e, d)) <- g ])) $
  groupWithKey (\(view -> (n, _, _)) -> n) $
  [ tup3 (n_nameQ n) (l_extendedpriceQ l) (l_discountQ l)
  | c <- customers
  , o <- orders
  , l <- lineitems
  , s <- suppliers
  , n <- nations
  , r <- regions
  , c_custkeyQ c == o_custkeyQ o
  , l_orderkeyQ l == o_orderkeyQ o
  , l_suppkeyQ l == s_suppkeyQ s
  , c_nationkeyQ c == s_nationkeyQ s
  , s_nationkeyQ s == n_nationkeyQ n
  , n_regionkeyQ n == r_regionkeyQ r
  , r_nameQ r == (toQ "ASIA")
  , o_orderdateQ o >= 42
  , o_orderdateQ o < 60
  ]
