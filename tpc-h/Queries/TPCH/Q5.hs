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
    , q5Default
    ) where

import qualified Data.Time.Calendar as C
import           Database.DSH
import           Schema.TPCH

-- | TPC-H Query Q5 with standard validation parameters
q5Default :: Q [(Text, Decimal)]
q5Default = q5 (C.fromGregorian 1994 1 1) "ASIA"

-- | TPC-H Query Q5
q5 :: Day -> Text -> Q [(Text, Decimal)]
q5 startDate regionName =
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
  , r_nameQ r == toQ regionName
  , o_orderdateQ o >= toQ startDate
  , o_orderdateQ o < toQ (C.addDays 365 startDate)
  ]
