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

-- TPC-H Q8

module Queries.TPCH.Q8
    ( q8
    ) where

import Database.DSH
import Schema.TPCH
import Queries.TPCH.Common

revenue :: Q LineItem -> Q Decimal
revenue l = l_extendedpriceQ l * (1 - l_discountQ l)

-- FIXME should extract the year
revenueByNation :: Text -> Text -> Interval -> Q [(Integer, Decimal, Text)]
revenueByNation regionName typ interval =
  [ tup3 (o_orderdateQ o) (revenue l) (n_nameQ n2)
  | p  <- parts
  , s  <- suppliers
  , l  <- lineitems
  , o  <- orders
  , c  <- customers
  , n1 <- nations
  , n2 <- nations
  , r  <- regions
  , p_partkeyQ p == l_partkeyQ l
  , s_suppkeyQ s == l_suppkeyQ l
  , l_orderkeyQ l == o_orderkeyQ o
  , o_custkeyQ o == c_custkeyQ c
  , c_nationkeyQ c == n_nationkeyQ n1
  , n_regionkeyQ n1 == r_regionkeyQ r
  , r_nameQ r == toQ regionName
  , s_nationkeyQ s == n_nationkeyQ n2
  , o_orderdateQ o `inInterval` interval
  , p_typeQ p == toQ typ
  ]

nationVolumne :: Text -> Q [(Integer, Decimal, Text)] -> Q Decimal
nationVolumne nationName salesInYear =
    sum [ if n == toQ nationName then v else 0
        | (view -> (_, v, n)) <- salesInYear
        ]

completeVolume :: Q [(Integer, Decimal, Text)] -> Q Decimal
completeVolume salesInYear =
    sum [ v | (view -> (_, v, _)) <- salesInYear ]

q8 :: Text -> Text -> Text -> Interval -> Q [(Integer, Decimal)]
q8 nationName regionName typ interval =
  sortWith fst
  [ tup2 y (nationVolumne nationName g / completeVolume g)
  | (view -> (y, g)) <- revenueByYear
  ]

  where
    revenueByYear = groupWithKey (\(view -> (y, _, _)) -> y)
                    $ revenueByNation regionName typ interval
