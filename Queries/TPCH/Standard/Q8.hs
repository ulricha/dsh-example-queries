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

module Queries.TPCH.Standard.Q8
    ( q8
    , q8Default
    ) where

import qualified Data.Time.Calendar  as C
import           Database.DSH
import           Queries.TPCH.BuildingBlocks
import           Schema.TPCH

revenueByNation :: Text -> Text -> Interval -> Q [(Integer, Decimal, Text)]
revenueByNation regionName typ interval =
    [ tup3 (dateYear $ o_orderdateQ o) (discPrice l) (n_nameQ n2)
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

-- | TPC-H Query Q8 with standard validation parameters
q8Default :: Q [(Integer, Decimal)]
q8Default = q8 "BRAZIL" "AMERICA" "ECONOMY ANODIZED STEEL"

-- | TPC-H Query Q8
q8 :: Text -> Text -> Text -> Q [(Integer, Decimal)]
q8 nationName regionName typ =
  sortWith fst
  [ tup2 y (nationVolumne nationName g / completeVolume g)
  | (view -> (y, g)) <- revenueByYear
  ]

  where
    revenueByYear = groupWithKey (\(view -> (y, _, _)) -> y)
                    $ revenueByNation regionName typ interval

    interval = Interval (C.fromGregorian 1995 1 1) (C.fromGregorian 1996 12 31)
