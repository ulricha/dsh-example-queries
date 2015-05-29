{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MonadComprehensions   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- TPC-H Q10

module Queries.TPCH.Q10
    ( q10
    , q10Default
    ) where

import qualified Data.Time.Calendar          as C
import           Database.DSH
import           Queries.TPCH.BuildingBlocks
import           Schema.TPCH

q10_join :: Day -> Q [((Integer, Text, Decimal, Text, Text, Text, Text), Decimal)]
q10_join startDate =
    [ pair (tup7 (c_custkeyQ c)
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
    , o_orderdateQ o `inInterval` interval
    , l_returnflagQ l == "R"
    , c_nationkeyQ c == n_nationkeyQ n
    ]

  where
    interval = Interval startDate (C.addGregorianMonthsRollOver 3 startDate)

-- | TPC-H Query Q10 with standard validation parameters
q10Default :: Q [((Integer, Text, Decimal, Text, Text, Text, Text), Decimal)]
q10Default = q10 (C.fromGregorian 1993 10 1)

-- | TPC-H Query Q10
q10 :: Day -> Q [((Integer, Text, Decimal, Text, Text, Text, Text), Decimal)]
q10 startDate =
    take 20 $ sortWith snd $ groupAggr fst snd sum (q10_join startDate)
