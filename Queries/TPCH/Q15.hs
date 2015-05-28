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

-- | TPC-H Q15
module Queries.TPCH.Q15
    ( q15
    , q15Default
    ) where

import qualified Data.Time.Calendar          as C
import           Database.DSH
import           Queries.TPCH.BuildingBlocks
import           Schema.TPCH

-- | For each supplier, compute the revenue in a 90 day interval
-- specified by the start date
revenueInInterval :: Day -> Q [(Integer, Decimal)]
revenueInInterval startDate =
    groupAggr l_suppkeyQ discPrice sum
    $ filter (\l -> inInterval (l_shipdateQ l) (intervalFrom startDate 90))
             lineitems

-- | TPC-H Query Q15 with standard validation parameters
q15Default :: Q [(Integer, (Text, Text, Text, Decimal))]
q15Default = q15 (C.fromGregorian 1996 1 1)

-- | TPC-H Query Q15
q15 :: Day -> Q [(Integer, (Text, Text, Text, Decimal))]
q15 startDate =
    sortWith fst
    [ pair (s_suppkeyQ s)
           (tup4 (s_nameQ s)
           (s_addressQ s)
           (s_phoneQ s)
           total_rev)
    | s <- suppliers
    , (view -> (supplier_no, total_rev)) <- revenueInInterval startDate
    , s_suppkeyQ s == supplier_no
    , total_rev == (maximum $ map snd $ revenueInInterval startDate)
    ]
