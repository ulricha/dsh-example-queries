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

module Queries.TPCH.Q1
    ( q1
    ) where

import qualified Data.Time.Calendar as C
import Database.DSH
import Schema.TPCH
import Queries.TPCH.BuildingBlocks

withFlagStatus :: Q LineItem -> Q (Text, Text)
withFlagStatus li = tup2 (l_returnflagQ li) (l_linestatusQ li)

itemsBefore :: Q Day -> Q [LineItem]
itemsBefore maxDate = [ li | li <- lineitems , l_shipdateQ li <= maxDate ]

type PricingTotals = (Decimal, Decimal, Decimal, Decimal,
                      Decimal, Decimal, Decimal, Integer)

-- | TPC-H Query Q1. Validation parameter: DELTA = 90
q1 :: Integer -> Q [((Text, Text), PricingTotals)]
q1 delta = sortWith fst $
     [ pair k (tup8 (sum $ map l_quantityQ lis)
                    (sum $ map l_extendedpriceQ lis)
                    (sum $ map discPrice lis)
                    (sum $ map chargedPrice lis)
                    (avg $ map l_quantityQ lis)
                    (avg $ map l_extendedpriceQ lis)
                    (avg $ map l_discountQ lis)
                    (length lis))
      | (view -> (k, lis)) <- groupWithKey withFlagStatus (itemsBefore maxDate)
      ]

  where
    maxDate = subDays (toQ delta) (toQ $ C.fromGregorian 1998 12 1)
