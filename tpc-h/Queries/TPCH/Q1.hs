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

import Database.DSH
import Schema.TPCH

withFlagStatus :: Q LineItem -> Q (Text, Text)
withFlagStatus li = tup2 (l_returnflagQ li) (l_linestatusQ li)

filteredItems :: Q [LineItem]
filteredItems = [ li | li <- lineitems , l_shipdateQ li <= 42 ]

fst9 :: (QA a, QA b, QA c, QA d, QA e, QA f, QA g, QA h, QA i) => Q (a, b, c, d, e, f, g, h, i) -> Q a
fst9 (view -> (a, _, _, _, _, _, _, _, _)) = a

q1 :: Q [((Text, Text), Decimal, Decimal, Decimal, Decimal, Decimal, Decimal, Decimal, Integer)]
q1 = sortWith fst9 $
     [ tup9
          k
          (sum $ map l_quantityQ lis)
          (sum $ map l_extendedpriceQ lis)
          (sum $ map (\li -> l_extendedpriceQ li * (1 - l_discountQ li)) lis)
	  (sum $ map (\li -> l_extendedpriceQ li * (1 - l_discountQ li) * (1 + l_taxQ li)) lis)
          (avg $ map l_quantityQ lis)
          (avg $ map l_extendedpriceQ lis)
          (avg $ map l_discountQ lis)
          (length lis)
      | (view -> (k, lis)) <- groupWithKey withFlagStatus filteredItems
      ]
