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

-- TPC-H Q14

module Queries.TPCH.Q14
    ( q14
    , q14'
    ) where

import Database.DSH
import Schema.TPCH

revenue :: Q Decimal -> Q Decimal -> Q Decimal
revenue ep dis = ep * (1 - dis)

itemPrices :: Integer -> Q [(Text, Decimal, Decimal)]
itemPrices date =
  [ tup3 (p_typeQ p) (l_extendedpriceQ l) (l_discountQ l)
  | l <- lineitems
  , p <- parts
  , l_partkeyQ l == p_partkeyQ p
  , l_shipdateQ l >= toQ date
  , l_shipdateQ l < toQ date + 23
  ]

-------------------------------------------------------------------------------
-- Literal transcription of the TPC-H benchmark query

q14 :: Integer -> Q Decimal
q14 date = 100.0 * promoRev / totalRev
  where

    promoRev = sum [ if ty `like` "PROMO%"
                     then revenue ep discount
                     else 0
                   | (view -> (ty, ep, discount)) <- itemPrices date
                   ]

    totalRev = sum $ map (\(view -> (_, ep, d)) -> revenue ep d) $ itemPrices date

-------------------------------------------------------------------------------
-- Variation which uses a subquery to reduce the number of tuples that go
-- into the aggregate. This formulation might be beneficial if the p_type 
-- predicate can be pushed to an index.

q14' :: Integer -> Q Decimal
q14' date = 100.0 * integerToDecimal promoRev / totalRev
  where
    promoRev = length [ revenue ep dis
                      | (view -> (ty, ep, dis)) <- itemPrices date
                      , ty `like` "PROMO%"
                      ]

    totalRev = sum $ map (\(view -> (_, ep, d)) -> revenue ep d) $ itemPrices date
