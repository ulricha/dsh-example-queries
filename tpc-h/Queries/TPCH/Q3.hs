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

-- TPC-H Q3

module Queries.TPCH.Q3
    ( q3
    ) where

import Database.DSH
import Schema.TPCH

project
  :: Q ((Integer, Day, Integer), [((Integer, Day, Integer), (Decimal, Decimal))])
  -> Q ((Integer, Day, Integer), Decimal)
project gk = pair (fst gk) revenue
  where
    revenue = sum [ ep * (1 - d) | (view -> (ep, d)) <- [ snd x | x <- snd gk ] ]

byRevDate :: Q ((Integer, Day, Integer), Decimal) -> Q (Decimal, Integer)
byRevDate (view -> (((view -> (_, _, sp)), r))) = pair (r * (-1)) sp

q3 :: Day -> Text -> Q [((Integer, Day, Integer), Decimal)]
q3 date marketSegment =
  sortWith byRevDate $
  map project $
  groupWithKey fst $
  [ let sep = tup3 (l_orderkeyQ l) (o_orderdateQ o) (o_shippriorityQ o)
    in pair sep (pair (l_extendedpriceQ l) (l_discountQ l))
  | c <- customers
  , o <- orders
  , l <- lineitems
  , c_mktsegmentQ c == toQ marketSegment
  , c_custkeyQ c == o_custkeyQ o
  , l_orderkeyQ l == o_orderkeyQ o
  , o_orderdateQ o < toQ date
  , l_shipdateQ l > toQ date
  ]
