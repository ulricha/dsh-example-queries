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
    , q3a
    , q3Default
    , q3aDefault
    ) where

import qualified Data.Time.Calendar as C

import Database.DSH
import Schema.TPCH
import Queries.TPCH.BuildingBlocks

--------------------------------------------------------------------------------

-- | TPC-H Query Q3 with standard validation parameters
q3Default :: Q [((Integer, Day, Integer), Decimal)]
q3Default = q3 (C.fromGregorian 1995 3 15) "BUILDING"

-- | TPC-H Query Q3 with standard validation parameters (alternative formulation)
q3aDefault :: Q [((Integer, Day, Integer), Decimal)]
q3aDefault = q3 (C.fromGregorian 1995 3 15) "BUILDING"

--------------------------------------------------------------------------------

byRevDate :: Q ((Integer, Day, Integer), Decimal) -> Q (Decimal, Integer)
byRevDate (view -> (((view -> (_, _, sp)), r))) = pair (r * (-1)) sp

--------------------------------------------------------------------------------

unshippedOrders :: Day -> Q [(Order, [LineItem])]
unshippedOrders date =
    [ pair o [ l | l <- orderItems o, l_shipdateQ l > toQ date ]
    | o <- orders
    , o_orderdateQ o < toQ date
    ]

unshippedInMktSeg :: Text -> Day -> Q [((Integer, Day, Integer), LineItem)]
unshippedInMktSeg mktSeg date =
    [ pair (tup3 (l_orderkeyQ l) (o_orderdateQ o) (o_shippriorityQ o)) l
    | c <- customers
    , c_mktsegmentQ c == toQ mktSeg
    , (view -> (o, ls)) <- unshippedOrders date
    , o_custkeyQ o `elem` map o_custkeyQ (custOrders c)
    , l <- ls
    ]

-- | TPC-H Query Q3.
-- Validation parameters: SEGMENT = "BUILDING", DATE = '1995-03-15'
q3 :: Day -> Text -> Q [((Integer, Day, Integer), Decimal)]
q3 date mktSeg =
    take 10
    $ sortWith byRevDate
      [ pair k (revenue $ map snd g)
      | (view -> (k, g)) <- groupWithKey fst (unshippedInMktSeg mktSeg date)
      ]

--------------------------------------------------------------------------------

project
  :: Q ((Integer, Day, Integer), [((Integer, Day, Integer), (Decimal, Decimal))])
  -> Q ((Integer, Day, Integer), Decimal)
project gk = pair (fst gk) revenue
  where
    revenue = sum [ ep * (1 - d) | (view -> (ep, d)) <- [ snd x | x <- snd gk ] ]


-- | A rather literal transcription of TPC-H Query Q3.
-- Validation parameters: SEGMENT = "BUILDING", DATE = '1995-03-15'
q3a :: Day -> Text -> Q [((Integer, Day, Integer), Decimal)]
q3a date marketSegment =
  take 10 $
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
