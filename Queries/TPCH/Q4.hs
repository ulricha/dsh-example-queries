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

-- TPC-H Q4

module Queries.TPCH.Q4
    ( q4
    , q4a
    , q4b
    , q4Default
    , q4aDefault
    , q4bDefault
    ) where

import qualified Data.Time.Calendar  as C
import           Database.DSH
import           Queries.TPCH.BuildingBlocks
import           Schema.TPCH

--------------------------------------------------------------------------------

q4Default :: Q [(Text, Integer)]
q4Default = q4 $ C.fromGregorian 1993 7 1

q4aDefault :: Q [(Text, Integer)]
q4aDefault = q4a $ C.fromGregorian 1993 7 1

q4bDefault :: Q [(Text, Integer)]
q4bDefault = q4b $ C.fromGregorian 1993 7 1

--------------------------------------------------------------------------------

-- | Is at least one of the orders' items overdue?
hasOverdueItem :: Q Order -> Q Bool
hasOverdueItem o = or [ l_commitdateQ l < l_receiptdateQ l
                      | l <- lineitems
                      , l_orderkeyQ l == o_orderkeyQ o
                      ]

-- | An order is problematic if at least one of its items was overdue.
problematicOrders :: Interval -> Q [Text]
problematicOrders interval =
    [ o_orderpriorityQ o
    | o <- orders
    , o_orderdateQ o `inInterval` interval
    , hasOverdueItem o
    ]

-- | TPC-H Query Q4 (abstraction-heavy DSH style)
-- Compute the number of problematic orders per priority level.
q4 :: Day -> Q [(Text, Integer)]
q4 startDate =
    sortWith fst [ tup2 op (length g) | (view -> (op, g)) <- groupWithKey id oids]
  where
    interval = Interval startDate (C.addDays 90 startDate)
    oids     = problematicOrders interval

--------------------------------------------------------------------------------

-- | TPC-H Query Q4 (literal transcription with 'null')
q4a :: Day -> Q [(Text, Integer)]
q4a startDate =
  sortWith fst
  $ map (\(view -> (k, g)) -> pair k (length g))
  $ groupWithKey id
    [ o_orderpriorityQ o
    | o <- orders
    , o_orderdateQ o >= toQ startDate
    , o_orderdateQ o < toQ (C.addDays 90 startDate)
    , not $ null [ toQ ()
                 | l <- lineitems
                 , l_orderkeyQ l == o_orderkeyQ o
                 , l_commitdateQ l < l_receiptdateQ l
                 ]
    ]

--------------------------------------------------------------------------------

-- | TPC-H Query Q4 (literal transcription with 'any')
q4b :: Day -> Q [(Text, Integer)]
q4b startDate =
  sortWith fst
  $ map (\(view -> (k, g)) -> pair k (length g))
  $ groupWithKey id
    [ o_orderpriorityQ o
    | o <- orders
    , o_orderdateQ o >= toQ startDate
    , o_orderdateQ o < toQ (C.addDays 90 startDate)
    , any (\l -> l_commitdateQ l < l_receiptdateQ l
                 && l_orderkeyQ l == o_orderkeyQ o)
          lineitems
    ]
