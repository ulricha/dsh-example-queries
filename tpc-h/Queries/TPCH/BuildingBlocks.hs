{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax    #-}

-- | Common building blocks for TPC-H queries
module Queries.TPCH.BuildingBlocks
    ( -- * Date intervals
      Interval(..)
    , inInterval
      -- * Various predicates
    , hasNationality
    , ordersWithStatus
      -- * One-to-many relationships along foreign keys
    , custOrders
    , orderItems
      -- * Price and revenue
    , discPrice
    , chargedPrice
    , revenue
    , orderRevenue
    ) where


import qualified Prelude as P
import Database.DSH
import Schema.TPCH

--------------------------------------------------------------------------------
-- Date intervals

data Interval = Interval { iv_start :: Day, iv_end :: Day }

inInterval :: Q Day -> Interval -> Q Bool
inInterval d interval = d >= toQ (iv_start interval) && d < toQ (iv_end interval)

--------------------------------------------------------------------------------
-- Various predicates

ordersWithStatus :: Text -> Q Customer -> Q [Order]
ordersWithStatus status c =
    [ o | o <- custOrders c, o_orderstatusQ o == toQ status ]

hasNationality :: Q Customer -> Text -> Q Bool
hasNationality c nn =
    or [ n_nameQ n == toQ nn && n_nationkeyQ n == c_nationkeyQ c
       | n <- nations
       ]

--------------------------------------------------------------------------------
-- One-to-many relationships along foreign keys

orderItems :: Q Order -> Q [LineItem]
orderItems o = [ l | l <- lineitems, l_orderkeyQ l == o_orderkeyQ o ]

custOrders :: Q Customer -> Q [Order]
custOrders c = [ o | o <- orders, o_custkeyQ o == c_custkeyQ c ]


--------------------------------------------------------------------------------
-- Price and revenue

discPrice :: Q LineItem -> Q Decimal
discPrice l = l_extendedpriceQ l * (1 - l_discountQ l)

chargedPrice :: Q LineItem -> Q Decimal
chargedPrice l = discPrice l * (1 + l_taxQ l)

revenue :: Q [LineItem] -> Q Decimal
revenue ls = sum $ map discPrice ls

orderRevenue :: Q Order -> Q Decimal
orderRevenue o = revenue $ orderItems o

