{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax    #-}

-- | Common building blocks for TPC-H queries
module Queries.TPCH.BuildingBlocks
    ( -- * Date intervals
      Interval(..)
    , inInterval
      -- * Various predicates
    , custFromNation
    , ordersWithStatus
      -- * One-to-many relationships along foreign keys
    , custOrders
    , orderItems
    , partSuppliers
    , regionNations
      -- * Price and revenue
    , discPrice
    , chargedPrice
    , revenue
    , orderRevenue
    ) where



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

custFromNation :: Q Customer -> Text -> Q Bool
custFromNation c nn =
    or [ n_nameQ n == toQ nn && n_nationkeyQ n == c_nationkeyQ c
       | n <- nations
       ]

--------------------------------------------------------------------------------
-- One-to-many relationships along foreign keys

orderItems :: Q Order -> Q [LineItem]
orderItems o = [ l | l <- lineitems, l_orderkeyQ l == o_orderkeyQ o ]

custOrders :: Q Customer -> Q [Order]
custOrders c = [ o | o <- orders, o_custkeyQ o == c_custkeyQ c ]

partSuppliers :: Q Part -> Q [(Supplier, PartSupp)]
partSuppliers p = [ tup2 s ps
                  | s <- suppliers
                  , ps <- partsupps
                  , ps_partkeyQ ps == p_partkeyQ p
                  , ps_suppkeyQ ps == s_suppkeyQ s
                  ]

regionNations :: Q Region -> Q [Nation]
regionNations r = [ n | n <- nations, n_regionkeyQ n == r_regionkeyQ r ]

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

