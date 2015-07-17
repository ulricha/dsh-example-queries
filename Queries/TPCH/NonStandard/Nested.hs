{-# LANGUAGE MonadComprehensions   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RebindableSyntax      #-}

-- | Queries with nested results over the TPC-H schema.
module Queries.TPCH.NonStandard.Nested where

import qualified Data.Time.Calendar  as C

import Database.DSH
import Schema.TPCH
import Queries.TPCH.BuildingBlocks

custFromOrders :: Text -> Q [(Text, [(Text, Decimal)])]
custFromOrders nationName =
    [ tup2 (c_nameQ c) [ tup2 (o_orderpriorityQ o) (o_totalpriceQ o) | o <- custOrders c]
    | c <- customers
    , c `custFromNation` nationName
    , any (\o -> o `orderedBy` c) orders 
    ]

--------------------------------------------------------------------------------

custRevenues :: Text -> Q [(Text, [Decimal])]
custRevenues nationName =
    [ tup2 (c_nameQ c)
           [ orderRevenue o | o <- orders, o `orderedBy` c ]
    | c <- customers
    , c `custFromNation` nationName
    , any (\o -> o `orderedBy` c) orders 
    ]

--------------------------------------------------------------------------------

-- | For each customer from a certain nation, compute the revenue to be expected
-- from pending orders.
expectedRevenueFor :: Text -> Q [(Text, [(Day, Decimal)])]
expectedRevenueFor nationName =
    [ pair (c_nameQ c) [ pair (o_orderdateQ o) (orderRevenue o)
                       | o <- ordersWithStatus "P"
                       , o `orderedBy` c
                       ]
    | c <- customers
    , c `custFromNation` nationName
    , any (\o -> o `orderedBy` c) (ordersWithStatus "P")
    ]

--------------------------------------------------------------------------------

-- | For each order, return the orders' lineitems sorted by shipping date and
-- compute the average shipping delay for the orders' items.
--
-- Note: This query is not selective enough to make much sense. It is meant as a
-- minimal example for a query that is nested, order-aware and uses aggregate
-- functions.
shippingDelay :: Q [(Integer, [Decimal], Double)]
shippingDelay =
    [ let ls = orderItems o
      in tup3 (o_orderkeyQ o)
              (map l_quantityQ $ sortWith l_shipdateQ ls)
              (avg [ integerToDouble $ diffDays (l_shipdateQ l) (o_orderdateQ o) | l <- ls ])
    | o <- orders
    ]

-- | Compute shipping delays (same as in the running example) for all orders
-- from a time interval.
shippingDelayInterval :: Q [(Integer, [Decimal], Double)]
shippingDelayInterval =
    [ let ls = orderItems o
      in tup3 (o_orderkeyQ o)
              (map l_quantityQ $ sortWith l_shipdateQ ls)
              (avg [ integerToDouble $ diffDays (l_shipdateQ l) (o_orderdateQ o) | l <- ls ])
    | o <- orders
    , o_orderdateQ o `inInterval` interval
    ]
  where
    startDate = C.fromGregorian 1993 7 1
    interval  = Interval startDate (C.addGregorianMonthsRollOver 3 startDate)

--------------------------------------------------------------------------------


