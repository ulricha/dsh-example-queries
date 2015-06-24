{-# LANGUAGE MonadComprehensions   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RebindableSyntax      #-}

-- | Queries with nested results over the TPC-H schema.
module Queries.TPCHOther.PendingProfit where

import Database.DSH
import Schema.TPCH
import Queries.TPCH.BuildingBlocks

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
shippingDelay :: Q [(Integer, [LineItem], Double)]
shippingDelay =
    [ let ls = orderItems o
      in tup3 (o_orderkeyQ o)
              (sortWith l_shipdateQ ls)
              (avg [ integerToDouble $ diffDays (o_orderdateQ o) (l_shipdateQ l) | l <- ls ])
    | o <- orders
    ]
