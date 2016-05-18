{-# LANGUAGE MonadComprehensions   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ViewPatterns          #-}

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

shippingDelayAvg :: Q [(Integer, [(Integer, Day)])]
shippingDelayAvg =
    [ tup2 (o_orderkeyQ o)
           [ tup2 (l_linenumberQ l) (l_shipdateQ l ) | l <- sortWith ((`diffDays` o_orderdateQ o) . l_shipdateQ) ls ]
    | o <- orders
    , let ls = orderItems o
    , 5 < avg [ integerToDouble $ diffDays (l_shipdateQ l) (o_orderdateQ o) | l <- ls ]
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

-- | For all customers from one nation, collect all orders and their lineitems
-- that are pending.
unshippedItemsPerCustomer :: Text -> Q [(Text, [(Integer, [(Integer, Text, Decimal)])])]
unshippedItemsPerCustomer nationName =
    [ tup2 (c_nameQ c)
           [ tup2 (o_orderkeyQ o)
                  [ tup3 (l_linenumberQ l) (p_nameQ p) (l_quantityQ l)
                  | l <- orderItems o
                  , p <- parts
                  , p_partkeyQ p == l_partkeyQ l
                  ] 
           | o <- custOrders c
           , o_orderstatusQ o == "P"
           ]
    | c <- customers
    , custFromNation c nationName
    , c_custkeyQ c `elem` [ o_custkeyQ o | o <- orders, o_orderstatusQ o == "P" ]
    ]

--------------------------------------------------------------------------------

-- | For each supplier with a negative account balance from a certain region,
-- compute for each supplied part the suppliers that are cheaper.
cheaperSuppliersInRegion :: Text -> Q [(Text, [(Text, Decimal, [(Text, Decimal)])])]
cheaperSuppliersInRegion regionName =
    [ tup2 (s_nameQ s)
           [ tup3 (p_nameQ p) (ps_supplycostQ ps)
                  [ tup2 (s_nameQ s') (ps_supplycostQ ps')
                  | (view -> (s', ps')) <- partSuppliers p
                  , s_nationkeyQ s' `fromRegion` regionName
                  , s_suppkeyQ s' /= s_suppkeyQ s
                  , ps_supplycostQ ps' < ps_supplycostQ ps
                  ]
           | (view -> (p, ps)) <- supplierParts s
           ]
    | s <- suppliers
    , s_nationkeyQ s `fromRegion` regionName
    , s_acctbalQ s < 0
    ]

-- | For each supplier from a certain region with a below-average account
-- balance, compute for each supplied part the suppliers that are cheaper.
cheaperSuppliersInRegionAvg :: Text -> Q [(Text, [(Text, Decimal, [(Text, Decimal)])])]
cheaperSuppliersInRegionAvg regionName =
    [ tup2 (s_nameQ s)
           [ tup3 (p_nameQ p) (ps_supplycostQ ps)
                  [ tup2 (s_nameQ s') (ps_supplycostQ ps')
                  | (view -> (s', ps')) <- partSuppliers p
                  , s_nationkeyQ s' `fromRegion` regionName
                  , s_suppkeyQ s' /= s_suppkeyQ s
                  , ps_supplycostQ ps' < ps_supplycostQ ps
                  ]
           | (view -> (p, ps)) <- supplierParts s
           ]
    | s <- suppliers
    , s_nationkeyQ s `fromRegion` regionName
    , s_acctbalQ s < avg [ s_acctbalQ s'
                         | s' <- suppliers
                         , s_nationkeyQ s' `fromRegion` regionName
                         ]
    ]

-- | For each supplier from a certain region, compute for each supplied part the
-- suppliers that are cheaper and have a below-average account balance.
cheaperSuppliersInRegionAvg2 :: Text -> Q [(Text, [(Text, Decimal, [(Text, Decimal)])])]
cheaperSuppliersInRegionAvg2 regionName =
    [ tup2 (s_nameQ s)
           [ tup3 (p_nameQ p) (ps_supplycostQ ps)
                  [ tup2 (s_nameQ s') (ps_supplycostQ ps')
                  | (view -> (s', ps')) <- partSuppliers p
                  , s_nationkeyQ s' `fromRegion` regionName
                  , s_suppkeyQ s' /= s_suppkeyQ s
                  , ps_supplycostQ ps' < ps_supplycostQ ps
                  , s_acctbalQ s' < avg [ s_acctbalQ s''
                                       | s'' <- suppliers
                                       , s_nationkeyQ s'' `fromRegion` regionName
                                       ]
                  ]
           | (view -> (p, ps)) <- supplierParts s
           ]
    | s <- suppliers
    , s_nationkeyQ s `fromRegion` regionName
    ]

-- For each nation in a region, compute for each part the cheapest supplier
