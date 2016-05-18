{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE OverloadedStrings   #-}

-- | Top-K queries (flat and nested) over the TPC-H schema.
module Queries.TPCH.NonStandard.TopK where

import Prelude()
import Database.DSH
import Schema.TPCH

import Queries.TPCH.BuildingBlocks

-- | Report the top /k/ customers by account balance from some country.
topCustAcct :: Text -> Integer -> Q [(Text, Decimal)]
topCustAcct n k =
    topK k snd [ tup2 (c_nameQ c) (c_acctbalQ c)
               | c <- customers
               , c `custFromNation` n
               ]

-- | Task: For each customer from some nation, compute the k orders with the
-- most lineitems.
topOrdersPerCustPrice :: Text -> Integer -> Q [(Text, [Order])]
topOrdersPerCustPrice n k =
    [ tup2 (c_nameQ c) (topK k o_totalpriceQ (custOrders c))
    | c <- customers
    , c `custFromNation` n
    , c_custkeyQ c `elem` [ o_custkeyQ o | o <- orders ]
    ]

-- | Task: For each customer from some nation, compute the k orders with the
-- most lineitems.
topOrdersPerCust :: Text -> Integer -> Q [(Text, [Order])]
topOrdersPerCust n k =
    [ tup2 (c_nameQ c) (topK k (length . orderItems) (custOrders c))
    | c <- customers
    , c `custFromNation` n
    , c_custkeyQ c `elem` [ o_custkeyQ o | o <- orders ]
    ]

-- | For each customer from nation 'n', fetch the date of the 'k' orders
-- with the most lineitems.
topOrdersPerCust' :: Integer -> Text -> Q [(Text, [Day])]
topOrdersPerCust' k n =
    [ tup2 (c_nameQ c) (map o_orderdateQ $ topK k (length . orderItems) (custOrders c))
    | c <- customers
    , c `custFromNation` n
    , c_custkeyQ c `elem` [ o_custkeyQ o | o <- orders ]
    ]

-- | For each customer from nation 'n', fetch the date of the 'k' orders
-- with the most revenue.
topOrdersPerCust'' :: Integer -> Text -> Q [(Text, [Day])]
topOrdersPerCust'' k n =
    [ tup2 (c_nameQ c) (map o_orderdateQ $ topK k orderRevenue (custOrders c))
    | c <- customers
    , c `custFromNation` n
    , c_custkeyQ c `elem` [ o_custkeyQ o | o <- orders ]
    ]

-- | The top k customers from one given country (by number of orders)
topCustomers :: Integer -> Text -> Q [(Text, Decimal)]
topCustomers k n =
    [ pair (c_nameQ c) (c_acctbalQ c)
    | c <- topK k (length . custOrders)
                  [ c | c <- customers, c `custFromNation` n ]
    ]

-- | Pair the top k customers with the most orders from two countries
pairTopCustomers :: Integer -> Text -> Text -> Q [((Text, Decimal), (Text, Decimal))]
pairTopCustomers k n1 n2 = zip (topCustomers k n1) (topCustomers k n2)

-- | Fetch the top customers in one region (by number of orders)
regionsTopCustomers :: Text -> Integer -> Q [(Text, [Text])]
regionsTopCustomers rn k =
    [ pair (n_nameQ n)
           (map c_nameQ $ topK k (length . custOrders)
                                 [ c | c <- customers
                                 , c_nationkeyQ c == n_nationkeyQ n
                                 ])
    | r <- regions
    , r_nameQ r == toQ rn
    , n <- regionNations r
    ]

-- | Compute all parts for which the 5 cheapest suppliers are from a given
-- nation.
cheapestSuppliersFrom :: Text -> Q [Part]
cheapestSuppliersFrom nationName =
    [ p
    | p <- parts
    , all (supplierFromNation nationName . fst)
          $ take 5
          $ reverse
          $ sortWith (ps_supplycostQ . snd)
          $ partSuppliers p
    ]
