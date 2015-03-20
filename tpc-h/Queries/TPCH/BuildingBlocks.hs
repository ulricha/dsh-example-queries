{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax    #-}

module Queries.TPCH.BuildingBlocks
    ( hasNationality
    , custOrders
    , orderItems
    , revenue
    , orderRevenue
    , ordersWithStatus
    ) where


import qualified Prelude as P
import Database.DSH
import Schema.TPCH

orderItems :: Q Order -> Q [LineItem]
orderItems o = [ l | l <- lineitems, l_orderkeyQ l == o_orderkeyQ o ]

custOrders :: Q Customer -> Q [Order]
custOrders c = [ o | o <- orders, o_custkeyQ o == c_custkeyQ c ]

hasNationality :: Q Customer -> Text -> Q Bool
hasNationality c nn =
    or [ n_nameQ n == toQ nn && n_nationkeyQ n == c_nationkeyQ c
       | n <- nations
       ]

revenue :: Q LineItem -> Q Decimal
revenue l = l_extendedpriceQ l * (1 - l_discountQ l)

orderRevenue :: Q Order -> Q Decimal
orderRevenue o = sum [ revenue l
                | l <- lineitems
                , l_orderkeyQ l == o_orderkeyQ o
                ]

ordersWithStatus :: Text -> Q Customer -> Q [Order]
ordersWithStatus status c =
    [ o | o <- custOrders c, o_orderstatusQ o == toQ status ]
