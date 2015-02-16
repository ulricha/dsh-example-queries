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

module Queries.TPCHOther.PendingProfit where

import qualified Prelude as P
import Database.DSH
import Database.DSH.Compiler

import Database.HDBC.PostgreSQL

import Schema.TPCH

hasNationality :: Q Customer -> Text -> Q Bool
hasNationality c nn = 
    or [ n_nameQ n == toQ nn && n_nationkeyQ n == c_nationkeyQ c
       | n <- nations
       ]

ordersOf :: Q Customer -> Q [Order]
ordersOf c = [ o | o <- orders, o_custkeyQ o == c_custkeyQ c ]

ordersWithStatus :: Text -> Q Customer -> Q [Order]
ordersWithStatus status c =
    [ o | o <- ordersOf c, o_orderstatusQ o == toQ status ]

revenue :: Q Order -> Q Double
revenue o = sum [ l_extendedpriceQ l * (1 - l_discountQ l)
                | l <- lineitems
                , l_orderkeyQ l == o_orderkeyQ o
                ]

expectedRevenueFor :: Text -> Q [(Text, [(Integer, Double)])]
expectedRevenueFor nation =
    [ pair (c_nameQ c) [ pair (o_orderdateQ o) (revenue o)
                       | o <- ordersWithStatus "P" c ]
    | c <- customers
    , c `hasNationality` nation
    , or [ toQ True | _ <- ordersWithStatus "P" c ]
    ]
