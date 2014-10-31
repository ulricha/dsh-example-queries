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
hasNationality c nationName = any (\n -> n_nameQ n == toQ nationName
                                         && 
                                         c_nationkeyQ c == n_nationkeyQ n) 
                                  nations

revenue :: Q Order -> Q Double
revenue o = sum [ l_extendedpriceQ l * (1 - l_discountQ l)
                | l <- lineitems
                , l_orderkeyQ l == o_orderkeyQ o
                ]

-- | For each customer of a given nationality, compute the pending
-- orders combined with the order revenue that could be obtained by
-- completing an order.
pendingProfit :: Text -> Q [(Text, [(Integer, Double)])]
pendingProfit nationName =
    [ tup2 (c_nameQ c) [ tup2 (o_orderdateQ o) (revenue o)
                       | o <- orders
                       , o_custkeyQ o == c_custkeyQ c
                       , o_orderstatusQ o == toQ "PENDING"
                       ]
    | c <- customers
    , c `hasNationality` nationName
    ]
